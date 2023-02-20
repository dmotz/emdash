import * as tf from '@tensorflow/tfjs'
import {load} from '@tensorflow-models/universal-sentence-encoder'
import {createStore, del, entries, setMany} from 'idb-keyval'

const dbNs = 'marginalia'
const embKey = 'embeddings'
const model = load()
const embSize = 512
const neighborsK = 5
const semanticSearchLimit = 203
const bookEmbMap = {}
const embsInProgress = {}
const embStore = createStore(`${dbNs}:${embKey}`, embKey)

let excerptEmbMap = {}
let titleMap = {}
let excerptsTensor
let booksTensor
let excerptsKeyList
let booksKeyList

const computeEmbeddings = async pairs => {
  const tensor = await (await model).embed(pairs.map(([, text]) => text))
  const embeddings = await tensor.data()

  setTimeout(() => tensor.dispose())

  return pairs.map(([id], i) => [
    id,
    embeddings.slice(i * embSize, (i + 1) * embSize)
  ])
}

const getTopK = async (tensor, keys, targetEmb, limit, dropFirst) => {
  const {values, indices} = tf.topk(
    tf.metrics.cosineProximity(targetEmb, tensor).neg(),
    Math.min(limit + +!!dropFirst, keys.length),
    true
  )

  const [scores, inds] = (
    await Promise.all([values.array(), indices.array()])
  ).map(dropFirst ? xs => xs.slice(1) : xs => xs)

  return inds.map((n, i) => [keys[n], scores[i]])
}

const semanticSearch = async (query, threshold) => {
  const tensor = await (await model).embed(query)
  const embedding = await tensor.data()

  setTimeout(() => tensor.dispose())

  return (
    await getTopK(
      excerptsTensor,
      excerptsKeyList,
      embedding,
      semanticSearchLimit
    )
  ).filter(([, v]) => v >= threshold)
}

const semanticSort = (bookId, exIds) =>
  getTopK(
    exIds.map(id => excerptEmbMap[id]),
    exIds,
    bookEmbMap[bookId],
    exIds.length
  )

const findExcerptNeighbors = async targetId => {
  const targetTitle = titleMap[targetId]

  return (
    await getTopK(
      excerptsTensor,
      excerptsKeyList,
      excerptEmbMap[targetId],
      excerptsKeyList.length,
      true
    )
  ).reduce(
    (a, c) =>
      a.length === neighborsK
        ? a
        : titleMap[c[0]] === targetTitle
        ? a
        : [...a, c],
    []
  )
}

const findBookNeighbors = bookId =>
  getTopK(booksTensor, booksKeyList, bookEmbMap[bookId], neighborsK, true)

const methods = {
  processNewExcerpts: async ({books, excerpts}, cb) => {
    const bookIdToTitle = Object.fromEntries(
      books.map(({id, title}) => [id, title])
    )

    if (embStore && !Object.keys(excerptEmbMap).length) {
      excerptEmbMap = Object.fromEntries(await entries(embStore))
    }

    titleMap = {
      ...titleMap,
      ...Object.fromEntries(
        excerpts.map(({id, bookId}) => [id, bookIdToTitle[bookId]])
      )
    }

    cb(Object.keys(excerptEmbMap))
  },

  computeExcerptEmbeddings: ({targets}, cb) => {
    const [has, needed] = targets.reduce(
      ([has, needed], [id, text]) =>
        excerptEmbMap[id]
          ? [[...has, id], needed]
          : [has, [...needed, [id, text]]],
      [[], []]
    )

    if (!needed.filter(([id]) => !embsInProgress[id]).length) {
      return cb(has)
    }

    needed.forEach(([id]) => (embsInProgress[id] = true))

    computeEmbeddings(needed).then(embeddings => {
      cb(embeddings.map(([id]) => id).concat(has))
      embeddings.forEach(([id, v]) => {
        excerptEmbMap[id] = v
        delete embsInProgress[id]
      })

      if (embStore) {
        setMany(embeddings, embStore)
      }
    })
  },

  computeBookEmbeddings: ({targets}, cb) => {
    targets.forEach(
      ([bookId, ids]) =>
        (bookEmbMap[bookId] = ids.length
          ? ids
              .flatMap(id => excerptEmbMap[id] || [])
              .reduce((a, c) => a.map((n, i) => n + c[i]))
              .map(n => n / targets[0][1][0].length)
          : [])
    )

    cb(null)
  },

  requestExcerptNeighbors: async ({target}, cb) =>
    cb([target, await findExcerptNeighbors(target)]),

  requestBookNeighbors: async ({target}, cb) =>
    cb([target, bookEmbMap[target] ? await findBookNeighbors(target) : []]),

  requestSemanticRank: async ({bookId, excerptIds}, cb) =>
    cb([bookId, await semanticSort(bookId, excerptIds)]),

  semanticSearch: ({query, threshold}, cb) =>
    semanticSearch(query, threshold).then(matches => cb([query, matches])),

  deleteEmbedding: ({target}) => {
    delete excerptEmbMap[target]

    if (embStore) {
      del(target, embStore)
    }
  },

  setDemoEmbeddings: async ({ids}, cb) => {
    let buff

    try {
      buff = await (await fetch('/demo/embs')).arrayBuffer()
    } catch (e) {
      return
    }

    const vecSize = embSize * 4

    excerptEmbMap = Object.fromEntries(
      new Array(buff.byteLength / vecSize)
        .fill()
        .map((_, i) => [ids[i], new Float32Array(buff, i * vecSize, embSize)])
    )

    cb(ids)
  }
}

self.onconnect = e => {
  const [port] = e.ports

  port.onmessage = ({data: {method, ...payload}}) =>
    methods[method](payload, data => port.postMessage({method, data}))
}
