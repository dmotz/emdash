import * as tf from '@tensorflow/tfjs'
import {load} from '@tensorflow-models/universal-sentence-encoder'
import {createStore, del, delMany, entries, keys, setMany} from 'idb-keyval'

const dbNs = 'marginalia'
const embKey = 'embeddings'
const model = load()
const embSize = 512
const neighborsK = 5
const semanticSearchLimit = 203
const embsInProgress = {}
const embStore = createStore(`${dbNs}:${embKey}`, embKey)

let excerptEmbMap = {}
let bookEmbMap = {}
let authorEmbMap = {}
let titleMap = {}
let excerptTensor
let bookTensor
let authorTensor
let excerptKeyList
let bookKeyList
let authorKeyList

const computeEmbeddings = async pairs => {
  const tensor = await (await model).embed(pairs.map(([, text]) => text))
  const embeddings = await tensor.data()

  setTimeout(() => tensor.dispose())

  return pairs.map(([id], i) => [
    id,
    embeddings.slice(i * embSize, (i + 1) * embSize)
  ])
}

const getTopK = async (tensor, ids, targetEmb, limit, dropFirst) => {
  const {values, indices} = tf.topk(
    tf.metrics.cosineProximity(targetEmb, tensor).neg(),
    Math.min(limit + +!!dropFirst, ids.length),
    true
  )

  const [scores, inds] = (
    await Promise.all([values.array(), indices.array()])
  ).map(dropFirst ? xs => xs.slice(1) : xs => xs)

  return inds.map((n, i) => [ids[n], scores[i]])
}

const semanticSearch = async (query, threshold) => {
  const tensor = await (await model).embed(query)
  const embedding = await tensor.data()

  setTimeout(() => tensor.dispose())

  return (
    await getTopK(excerptTensor, excerptKeyList, embedding, semanticSearchLimit)
  ).filter(([, v]) => v >= threshold)
}

const semanticSort = (bookId, exIds) =>
  getTopK(
    exIds.map(id => excerptEmbMap[id]),
    exIds,
    bookEmbMap[bookId],
    exIds.length
  )

const computeAverages = (targets, map) =>
  targets.forEach(([collId, ids]) => {
    if (ids.length) {
      map[collId] = ids
        .flatMap(id => excerptEmbMap[id] || [])
        .reduce((a, c) => a.map((n, i) => n + c[i]))
        .map(n => n / targets[0][1][0].length)
    }
  })

const findExcerptNeighbors = async targetId => {
  const targetTitle = titleMap[targetId]

  return (
    await getTopK(
      excerptTensor,
      excerptKeyList,
      excerptEmbMap[targetId],
      excerptKeyList.length,
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
  getTopK(bookTensor, bookKeyList, bookEmbMap[bookId], neighborsK, true)

const findAuthorNeighbors = async authorId =>
  (
    await getTopK(
      authorTensor,
      authorKeyList,
      authorEmbMap[authorId],
      neighborsK + 1
    )
  ).filter(([auth]) => auth !== authorId)

const processNewExcerpts = async ({books, excerpts}, cb) => {
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
}

const methods = {
  processNewExcerpts,

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
    excerptTensor?.dispose()
    bookTensor?.dispose()
    computeAverages(targets, bookEmbMap)
    excerptKeyList = Object.keys(excerptEmbMap)
    bookKeyList = Object.keys(bookEmbMap)
    excerptTensor = tf.tensor(Object.values(excerptEmbMap))
    bookTensor = tf.tensor(Object.values(bookEmbMap))
    cb(null)
  },

  computeAuthorEmbeddings: ({targets}, cb) => {
    authorTensor?.dispose()
    computeAverages(targets, authorEmbMap)
    authorKeyList = Object.keys(authorEmbMap)
    authorTensor = tf.tensor(Object.values(authorEmbMap))
    cb(null)
  },

  requestExcerptNeighbors: async ({target}, cb) =>
    cb([target, await findExcerptNeighbors(target)]),

  requestBookNeighbors: async ({target}, cb) =>
    cb([target, bookEmbMap[target] ? await findBookNeighbors(target) : []]),

  requestAuthorNeighbors: async ({target}, cb) =>
    cb([target, authorEmbMap[target] ? await findAuthorNeighbors(target) : []]),

  requestSemanticRank: async ({bookId, excerptIds}, cb) =>
    cb([
      bookId,
      excerptIds.length ? await semanticSort(bookId, excerptIds) : []
    ]),

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
  },

  initWithClear: (state, cb) => {
    excerptEmbMap = {}
    bookEmbMap = {}
    authorEmbMap = {}
    titleMap = {}
    excerptTensor?.dispose()
    bookTensor?.dispose()
    authorTensor?.dispose()
    processNewExcerpts(state, cb)

    if (embStore) {
      keys(embStore).then(ids => {
        const toKeep = state.excerpts.map(({id}) => id)

        delMany(
          ids.filter(k => !toKeep.includes(k)),
          embStore
        )
      })
    }
  }
}

self.onconnect = e => {
  const [port] = e.ports

  model.then(() => tf.setBackend('webgl'))

  port.onmessage = ({data: {method, ...payload}}) =>
    methods[method](payload, data => port.postMessage({method, data}))
}
