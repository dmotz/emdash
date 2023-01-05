import '@tensorflow/tfjs'
import {load} from '@tensorflow-models/universal-sentence-encoder'
import {createStore, del, entries, setMany} from 'idb-keyval'

const dbNs = 'marginalia'
const embKey = 'embeddings'
const model = load()
const embStep = 512
const neighborsK = 5
const semanticSearchLimit = 111
const bookEmbMap = {}
const embsInProgress = {}
const embStore = createStore(`${dbNs}:${embKey}`, embKey)

let excerptEmbMap = {}
let titleMap = {}

const dot = (a, b) => a.reduce((acc, c, i) => acc + c * b[i], 0)

const similarity = (a, b) =>
  dot(a, b) / (Math.sqrt(dot(a, a)) * Math.sqrt(dot(b, b)))

const computeEmbeddings = async pairs => {
  const tensor = await (await model).embed(pairs.map(([, text]) => text))
  const embeddings = await tensor.data()

  setTimeout(() => tensor.dispose())

  return pairs.map(([id], i) => [
    id,
    embeddings.slice(i * embStep, (i + 1) * embStep)
  ])
}

const semanticSearch = async (query, threshold) => {
  const tensor = await (await model).embed(query)
  const embedding = await tensor.data()

  setTimeout(() => tensor.dispose())

  return Object.entries(excerptEmbMap)
    .map(([k, v]) => [k, similarity(embedding, v)])
    .filter(([, v]) => v >= threshold)
    .sort(([, a], [, b]) => b - a)
    .slice(0, semanticSearchLimit)
}

const semanticSort = ids =>
  ids
    .map(id => [
      id,
      ids.reduce(
        (a, c) =>
          a + (c === id ? 0 : similarity(excerptEmbMap[id], excerptEmbMap[c])),
        0
      ) /
        (ids.length - 1)
    ])
    .sort(([, a], [, b]) => b - a)

const findNeighbors = (targetId, embMap, ignoreSameTitle) => {
  const target = embMap[targetId]
  const predicate = ignoreSameTitle
    ? k => titleMap[k] !== titleMap[targetId]
    : x => x

  return Object.entries(embMap)
    .flatMap(([k, v]) =>
      k === targetId || !predicate(k) ? [] : [[k, similarity(target, v)]]
    )
    .sort(([, a], [, b]) => b - a)
    .slice(0, neighborsK)
}

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
    const embeddings = targets.map(([bookId, ids]) => [
      bookId,
      ids.length
        ? ids
            .flatMap(id => excerptEmbMap[id] || [])
            .reduce((a, c) => a.map((n, i) => n + c[i]))
            .map(n => n / targets[0][1][0].length)
        : []
    ])

    cb(null)
    embeddings.forEach(([k, v]) => (bookEmbMap[k] = v))
  },

  requestExcerptNeighbors: ({target}, cb) =>
    cb([target, findNeighbors(target, excerptEmbMap, true)]),

  requestBookNeighbors: ({target}, cb) =>
    cb([target, findNeighbors(target, bookEmbMap)]),

  requestSemanticRank: ({bookId, entryIds}, cb) =>
    cb([bookId, semanticSort(entryIds)]),

  semanticSearch: ({query, threshold}, cb) =>
    semanticSearch(query, threshold).then(matches => cb([query, matches])),

  deleteEmbedding: ({target}) => {
    delete excerptEmbMap[target]

    if (embStore) {
      del(target, embStore)
    }
  }
}

self.onconnect = e => {
  const [port] = e.ports

  port.onmessage = ({data: {method, ...payload}}) =>
    methods[method](payload, data => port.postMessage({method, data}))
}
