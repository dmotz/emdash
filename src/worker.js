import * as tf from '@tensorflow/tfjs'
import {setWasmPaths, version_wasm as wasmVersion} from '@tensorflow/tfjs-backend-wasm'
import {load} from '@tensorflow-models/universal-sentence-encoder'
import {createStore, del, delMany, entries, keys, setMany} from 'idb-keyval'

const dbNs = 'emdash'
const embKey = 'embeddings'
const embSize = 512
const semanticSearchLimit = 203
const embsInProgress = {}
const embStore = createStore(`${dbNs}:${embKey}`, embKey)
const hasDb = new Promise(res => {
  const attempt = () => {
    const testNs = `${dbNs}:test`
    const dbReq = indexedDB.open(testNs)

    dbReq.onerror = () => {
      clearTimeout(timeout)
      res(false)
    }

    dbReq.onsuccess = () => {
      clearTimeout(timeout)
      res(true)
      indexedDB.deleteDatabase(testNs)
    }
  }

  let timeout = setTimeout(() => {
    attempt()
    timeout = setTimeout(() => res(false), 3000)
  }, 3000)

  attempt()
})

let excerptEmbMap = {}
let bookEmbMap = {}
let authorEmbMap = {}
let excerptIdToBookId = {}
let excerptTensor
let bookTensor
let authorTensor
let excerptKeyList
let bookKeyList
let authorKeyList
let model
let demoEmbedP

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
      const embs = ids.flatMap(id => excerptEmbMap[id] || [])

      if (embs.length) {
        map[collId] = embs
          .reduce((a, c) => a.map((n, i) => n + c[i]))
          .map(n => n / embs.length)
      }
    }
  })

const findExcerptNeighbors = async (targetId, k) => {
  const targetTitle = excerptIdToBookId[targetId]

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
      a.length === k
        ? a
        : excerptIdToBookId[c[0]] === targetTitle
        ? a
        : [...a, c],
    []
  )
}

const findBookNeighbors = (bookId, k) =>
  getTopK(bookTensor, bookKeyList, bookEmbMap[bookId], k, true)

const findAuthorNeighbors = async (authorId, k) =>
  (
    await getTopK(authorTensor, authorKeyList, authorEmbMap[authorId], k + 1)
  ).filter(([auth]) => auth !== authorId)

const processNewExcerpts = async ({excerpts}, cb) => {
  if ((await hasDb) && !Object.keys(excerptEmbMap).length) {
    const storedEmbs = await entries(embStore)

    excerptEmbMap = Object.fromEntries([
      ...storedEmbs,
      ...Object.entries(excerptEmbMap)
    ])
  }

  excerptIdToBookId = {
    ...excerptIdToBookId,
    ...Object.fromEntries(excerpts.map(({id, bookId}) => [id, bookId]))
  }

  cb(Object.keys(excerptEmbMap))
}

const updateCaches = () => {
  excerptTensor?.dispose()
  bookTensor?.dispose()
  excerptKeyList = Object.keys(excerptEmbMap)
  bookKeyList = Object.keys(bookEmbMap)
  excerptTensor = tf.tensor(Object.values(excerptEmbMap))
  bookTensor = tf.tensor(Object.values(bookEmbMap))
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

    computeEmbeddings(needed).then(async embeddings => {
      cb(embeddings.map(([id]) => id).concat(has))
      embeddings.forEach(([id, v]) => {
        excerptEmbMap[id] = v
        delete embsInProgress[id]
      })

      if (await hasDb) {
        setMany(embeddings, embStore)
      }
    })
  },

  computeBookEmbeddings: ({targets}, cb) => {
    computeAverages(targets, bookEmbMap)
    updateCaches()
    cb(null)
  },

  computeAuthorEmbeddings: ({targets}, cb) => {
    authorTensor?.dispose()
    computeAverages(targets, authorEmbMap)
    authorKeyList = Object.keys(authorEmbMap)
    authorTensor = tf.tensor(Object.values(authorEmbMap))
    cb(null)
  },

  requestExcerptNeighbors: async ({target, k}, cb) =>
    cb([target, await findExcerptNeighbors(target, k)]),

  requestBookNeighbors: async ({target, k}, cb) =>
    cb([target, bookEmbMap[target] ? await findBookNeighbors(target, k) : []]),

  requestAuthorNeighbors: async ({target, k}, cb) =>
    cb([
      target,
      authorEmbMap[target] ? await findAuthorNeighbors(target, k) : []
    ]),

  requestSemanticRank: async ({bookId, excerptIds}, cb) =>
    cb([
      bookId,
      excerptIds.length ? await semanticSort(bookId, excerptIds) : []
    ]),

  semanticSearch: ({query, threshold}, cb) =>
    semanticSearch(query, threshold).then(matches => cb([query, matches])),

  deleteExcerpt: async ({targetId, bookId, bookExcerptIds}) => {
    delete excerptEmbMap[targetId]

    if (await hasDb) {
      del(targetId, embStore)
    }

    computeAverages([[bookId, bookExcerptIds]], bookEmbMap)
    updateCaches()
  },

  deleteBook: async ({bookId, bookExcerptIds}) => {
    delete bookEmbMap[bookId]

    bookExcerptIds.forEach(id => {
      delete excerptEmbMap[id]
      delete excerptIdToBookId[id]
    })

    if (await hasDb) {
      delMany(bookExcerptIds, embStore)
    }

    updateCaches()
  },

  fetchDemoEmbeddings: () =>
    (demoEmbedP = fetch('/demo/embs').then(r => r.arrayBuffer())),

  setDemoEmbeddings: async ({ids}, cb) => {
    let buff

    try {
      buff = await demoEmbedP
    } catch (e) {
      console.log(e)
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

  initWithClear: async (state, cb) => {
    excerptEmbMap = {}
    bookEmbMap = {}
    authorEmbMap = {}
    excerptIdToBookId = {}
    excerptTensor?.dispose()
    bookTensor?.dispose()
    authorTensor?.dispose()
    processNewExcerpts(state, cb)

    if (await hasDb) {
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

const setWasm = () => {
  setWasmPaths(
    `https://cdn.jsdelivr.net/npm/@tensorflow/tfjs-backend-wasm@${wasmVersion}/dist/`
  )

  return tf.setBackend('wasm')
}

const start = port => {
  port.onmessage = ({data: {method, ...payload}}) =>
    methods[method](payload, data => port.postMessage({method, data}))

  console.log = msg => port.postMessage(msg)

  model = (
    'OffscreenCanvas' in self
      ? tf
          .setBackend('webgl')
          .then(() => (tf.ENV.flags.HAS_WEBGL ? Promise.resolve() : setWasm()))
      : setWasm()
  )
    .then(tf.ready)
    .then(() => console.log(`using ${tf.getBackend()} backend`))
    .then(load)
}

tf.enableProdMode()
self.onconnect = e => start(e.ports[0])
self.onerror = e => console.log(e)

if (!('SharedWorkerGlobalScope' in self)) {
  start(self)
}
