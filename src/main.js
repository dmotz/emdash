import {get, getMany, set, setMany, del, keys, createStore} from 'idb-keyval'
import JsZip from 'jszip'
import {Elm} from './Main.elm'
import EmbedWorker from './workers/embed-worker?worker'
import BookEmbedWorker from './workers/book-embed-worker?worker'
import NeighborWorker from './workers/neighbor-worker?worker'
import SemanticSearchWorker from './workers/semantic-search-worker?worker'
import {version} from '../package.json'
import './styles/main.sass'

const downloadFile = (name, data) => {
  const a = document.createElement('a')
  const url = URL.createObjectURL(data)

  a.href = url
  a.download = name
  a.click()
  URL.revokeObjectURL(url)
}

const handleNewEntries = async state => {
  const bookIdToTitle = Object.fromEntries(
    state.books.map(({id, title}) => [id, title])
  )
  let ids = []

  if (embeddingsStore) {
    ids = await keys(embeddingsStore)
    const vals = await getMany(ids, embeddingsStore)
    embeddings = Object.fromEntries(ids.map((id, i) => [id, vals[i]]))
  }

  titleMap = {
    ...titleMap,
    ...Object.fromEntries(
      state.entries.map(({id, bookId}) => [id, bookIdToTitle[bookId]])
    )
  }

  app.ports.receiveEmbeddings.send(ids)
}

const dbNs = 'marginalia'
const stateKey = 'state'
const embeddingsKey = 'embeddings'
const writeMs = 999
const batchIds = {}

let embeddings = {}
let bookEmbeddings = {}
let titleMap = {}
let workerBatch = 0
let app
let restored
let stateStore
let embeddingsStore
let writeTimer
let embedWorker
let bookEmbedWorker
let neighborWorker
let bookNeighborWorker
let semanticSearchWorker

!(async () => {
  console.log(`Marginalia v${version}`)

  try {
    await new Promise((res, rej) => {
      const testNs = `${dbNs}:test`
      const dbReq = indexedDB.open(testNs)
      dbReq.onerror = rej

      dbReq.onsuccess = () => {
        res()
        indexedDB.deleteDatabase(testNs)
      }
    })

    stateStore = createStore(`${dbNs}:${stateKey}`, stateKey)
    embeddingsStore = createStore(`${dbNs}:${embeddingsKey}`, embeddingsKey)
    restored = await get(stateKey, stateStore)
  } catch (e) {
    console.warn('cannot open DB for writing')
  }

  try {
    app = Elm.Main.init({flags: [restored, false]})
  } catch (e) {
    console.warn('malformed restored state:', restored)
    app = Elm.Main.init({flags: [null, false]})
  }

  app.ports.handleNewEntries.subscribe(handleNewEntries)

  app.ports.exportJson.subscribe(state =>
    downloadFile(
      `marginalia_backup_${new Date().toLocaleDateString()}.json`,
      new Blob([JSON.stringify(state)], {type: 'text/plain'})
    )
  )

  app.ports.setStorage.subscribe(state => {
    clearTimeout(writeTimer)
    if (stateStore) {
      writeTimer = setTimeout(() => set(stateKey, state, stateStore), writeMs)
    }
  })

  app.ports.createEpub.subscribe(async pairs => {
    const zip = new JsZip()

    zip.folder('META-INF')
    zip.folder('OEBPS')
    pairs.forEach(([path, text]) => zip.file(path, text.trim()))
    downloadFile(
      `marginalia_excerpts_${new Date().toLocaleDateString()}.epub`,
      await zip.generateAsync({type: 'blob'})
    )
  })

  app.ports.requestEmbeddings.subscribe(pairs => {
    if (!embedWorker) {
      embedWorker = new EmbedWorker()
      embedWorker.onmessage = ({data}) => {
        app.ports.receiveEmbeddings.send(batchIds[data.batchId])
        data.targets.forEach(([k, v]) => (embeddings[k] = v))
        delete batchIds[data.batchId]

        if (embeddingsStore) {
          setMany(data.targets, embeddingsStore)
        }
      }
    }

    const batchId = workerBatch++
    const targets = pairs.filter(([id]) => !embeddings[id])
    const ids = pairs.map(([id]) => id)

    if (!targets.length) {
      app.ports.receiveEmbeddings.send(ids)
      return
    }

    batchIds[batchId] = ids
    embedWorker.postMessage({targets, batchId})
  })

  app.ports.requestBookEmbeddings.subscribe(sets => {
    if (!bookEmbedWorker) {
      bookEmbedWorker = new BookEmbedWorker()
      bookEmbedWorker.onmessage = ({data}) => {
        bookEmbeddings = Object.fromEntries(data.embeddingPairs)
        app.ports.receiveBookEmbeddings.send(null)
      }
    }

    bookEmbedWorker.postMessage({sets, embeddings})
  })

  app.ports.deleteEmbedding.subscribe(id => {
    delete embeddings[id]
    if (embeddingsStore) {
      del(id, embeddingsStore)
    }
  })

  app.ports.requestNeighbors.subscribe(([targetId, ignoreSameTitle]) => {
    if (!embeddings[targetId]) {
      console.warn(`no embeddings yet for ${targetId}`)
      return
    }

    if (!neighborWorker) {
      neighborWorker = new NeighborWorker()
      neighborWorker.onmessage = ({data}) =>
        app.ports.receiveNeighbors.send([data.id, data.neighbors])
    }

    neighborWorker.postMessage({
      targetId,
      titleMap,
      ignoreSameTitle,
      embeddingMap: embeddings
    })
  })

  app.ports.requestBookNeighbors.subscribe(targetId => {
    if (!bookNeighborWorker) {
      bookNeighborWorker = new NeighborWorker()
      bookNeighborWorker.onmessage = ({data}) =>
        app.ports.receiveBookNeighbors.send([data.id, data.neighbors])
    }

    bookNeighborWorker.postMessage({targetId, embeddingMap: bookEmbeddings})
  })

  app.ports.requestSemanticSearch.subscribe(([query, threshold]) => {
    if (!semanticSearchWorker) {
      semanticSearchWorker = new SemanticSearchWorker()
      semanticSearchWorker.onmessage = ({data}) =>
        app.ports.receiveSemanticSearch.send([data.query, data.matches])
    }

    semanticSearchWorker.postMessage({
      query,
      threshold,
      embeddingMap: embeddings
    })
  })

  app.ports.scrollToTop.subscribe(() =>
    window.scrollTo({top: 0, left: 0, behavior: 'smooth'})
  )

  app.ports.requestUnicodeNormalized.subscribe(str =>
    app.ports.receiveUnicodeNormalized.send(str.normalize('NFKD'))
  )
})()
