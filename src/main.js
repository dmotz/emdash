import {get, set, createStore} from 'idb-keyval'
import {Elm} from './Main.elm'
import {version} from '../package.json'
import './styles/main.sass'

const dbNs = 'marginalia'
const stateKey = 'state'
const writeMs = 999
const worker = new SharedWorker(new URL('./worker.js', import.meta.url), {
  name: 'marginalia',
  type: 'module'
})
const messageToPort = {
  processNewExcerpts: 'receiveExcerptEmbeddings',
  computeExcerptEmbeddings: 'receiveExcerptEmbeddings',
  computeBookEmbeddings: 'receiveBookEmbeddings',
  requestExcerptNeighbors: 'receiveExcerptNeighbors',
  requestBookNeighbors: 'receiveBookNeighbors',
  requestSemanticRank: 'receiveSemanticRank',
  semanticSearch: 'receiveSemanticSearch'
}

const downloadFile = (name, data) => {
  const a = document.createElement('a')
  const url = URL.createObjectURL(data)

  a.href = url
  a.download = name
  a.click()
  URL.revokeObjectURL(url)
}

let app
let writeTimer
let zipWorker

!(async () => {
  console.log(`Marginalia v${version} ⁓ habent sua fata libelli`)

  let restored = null
  let stateStore

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
    restored = await get(stateKey, stateStore)
  } catch (e) {
    console.warn('cannot open DB for writing')
  }

  try {
    app = Elm.Main.init({flags: [version, restored || null]})
  } catch (e) {
    console.warn('malformed restored state:', restored)
    app = Elm.Main.init({flags: [version, null]})
  }

  app.ports.handleNewEntries.subscribe(state =>
    worker.port.postMessage({
      method: 'processNewExcerpts',
      books: state.books,
      excerpts: state.entries
    })
  )

  app.ports.exportJson.subscribe(state =>
    downloadFile(
      `marginalia_backup_${new Date().toLocaleDateString()}.json`,
      new Blob([JSON.stringify(state)], {type: 'text/plain'})
    )
  )

  app.ports.setStorage.subscribe(state => {
    clearTimeout(writeTimer)
    if (stateStore) {
      writeTimer = setTimeout(
        () => set(stateKey, JSON.stringify(state), stateStore),
        writeMs
      )
    }
  })

  app.ports.createEpub.subscribe(pairs => {
    if (!zipWorker) {
      zipWorker = new Worker(new URL('./zip-worker.js', import.meta.url), {
        type: 'module'
      })
      zipWorker.onmessage = ({data}) => downloadFile(...data)
    }

    zipWorker.postMessage(pairs)
  })

  app.ports.requestExcerptEmbeddings.subscribe(targets =>
    worker.port.postMessage({method: 'computeExcerptEmbeddings', targets})
  )

  app.ports.requestBookEmbeddings.subscribe(targets =>
    worker.port.postMessage({method: 'computeBookEmbeddings', targets})
  )

  app.ports.deleteEmbedding.subscribe(target =>
    worker.port.postMessage({method: 'deleteEmbedding', target})
  )

  app.ports.requestExcerptNeighbors.subscribe(([target]) =>
    worker.port.postMessage({method: 'requestExcerptNeighbors', target})
  )

  app.ports.requestBookNeighbors.subscribe(target =>
    worker.port.postMessage({method: 'requestBookNeighbors', target})
  )

  app.ports.requestSemanticSearch.subscribe(([query, threshold]) =>
    worker.port.postMessage({method: 'semanticSearch', query, threshold})
  )

  app.ports.requestSemanticRank.subscribe(([bookId, entryIds]) =>
    worker.port.postMessage({method: 'requestSemanticRank', bookId, entryIds})
  )

  app.ports.scrollToTop.subscribe(() =>
    window.scrollTo({top: 0, left: 0, behavior: 'smooth'})
  )

  app.ports.requestUnicodeNormalized.subscribe(str =>
    app.ports.receiveUnicodeNormalized.send(str.normalize('NFKD'))
  )

  worker.port.start()
  worker.port.onmessage = ({data: {method, data}}) =>
    app.ports[messageToPort[method]].send(data)
})()
