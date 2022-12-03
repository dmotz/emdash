import {get, set, createStore} from 'idb-keyval'
import {Elm} from './Main.elm'
import MarginaliaWorker from './worker?sharedworker'
import ZipWorker from './zip-worker?worker'
import {version} from '../package.json'
import './styles/main.sass'

console.log(`Marginalia v${version} ⁓ habent sua fata libelli`)

const dbNs = 'marginalia'
const stateKey = 'state'
const writeMs = 999
const worker = new MarginaliaWorker()
const stateStore = createStore(`${dbNs}:${stateKey}`, stateKey)
const messageToPort = {
  processNewExcerpts: 'receiveExcerptEmbeddings',
  computeExcerptEmbeddings: 'receiveExcerptEmbeddings',
  computeBookEmbeddings: 'receiveBookEmbeddings',
  requestExcerptNeighbors: 'receiveExcerptNeighbors',
  requestBookNeighbors: 'receiveBookNeighbors',
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
  const restored = await get(stateKey, stateStore)

  try {
    app = Elm.Main.init({flags: [restored, false]})
  } catch (e) {
    console.warn('malformed restored state:', restored)
    app = Elm.Main.init({flags: [null, false]})
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
      writeTimer = setTimeout(() => set(stateKey, state, stateStore), writeMs)
    }
  })

  app.ports.createEpub.subscribe(pairs => {
    if (!zipWorker) {
      zipWorker = new ZipWorker()
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
