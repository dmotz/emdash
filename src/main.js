import {get, set, createStore} from 'idb-keyval'
import {Elm} from './Main.elm'
import {version} from '../package.json'
import './styles/main.sass'

const dbNs = 'marginalia'
const stateKey = 'state'
const writeMs = 333
const worker = new SharedWorker(new URL('./worker.js', import.meta.url), {
  name: 'marginalia',
  type: 'module'
})
const channel = new BroadcastChannel(dbNs)
const messageToPort = {
  processNewExcerpts: 'receiveExcerptEmbeddings',
  initWithClear: 'receiveExcerptEmbeddings',
  computeExcerptEmbeddings: 'receiveExcerptEmbeddings',
  computeBookEmbeddings: 'receiveBookEmbeddings',
  computeAuthorEmbeddings: 'receiveAuthorEmbeddings',
  requestExcerptNeighbors: 'receiveExcerptNeighbors',
  requestBookNeighbors: 'receiveBookNeighbors',
  requestAuthorNeighbors: 'receiveAuthorNeighbors',
  requestSemanticRank: 'receiveSemanticRank',
  semanticSearch: 'receiveSemanticSearch',
  setDemoEmbeddings: 'receiveExcerptEmbeddings'
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

  const flags = [
    restored || null,
    [
      version,
      import.meta.env.VITE_MAILING_LIST_URL || '',
      import.meta.env.VITE_MAILING_LIST_FIELD || ''
    ]
  ]

  try {
    app = Elm.Main.init({flags})
  } catch (e) {
    console.warn('malformed restored state:', restored)
    app = Elm.Main.init({flags: [null, ...flags.slice(1)]})
  }

  channel.onmessage = ({data}) => app.ports.syncState.send(data)

  app.ports.initWithClear.subscribe(state =>
    worker.port.postMessage({
      method: 'initWithClear',
      books: state.books,
      excerpts: state.excerpts
    })
  )

  app.ports.handleNewExcerpts.subscribe(state =>
    worker.port.postMessage({
      method: 'processNewExcerpts',
      excerpts: state.excerpts
    })
  )

  app.ports.exportJson.subscribe(state =>
    downloadFile(
      `${dbNs}_backup_${new Date().toLocaleDateString()}.json`,
      new Blob([JSON.stringify(state)], {type: 'text/plain'})
    )
  )

  app.ports.setStorage.subscribe(state => {
    clearTimeout(writeTimer)
    if (stateStore) {
      writeTimer = setTimeout(() => {
        set(stateKey, JSON.stringify(state), stateStore)
        channel.postMessage(state)
      }, writeMs)
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

  app.ports.requestAuthorEmbeddings.subscribe(targets =>
    worker.port.postMessage({method: 'computeAuthorEmbeddings', targets})
  )

  app.ports.deleteExcerpt.subscribe(([targetId, [bookId, bookExcerptIds]]) => {
    worker.port.postMessage({
      method: 'deleteExcerpt',
      targetId,
      bookId,
      bookExcerptIds
    })

    worker.port.postMessage({method: 'requestBookNeighbors', target: bookId})
  })

  app.ports.deleteBook.subscribe(([bookId, bookExcerptIds]) =>
    worker.port.postMessage({method: 'deleteBook', bookId, bookExcerptIds})
  )

  app.ports.requestExcerptNeighbors.subscribe(([target]) =>
    worker.port.postMessage({method: 'requestExcerptNeighbors', target})
  )

  app.ports.requestAuthorNeighbors.subscribe(target =>
    worker.port.postMessage({method: 'requestAuthorNeighbors', target})
  )

  app.ports.requestBookNeighbors.subscribe(target =>
    worker.port.postMessage({method: 'requestBookNeighbors', target})
  )

  app.ports.requestSemanticSearch.subscribe(([query, threshold]) =>
    worker.port.postMessage({method: 'semanticSearch', query, threshold})
  )

  app.ports.requestSemanticRank.subscribe(([bookId, excerptIds]) =>
    worker.port.postMessage({method: 'requestSemanticRank', bookId, excerptIds})
  )

  app.ports.scrollToTop.subscribe(() =>
    window.scrollTo({top: 0, left: 0, behavior: 'smooth'})
  )

  app.ports.requestUnicodeNormalized.subscribe(str =>
    app.ports.receiveUnicodeNormalized.send(str.normalize('NFC'))
  )

  app.ports.fetchDemoEmbeddings.subscribe(ids =>
    worker.port.postMessage({method: 'setDemoEmbeddings', ids})
  )

  worker.port.start()
  worker.port.onmessage = ({data: {method, data}}) =>
    app.ports[messageToPort[method]].send(data)
})()

window.addEventListener(
  'input',
  ({target}) => {
    const {value, selectionStart} = target

    requestAnimationFrame(() => {
      if (target.value !== value) {
        target.selectionStart = target.selectionEnd =
          selectionStart - (value.length - target.value.length)
      }
    })
  },
  true
)
