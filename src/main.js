import {get, set, createStore} from 'idb-keyval'
import SharedWorker from '@okikio/sharedworker'
import {Elm} from './Main.elm'
import {version} from '../package.json'
import './styles/main.sass'

const dbNs = 'marginalia'
const stateKey = 'state'
const writeMs = 333
const bcKey = 'BroadcastChannel'
const supportsBroadcastChannel = bcKey in window

const worker = new SharedWorker(new URL('./worker.js', import.meta.url), {
  name: 'marginalia',
  type: 'module'
})

const channel = supportsBroadcastChannel && new BroadcastChannel(dbNs)

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

const msgWorker = (method, payload) =>
  worker && worker.port.postMessage({method, ...payload})

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
  console.log(`${dbNs} v${version} ⁓ habent sua fata libelli`)

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
    [!supportsBroadcastChannel && bcKey].filter(Boolean),
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

  if (channel) {
    channel.onmessage = ({data}) => app.ports.syncState.send(data)
  }

  app.ports.initWithClear.subscribe(state =>
    msgWorker('initWithClear', {books: state.books, excerpts: state.excerpts})
  )

  app.ports.handleNewExcerpts.subscribe(state =>
    msgWorker('processNewExcerpts', {excerpts: state.excerpts})
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
    msgWorker('computeExcerptEmbeddings', {targets})
  )

  app.ports.requestBookEmbeddings.subscribe(targets =>
    msgWorker('computeBookEmbeddings', {targets})
  )

  app.ports.requestAuthorEmbeddings.subscribe(targets =>
    msgWorker('computeAuthorEmbeddings', {targets})
  )

  app.ports.deleteExcerpt.subscribe(
    ([targetId, [bookId, bookExcerptIds], k]) => {
      msgWorker('deleteExcerpt', {targetId, bookId, bookExcerptIds})
      msgWorker('requestBookNeighbors', {target: bookId, k})
    }
  )

  app.ports.deleteBook.subscribe(([bookId, bookExcerptIds]) =>
    msgWorker('deleteBook', {bookId, bookExcerptIds})
  )

  app.ports.requestExcerptNeighbors.subscribe(([target, k]) =>
    msgWorker('requestExcerptNeighbors', {target, k})
  )

  app.ports.requestAuthorNeighbors.subscribe(([target, k]) =>
    msgWorker('requestAuthorNeighbors', {target, k})
  )

  app.ports.requestBookNeighbors.subscribe(([target, k]) =>
    msgWorker('requestBookNeighbors', {target, k})
  )

  app.ports.requestSemanticSearch.subscribe(([query, threshold]) =>
    msgWorker('semanticSearch', {query, threshold})
  )

  app.ports.requestSemanticRank.subscribe(([bookId, excerptIds]) =>
    msgWorker('requestSemanticRank', {bookId, excerptIds})
  )

  app.ports.scrollToTop.subscribe(() =>
    window.scrollTo({top: 0, left: 0, behavior: 'smooth'})
  )

  app.ports.requestUnicodeNormalized.subscribe(str =>
    app.ports.receiveUnicodeNormalized.send(str.normalize('NFC'))
  )

  app.ports.fetchDemoEmbeddings.subscribe(ids =>
    msgWorker('setDemoEmbeddings', {ids})
  )

  worker.port.start()
  worker.port.onmessage = ({data: {method, data}}) => {
    app.ports[messageToPort[method]].send(data)
  }
})()

window.addEventListener(
  'input',
  ({target}) => {
    if (target.type !== 'text') {
      return
    }

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
