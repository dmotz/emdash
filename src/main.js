import {get, getMany, set, setMany, del, keys, createStore} from 'idb-keyval'
import JsZip from 'jszip'
import {Elm} from './Main.elm'
import EmbedWorker from './embed-worker?worker'
import BookEmbedWorker from './book-embed-worker?worker'
import NeighborWorker from './neighbor-worker?worker'
import {version} from '../package.json'
import './styles.sass'

const dbNs = 'marginalia'
const stateKey = 'state'
const embeddingsKey = 'embeddings'
const stateStore = createStore(`${dbNs}:${stateKey}`, stateKey)
const embeddingsStore = createStore(`${dbNs}:${embeddingsKey}`, embeddingsKey)
const writeMs = 1000
const batchIds = {}
const observer = new IntersectionObserver(
  obs => {
    const visible = obs.find(ob => ob.intersectionRatio > 0)

    if (visible) {
      app.ports.onIntersect.send(visible.target.id.replace(/^entry/, ''))
    }
  },
  {rootMargin: '-20% 0% -60% 0%'}
)

const downloadFile = (name, data) => {
  const a = document.createElement('a')
  const url = URL.createObjectURL(data)

  a.href = url
  a.download = name
  a.click()
  URL.revokeObjectURL(url)
}

const handleNewEntries = async state => {
  const ids = await keys(embeddingsStore)
  const vals = await getMany(ids, embeddingsStore)
  const bookIdToTitle = Object.fromEntries(
    state.books.map(({id, title}) => [id, title])
  )

  embeddings = Object.fromEntries(ids.map((id, i) => [id, vals[i]]))

  titleMap = {
    ...titleMap,
    ...Object.fromEntries(
      state.entries.map(({id, bookId}) => [id, bookIdToTitle[bookId]])
    )
  }

  app.ports.receiveEmbeddings.send(ids)
}

const init = async () => {
  const restored = await get(stateKey, stateStore)
  let didFail = false

  console.log(`Marginalia v${version}`)

  try {
    app = Elm.Main.init({flags: restored || null})
  } catch (e) {
    console.warn('malformed restored state', restored)
    app = Elm.Main.init({flags: null})
    didFail = true
  }

  app.ports.importJson.subscribe(text => {
    try {
      app = Elm.Main.init({flags: JSON.parse(text)})
    } catch (e) {
      console.warn('failed to parse restored JSON:', e)
    }
  })

  app.ports.exportJson.subscribe(state =>
    downloadFile(
      `marginalia_backup_${new Date().toLocaleDateString()}.json`,
      new Blob([JSON.stringify(state)], {type: 'text/plain'})
    )
  )

  app.ports.setStorage.subscribe(state => {
    clearTimeout(writeTimer)
    writeTimer = setTimeout(() => set(stateKey, state, stateStore), writeMs)
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
        setMany(data.targets, embeddingsStore)
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
    del(id, embeddingsStore)
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

  app.ports.setObservers.subscribe(ids =>
    requestAnimationFrame(() =>
      ids.forEach(id => observer.observe(document.getElementById('entry' + id)))
    )
  )

  app.ports.scrollToTop.subscribe(() =>
    window.scrollTo({top: 0, left: 0, behavior: 'smooth'})
  )

  app.ports.requestUnicodeNormalized.subscribe(str =>
    app.ports.receiveUnicodeNormalized.send(str.normalize('NFKD'))
  )

  if (restored && !didFail) {
    handleNewEntries(restored)
  }

  window.addEventListener(
    'scroll',
    () => {
      app.ports.onScroll.send(window.scrollY - lastScrollY)
      lastScrollY = window.scrollY
    },
    {passive: true}
  )
}

let embeddings = {}
let bookEmbeddings = {}
let titleMap = {}
let workerBatch = 0
let lastScrollY = window.scrollY
let app
let writeTimer
let embedWorker
let bookEmbedWorker
let neighborWorker
let bookNeighborWorker

init()
