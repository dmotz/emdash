import {get, getMany, set, setMany, del, keys, createStore} from 'idb-keyval'
import JsZip from 'jszip'
import {Elm} from './Main.elm'
import EmbedWorker from './embed-worker?worker'
import BookEmbedWorker from './book-embed-worker?worker'
import {version} from '../package.json'
import './styles.sass'

const dbNs = 'marginalia'
const stateKey = 'state'
const embeddingsKey = 'embeddings'
const stateStore = createStore(`${dbNs}:${stateKey}`, stateKey)
const embeddingsStore = createStore(`${dbNs}:${embeddingsKey}`, embeddingsKey)
const neighborsK = 5
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

let embeddings = {}
let bookEmbeddings = {}
let titleMap = {}
let workerBatch = 0
let lastScrollY = window.scrollY
let app
let writeTimer
let embedWorker
let bookEmbedWorker

init()

async function init() {
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

  app.ports.importJson.subscribe(importJson)
  app.ports.exportJson.subscribe(exportJson)
  app.ports.setStorage.subscribe(setStorage)
  app.ports.createEpub.subscribe(createEpub)
  app.ports.requestEmbeddings.subscribe(requestEmbeddings)
  app.ports.requestBookEmbeddings.subscribe(requestBookEmbeddings)
  app.ports.deleteEmbedding.subscribe(deleteEmbedding)
  app.ports.requestNeighbors.subscribe(requestNeighbors)
  app.ports.requestBookNeighbors.subscribe(requestBookNeighbors)
  app.ports.setObservers.subscribe(setObservers)
  app.ports.scrollToTop.subscribe(scrollToTop)
  app.ports.requestUnicodeNormalized.subscribe(requestUnicodeNormalized)

  if (restored && !didFail) {
    const ids = await keys(embeddingsStore)
    const vals = await getMany(ids, embeddingsStore)
    const bookIdToTitle = Object.fromEntries(
      restored.books.map(({id, title}) => [id, title])
    )

    embeddings = Object.fromEntries(ids.map((id, i) => [id, vals[i]]))

    titleMap = Object.fromEntries(
      restored.entries.map(({id, bookId}) => [id, bookIdToTitle[bookId]])
    )

    app.ports.receiveEmbeddings.send(ids)
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

function downloadFile(name, data) {
  const a = document.createElement('a')
  const url = URL.createObjectURL(data)

  a.href = url
  a.download = name
  a.click()
  URL.revokeObjectURL(url)
}

function setStorage(state) {
  clearTimeout(writeTimer)
  writeTimer = setTimeout(() => set(stateKey, state, stateStore), writeMs)
}

function importJson(text) {
  try {
    app = Elm.Main.init({flags: JSON.parse(text)})
  } catch (e) {
    console.warn('Failed to parse restored JSON:', e)
  }
}

function exportJson(state) {
  downloadFile(
    `marginalia_backup_${new Date().toLocaleDateString()}.json`,
    new Blob([JSON.stringify(state)], {type: 'text/plain'})
  )
}

async function createEpub(pairs) {
  const zip = new JsZip()

  zip.folder('META-INF')
  zip.folder('OEBPS')
  pairs.forEach(([path, text]) => zip.file(path, text.trim()))
  downloadFile(
    `marginalia_excerpts_${new Date().toLocaleDateString()}.epub`,
    await zip.generateAsync({type: 'blob'})
  )
}

function requestEmbeddings(pairs) {
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
}

function requestBookEmbeddings(sets) {
  if (!bookEmbedWorker) {
    bookEmbedWorker = new BookEmbedWorker()
    bookEmbedWorker.onmessage = ({data}) => {
      bookEmbeddings = Object.fromEntries(data.embeddingPairs)
      app.ports.receiveBookEmbeddings.send(null)
    }
  }

  bookEmbedWorker.postMessage({sets, embeddings})
}

function deleteEmbedding(id) {
  delete embeddings[id]
  del(id, embeddingsStore)
}

function rank(xs, id, predicate) {
  const target = xs[id]

  return Object.entries(xs)
    .reduce((a, [k, v]) => {
      if (k === id || !predicate(k)) {
        return a
      }

      a.push([k, similarity(target, v)])

      return a
    }, [])
    .sort(([, a], [, b]) => b - a)
    .slice(0, neighborsK)
}

function requestNeighbors([id, ignoreSameTitle]) {
  if (!embeddings[id]) {
    console.warn(`no embeddings yet for ${id}`)
    return
  }

  app.ports.receiveNeighbors.send([
    id,
    rank(
      embeddings,
      id,
      ignoreSameTitle ? k => titleMap[k] !== titleMap[id] : x => x
    )
  ])
}

function requestBookNeighbors(id) {
  app.ports.receiveBookNeighbors.send([id, rank(bookEmbeddings, id, x => x)])
}

function dot(a, b) {
  return a.reduce((a, c, i) => a + c * b[i], 0)
}

function similarity(a, b) {
  return dot(a, b) / (Math.sqrt(dot(a, a)) * Math.sqrt(dot(b, b)))
}

function setObservers(ids) {
  requestAnimationFrame(() =>
    ids.forEach(id => observer.observe(document.getElementById('entry' + id)))
  )
}

function scrollToTop() {
  window.scrollTo({top: 0, left: 0, behavior: 'smooth'})
}

function requestUnicodeNormalized(str) {
  app.ports.receiveUnicodeNormalized.send(str.normalize('NFKD'))
}
