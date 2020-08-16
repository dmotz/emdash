import {Elm} from './Main'
import {Store, get, set, keys} from 'idb-keyval'
import JsZip from 'jszip'
import EmbedWorker from 'worker-loader!./embed-worker'
import './styles.sass'

const dbNs = 'marginalia'
const stateKey = 'state'
const embeddingsKey = 'embeddings'
const stateStore = new Store(`${dbNs}:${stateKey}`, stateKey)
const embeddingsStore = new Store(`${dbNs}:${embeddingsKey}`, embeddingsKey)
const neighborsK = 5
const writeMs = 1000

let embeddings = {}
let titleMap = {}
let app
let writeTimer

init()

async function init() {
  console.log(`Marginalia v${require('../package.json').version}`)

  const restored = await get(stateKey, stateStore)

  try {
    app = Elm.Main.init({flags: restored || null})
  } catch (e) {
    console.warn('malformed restored state', restored)
    app = Elm.Main.init({flags: null})
  }

  app.ports.importJson.subscribe(importJson)
  app.ports.exportJson.subscribe(exportJson)
  app.ports.setStorage.subscribe(setStorage)
  app.ports.createEpub.subscribe(createEpub)
  app.ports.calculateEmbeddings.subscribe(calculateEmbeddings)
  app.ports.requestNeighbors.subscribe(requestNeighbors)

  const ids = await keys(embeddingsStore)
  const vals = await Promise.all(ids.map(id => get(id, embeddingsStore)))

  embeddings = Object.fromEntries(ids.map((id, i) => [id, vals[i]]))

  if (restored) {
    titleMap = Object.fromEntries(restored.entries.map(e => [e.id, e.title]))
  }

  window.addEventListener('keydown', e => {
    if (
      e.key &&
      e.key.toLowerCase() === 'a' &&
      (e.metaKey || e.ctrlKey) &&
      !(
        document.activeElement.nodeName === 'INPUT' ||
        document.activeElement.nodeName === 'TEXTAREA'
      )
    ) {
      e.preventDefault()
    }
  })

  if (process.env.NODE_ENV === 'production' && 'serviceWorker' in navigator) {
    window.addEventListener('load', () =>
      navigator.serviceWorker.register('sw.js')
    )
  }
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

function calculateEmbeddings([ids, texts]) {
  const worker = new EmbedWorker()
  worker.postMessage([ids, texts])
  worker.onmessage = ({data}) => {
    Object.entries(data).forEach(([k, v]) => {
      embeddings[k] = v
      set(k, v, embeddingsStore)
    })
    worker.terminate()
  }
}

function requestNeighbors(id) {
  const target = embeddings[id]

  if (!target) {
    console.warn(`no embeddings yet for ${id}`)
    return
  }

  const ranked = Object.entries(embeddings)
    .reduce((a, [k, v]) => {
      if (k === id) {
        return a
      }

      a.push([k, similarity(target, v)])
      return a
    }, [])
    .filter(([k]) => titleMap[k] !== titleMap[id])
    .sort(([, a], [, b]) => b - a)
    .slice(0, neighborsK)

  app.ports.receiveNeighbors.send([id, ranked])
}

function dot(a, b) {
  return a.reduce((a, c, i) => a + c * b[i], 0)
}

function similarity(a, b) {
  return dot(a, b) / (Math.sqrt(dot(a, a)) * Math.sqrt(dot(b, b)))
}
