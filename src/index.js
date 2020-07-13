import {Elm} from './Main'
import {Store, get, set, keys} from 'idb-keyval'
import JsZip from 'jszip'
import './styles.sass'

const dbNs = 'marginalia'
const stateKey = 'state'
const embeddingsKey = 'embeddings'
const stateStore = new Store(`${dbNs}:${stateKey}`, stateKey)
const embeddingsStore = new Store(`${dbNs}:${embeddingsKey}`, embeddingsKey)
const writeMs = 1000

let embeddings = {}
let app
let writeTimer

init()

async function init() {
  console.log(`Marginalia v${require('../package.json').version}`)

  try {
    app = Elm.Main.init({flags: (await get(stateKey, stateStore)) || null})
  } catch (e) {
    console.warn('malformed restored state')
    app = Elm.Main.init({flags: null})
  }

  app.ports.importJson.subscribe(importJson)
  app.ports.exportJson.subscribe(exportJson)
  app.ports.setStorage.subscribe(setStorage)
  app.ports.createEpub.subscribe(createEpub)

  window.addEventListener('keydown', e => {
    if (e.key && e.key.toLowerCase() === 'a' && (e.metaKey || e.ctrlKey)) {
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
