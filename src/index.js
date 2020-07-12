import {Elm} from './Main'
import JsZip from 'jszip'
import './styles.sass'

const {document, localStorage, URL} = window
const lsNs = 'marginalia'
const debounceTime = 1000

let app
let lsTimer

init()

function init() {
  const restored = localStorage.getItem(lsNs)

  try {
    app = Elm.Main.init({flags: restored ? JSON.parse(restored) : null})
  } catch (e) {
    console.warn('Failed to handle persisted state:', e)
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
  clearTimeout(lsTimer)
  lsTimer = setTimeout(
    () => localStorage.setItem(lsNs, JSON.stringify(state)),
    debounceTime
  )
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
