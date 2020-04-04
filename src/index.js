import JsZip from 'jszip'

require('./styles.sass')

const {Elm} = require('./Main')
const {document, localStorage, URL} = window
const lsNs = 'marginalia'
const state = window.localStorage.getItem(lsNs)
const debounceTime = 1000

const downloadFile = (name, data) => {
  const a = document.createElement('a')
  const url = URL.createObjectURL(data)

  a.href = url
  a.download = name
  a.click()
  URL.revokeObjectURL(url)
}

let app
let lsTimer

try {
  app = Elm.Main.init({flags: state ? JSON.parse(state) : null})
} catch (e) {
  console.warn('Failed to handle persisted state:', e)
  app = Elm.Main.init({flags: null})
}

app.ports.setStorage.subscribe(state => {
  clearTimeout(lsTimer)
  lsTimer = setTimeout(
    () => localStorage.setItem(lsNs, JSON.stringify(state)),
    debounceTime
  )
})

app.ports.exportJson.subscribe(state =>
  downloadFile(
    `marginalia_backup_${new Date().toLocaleDateString()}.json`,
    new Blob([JSON.stringify(state)], {type: 'text/plain'})
  )
)

app.ports.importJson.subscribe(text => {
  try {
    app = Elm.Main.init({flags: JSON.parse(text)})
  } catch (e) {
    console.warn('Failed to parse restored JSON:', e)
  }
})

app.ports.createEpub.subscribe(async pairs => {
  const zip = new JsZip()

  zip.folder('META-INF')
  zip.folder('OEBPS')
  pairs.forEach(([path, text]) => zip.file(path, text.trim()))

  const data = await zip.generateAsync({type: 'blob'})
  const a = document.createElement('a')
  const url = URL.createObjectURL(data)

  a.href = url
  a.download = 'marginalia-excerpts.epub'
  a.click()
  URL.revokeObjectURL(url)
})

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
