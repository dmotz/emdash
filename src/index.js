require('./styles.sass')
const {Elm} = require('./Main')
const {document, localStorage, URL} = window
const lsNs = 'marginalia'
const state = window.localStorage.getItem(lsNs)
const debounceTime = 1000
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

app.ports.exportJson.subscribe(state => {
  const a = document.createElement('a')
  const url = URL.createObjectURL(
    new Blob([JSON.stringify(state)], {type: 'text/plain'})
  )
  a.href = url
  a.download = `marginalia_backup_${new Date().toLocaleDateString()}.json`
  a.click()
  URL.revokeObjectURL(url)
})

app.ports.importJson.subscribe(text => {
  try {
    app = Elm.Main.init({flags: JSON.parse(text)})
  } catch (e) {
    console.warn('Failed to parse restored JSON:', e)
  }
})

window.addEventListener('keydown', e => {
  if (e.key.toLowerCase() === 'a' && (e.metaKey || e.ctrlKey)) {
    e.preventDefault()
  }
})

if (process.env.NODE_ENV === 'production' && 'serviceWorker' in navigator) {
  window.addEventListener('load', () =>
    navigator.serviceWorker.register('sw.js')
  )
}
