require('./styles.sass')
const {Elm} = require('./Main')
const {document, localStorage, URL} = window
const lsNs = 'marginalia'
const state = window.localStorage.getItem(lsNs)
let app

try {
  app = Elm.Main.init({flags: state ? JSON.parse(state) : null})
} catch (e) {
  console.warn('Failed to handle persisted state:', e)
  app = Elm.Main.init({flags: null})
}

app.ports.setStorage.subscribe(state =>
  localStorage.setItem(lsNs, JSON.stringify(state))
)

app.ports.exportJson.subscribe(state => {
  const a = document.createElement('a')
  a.href = URL.createObjectURL(
    new Blob([JSON.stringify(state)], {type: 'text/plain'})
  )
  a.download = `marginalia_backup_${new Date().toLocaleDateString()}.json`
  a.click()
})

app.ports.importJson.subscribe(text => {
  try {
    app = Elm.Main.init({flags: JSON.parse(text)})
  } catch (e) {
    console.warn('Failed to parse restored JSON:', e)
  }
})

if (process.env.NODE_ENV === 'production' && 'serviceWorker' in navigator) {
  window.addEventListener('load', () =>
    navigator.serviceWorker.register('sw.js')
  )
}
