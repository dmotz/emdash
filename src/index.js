require('./styles.sass')
const {Elm} = require('./Main')
const lsNs = 'ls'
const state = window.localStorage.getItem(lsNs)
let app

try {
  app = Elm.Main.init({flags: state ? JSON.parse(state) : null})
} catch (e) {
  console.warn('Failed to handle persisted state:', e)
  app = Elm.Main.init({flags: null})
}

app.ports.setStorage.subscribe(state =>
  window.localStorage.setItem(lsNs, JSON.stringify(state))
)
