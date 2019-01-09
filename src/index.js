require('./styles.sass')
const {Elm} = require('./Main')
const lsNs = 'ls'
const state = window.localStorage.getItem(lsNs)
const app = Elm.Main.init({flags: state ? JSON.parse(state) : null})
app.ports.setStorage.subscribe(state =>
  window.localStorage.setItem(lsNs, JSON.stringify(state))
)
