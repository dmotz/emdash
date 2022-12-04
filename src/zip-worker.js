import JsZip from 'jszip'

self.onmessage = ({data}) => {
  const zip = new JsZip()
  zip.folder('META-INF')
  zip.folder('OEBPS')
  data.forEach(([path, text]) => zip.file(path, text.trim()))

  zip
    .generateAsync({type: 'blob'})
    .then(zipData =>
      self.postMessage([
        `marginalia_excerpts_${new Date().toLocaleDateString()}.epub`,
        zipData
      ])
    )
}
