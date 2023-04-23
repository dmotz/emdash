import JsZip from 'jszip'

self.onmessage = ({data}) => {
  const zip = new JsZip()
  zip.file('mimetype', 'application/epub+zip')
  zip.folder('META-INF')
  zip.folder('OEBPS')
  data.forEach(([path, text]) =>
    zip.file(
      path,
      text
        .trim()
        // eslint-disable-next-line
        .replaceAll(/[^\u0009\u000a\u000d\u0020-\uD7FF\uE000-\uFFFD]/g, '')
    )
  )

  zip
    .generateAsync({type: 'blob'})
    .then(zipData =>
      self.postMessage([
        `marginalia_excerpts_${new Date().toLocaleDateString()}.epub`,
        zipData
      ])
    )
}

self.postMessage(null)
