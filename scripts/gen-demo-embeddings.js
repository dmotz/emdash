/* globals Buffer */

const {readFileSync, unlinkSync, createWriteStream} = require('fs')
const {load} = require('@tensorflow-models/universal-sentence-encoder')
require('@tensorflow/tfjs-node')

const batchSize = 20
const outputPath = __dirname + '/../assets/demo/embs'

unlinkSync(outputPath)

const file = createWriteStream(outputPath, {flags: 'a'})

!(async () => {
  const model = await load()
  const {excerpts} = JSON.parse(
    readFileSync(__dirname + '/../assets/demo/demo.json', 'utf8')
  )

  let i = 0

  while (i < excerpts.length) {
    const start = i
    const end = i + batchSize

    i += batchSize

    console.log(`writing ${start} - ${end}`)

    file.write(
      Buffer.from(
        (
          await model.embed(excerpts.slice(start, end).map(({text}) => text))
        ).dataSync().buffer
      )
    )
  }

  console.log(`\n\ntotal: ${excerpts.length}`)
})()
