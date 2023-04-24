import {dirname} from 'path'
import {Buffer} from 'buffer'
import {fileURLToPath} from 'url'
import {createWriteStream, unlinkSync} from 'fs'
import {load} from '@tensorflow-models/universal-sentence-encoder'
import '@tensorflow/tfjs-node'
import demoJson from '../assets/demo/demo.json' assert {type: 'json'}

const batchSize = 20
const dir = dirname(fileURLToPath(import.meta.url))
const outputPath = dir + '/../assets/demo/embs'

unlinkSync(outputPath)

const file = createWriteStream(outputPath, {flags: 'a'})

const model = await load()
const {excerpts} = JSON.parse(demoJson)

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
