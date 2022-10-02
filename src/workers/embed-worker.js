import '@tensorflow/tfjs'
import {load} from '@tensorflow-models/universal-sentence-encoder'

const step = 512
const model = load()

self.addEventListener('message', async ({data}) => {
  const tensor = await (await model).embed(data.targets.map(([, text]) => text))
  const embeddings = await tensor.data()

  self.postMessage({
    targets: data.targets.map(([id], i) => [
      id,
      embeddings.slice(i * step, (i + 1) * step)
    ]),
    batchId: data.batchId
  })
  tensor.dispose()
})
