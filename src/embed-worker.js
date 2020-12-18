import '@tensorflow/tfjs'
import * as use from '@tensorflow-models/universal-sentence-encoder'

const step = 512
const model = use.load()

self.addEventListener('message', async ({data}) => {
  const embeddings = (
    await (await model).embed(data.map(([, text]) => text))
  ).dataSync()

  self.postMessage(
    data.map(([id], i) => [id, embeddings.slice(i * step, (i + 1) * step)])
  )
})

export default () => {}
