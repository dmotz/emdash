import '@tensorflow/tfjs'
import {load} from '@tensorflow-models/universal-sentence-encoder'

const {sqrt} = Math
const neighborsK = 20
const threshold = 0.3
const model = load()

const dot = (a, b) => a.reduce((a, c, i) => a + c * b[i], 0)

const similarity = (a, b) => dot(a, b) / (sqrt(dot(a, a)) * sqrt(dot(b, b)))

self.addEventListener('message', async ({data}) => {
  const {query, embeddingMap} = data
  const tensor = await (await model).embed(query)
  const embedding = await tensor.data()

  self.postMessage({
    query,
    matches: Object.entries(embeddingMap)
      .map(([k, v]) => [k, similarity(embedding, v)])
      .filter(([, v]) => v >= threshold)
      .sort(([, a], [, b]) => b - a)
      .slice(0, neighborsK)
  })
  tensor.dispose()
})

export default () => {}
