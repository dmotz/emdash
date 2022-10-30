import '@tensorflow/tfjs'
import {load} from '@tensorflow-models/universal-sentence-encoder'
import {similarity} from './utils'

const neighborsK = 20
const model = load()

self.addEventListener('message', async ({data}) => {
  const {query, embeddingMap, threshold} = data
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

export default {}
