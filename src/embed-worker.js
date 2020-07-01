import * as use from '@tensorflow-models/universal-sentence-encoder'

const step = 512
const model = use.load()

self.addEventListener('message', async ({data}) => {
  const [ids, excerpts] = data
  const embeddings = (await (await model).embed(excerpts)).dataSync()

  self.postMessage(
    Object.fromEntries(
      ids.map((id, i) => [id, embeddings.slice(i * step, (i + 1) * step)])
    )
  )
})

export default () => {}
