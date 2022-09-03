const {sqrt} = Math
const neighborsK = 5

const dot = (a, b) => a.reduce((a, c, i) => a + c * b[i], 0)

const similarity = (a, b) => dot(a, b) / (sqrt(dot(a, a)) * sqrt(dot(b, b)))

self.addEventListener('message', ({data}) => {
  const {embeddingMap, targetId, titleMap, ignoreSameTitle} = data
  const target = embeddingMap[targetId]
  const predicate =
    titleMap && ignoreSameTitle
      ? k => titleMap[k] !== titleMap[targetId]
      : x => x

  self.postMessage({
    id: targetId,
    neighbors: Object.entries(embeddingMap)
      .flatMap(([k, v]) =>
        k === targetId || !predicate(k) ? [] : [[k, similarity(target, v)]]
      )
      .sort(([, a], [, b]) => b - a)
      .slice(0, neighborsK)
  })
})

export default () => {}
