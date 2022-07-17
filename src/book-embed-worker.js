self.addEventListener('message', ({data}) =>
  self.postMessage({
    embeddingPairs: data.sets.map(([bookId, ids]) => [
      bookId,
      ids
        .map(id => data.embeddings[id])
        .filter(x => x)
        .reduce((a, c) => a.map((n, i) => n + c[i]))
        .map(n => n / data.sets[0][1][0].length)
    ])
  })
)

export default () => {}
