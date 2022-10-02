const {sqrt} = Math

const dot = (a, b) => a.reduce((a, c, i) => a + c * b[i], 0)

export const similarity = (a, b) =>
  dot(a, b) / (sqrt(dot(a, a)) * sqrt(dot(b, b)))
