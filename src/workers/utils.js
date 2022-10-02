const dot = (a, b) => a.reduce((a, c, i) => a + c * b[i], 0)

export const similarity = (a, b) =>
  dot(a, b) / (Math.sqrt(dot(a, a)) * Math.sqrt(dot(b, b)))
