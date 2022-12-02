#!/usr/bin/env node
const main = async () => {
  const data = await require('node:fs/promises').readFile(
    './input.txt',
    'utf-8'
  )

  let elves = Id(data)
    .map(lines)
    .map(splitBy(eq('')))
    .map(
      mapWith(
        mapWith((x) => Number(x)),
        sum
      )
    ).value

  const step1 = maximum(elves)
  const step2 = sum(elves.sort(desc).slice(0, 3))

  console.log({ step1, step2 })
}

const Id = (value) => ({ map: (f) => Id(f(value)), value })

const eq = (a) => (b) => a === b
const sum = (a) => a.reduce((a, b) => a + b, 0)
const maximum = (a) => Math.max(...a)
const lines = (s) => s.split('\n')
const desc = (a, b) => b - a

const mapWith =
  (...fs) =>
  (a) =>
    fs.reduce((a, f) => a.map(f), a)

const findIndex =
  (f, start = 0) =>
  (arr) => {
    for (let i = start; i < arr.length; i++) {
      if (f(arr[i])) return i
    }

    return -1
  }

const splitBy = (f) => (arr) => {
  const result = []
  let start = 0
  let end
  while ((end = findIndex(f, start)(arr)) !== -1) {
    result.push(arr.slice(start, end))
    start = end + 1
  }

  result.push(arr.slice(start))
  return result
}

main().catch((e) => {
  console.error(e)
  process.exitCode = 1
})
