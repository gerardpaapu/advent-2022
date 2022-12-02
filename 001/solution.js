#!/usr/bin/env node
const main = async () => {
  const data = await require('node:fs/promises').readFile(
    './input.txt',
    'utf-8'
  )

  let elves = Id(data)
    .map(splitOn('\n'))
    .map(splitOn(''))
    .unwrap()
    .map(mapWith(toNumber))
    .map(sum)

  const step1 = maximum(elves)
  const top3 = elves.sort(desc).slice(0, 3)
  const step2 = sum(top3)

  console.log({ step1, step2 })
}

const Id = (value) => ({ map: (f) => Id(f(value)), unwrap: () => value })
const sum = (a) => a.reduce((a, b) => a + b, 0)
const maximum = (a) => Math.max(...a)
const desc = (a, b) => b - a
const toNumber = (n) => Number(n)

const mapWith =
  (...fs) =>
  (a) =>
    fs.reduce((a, f) => a.map(f), a)

const splitOn = (c) => (arr) => {
  const result = []
  let start = 0
  let end
  while ((end = arr.indexOf(c, start)) !== -1) {
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
