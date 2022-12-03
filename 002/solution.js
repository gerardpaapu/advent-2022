#!/usr/bin/env node

const main = async () => {
  const txt = await require('node:fs/promises').readFile('./input.txt', 'utf-8')
  const part1 = Id(txt)
    .map((s) => s.split('\n'))
    .map(mapWith(parseLine1, scoreLine_))
    .map(sum)
    .unwrap()

  const part2 = Id(txt)
    .map((s) => s.split('\n'))
    .map(mapWith(parseLine2, runStrategy, scoreLine_))
    .map(sum)
    .unwrap()

  console.log({ part1, part2 })
}

const parseThrow = (s) => {
  switch (s) {
    case 'A':
    case 'X':
      return just('Rock')
    case 'B':
    case 'Y':
      return just('Paper')
    case 'C':
    case 'Z':
      return just('Scissors')
    default:
      return nothing()
  }
}

const parseGoal = (s) => {
  switch (s) {
    case 'X':
      return just('Lose')
    case 'Y':
      return just('Draw')
    case 'Z':
      return just('Win')
    default:
      return nothing()
  }
}
const scoreLine_ = (n) =>
  n({
    just: (v) => scoreLine(v),
    nothing: () => {
      throw new Error()
    },
  })

const scoreLine = ([a, b]) =>
  switch_(`${a} ${b}`)({
    'Rock Rock': 3 + 1,
    'Rock Paper': 6 + 2,
    'Rock Scissors': 0 + 3,
    'Paper Rock': 0 + 1,
    'Paper Paper': 3 + 2,
    'Paper Scissors': 6 + 3,
    'Scissors Rock': 6 + 1,
    'Scissors Paper': 0 + 2,
    'Scissors Scissors': 3 + 3,
  })

const chooseThrow = (a, b) =>
  switch_(`${a} ${b}`)({
    'Rock Win': 'Paper',
    'Rock Lose': 'Scissors',
    'Rock Draw': 'Rock',
    'Paper Win': 'Scissors',
    'Paper Draw': 'Paper',
    'Paper Lose': 'Rock',
    'Scissors Win': 'Rock',
    'Scissors Draw': 'Scissors',
    'Scissors Lose': 'Paper',
  })

const runStrategy = (m) =>
  m({
    just: ([a, b]) => just([a, chooseThrow(a, b)]),
    nothing: () => nothing(),
  })

const parseLine1 = (s) => {
  const [a, b] = s.split(' ')
  const lhs = parseThrow(a)
  const rhs = parseThrow(b)

  return maybe2(lhs, rhs)
}

const parseLine2 = (s) => {
  const [a, b] = s.split(' ')
  const lhs = parseThrow(a)
  const rhs = parseGoal(b)
  return maybe2(lhs, rhs)
}

const maybe2 = (a, b) =>
  a({
    just: (aa) => b({ just: (bb) => just([aa, bb]), nothing: () => nothing() }),
    nothing: () => nothing(),
  })

const Id = (value) => ({ map: (f) => Id(f(value)), unwrap: () => value })
const just = (v) => ($) => $.just(v)
const nothing = () => ($) => $.nothing()
const switch_ = (k) => (opts) => opts[k]
const sum = (a) => a.reduce((a, b) => a + b, 0)

const mapWith =
  (...fs) =>
  (a) =>
    fs.reduce((a, f) => a.map(f), a)

main().catch((e) => {
  console.error(e)
  process.exitCode = 1
})
