const main = async () => {
  const data = await require('node:fs/promises').readFile(
    './input.txt',
    'utf-8'
  )
  const lines = data.split('\n')
  const elves = []
  let start = 0
  let end
  while ((end = lines.indexOf('', start)) !== -1) {
    elves.push(lines.slice(start, end))
    start = end + 1
  }

  elves.push(lines.slice(start))

  let e1 = elves.map((elf) =>
    elf.map((n) => Number(n)).reduce((a, b) => a + b, 0)
  )

  console.log(Math.max(...e1))
}

main().catch((e) => {
  console.error(e)
  process.exitCode = 1
})
