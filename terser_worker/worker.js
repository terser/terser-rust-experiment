#!/usr/bin/env node

const assert = require('assert')
const Terser = require('terser')

// We need to use stderr to debug things,
// because the protocol is over stdin/stdout
console.log = console.error

const waitForReadable = () => new Promise((resolve, reject) => {
  const ok = () => {
    resolve()
    unlisten()
  }
  const notOk = (error) => {
    reject(error || new Error('Stream finished!'))
    unlisten()
  }
  const unlisten = () => {
    process.stdin.off('readable', ok)
    process.stdin.off('error', notOk)
    process.stdin.off('end', notOk)
  }

  process.stdin.once('readable', ok)
  process.stdin.once('error', notOk)
  process.stdin.once('end', notOk)
})

async function readSmall(n) {
  const buffered_read = process.stdin.read(n)

  if (buffered_read != null) {
    if (buffered_read.length < n) {
      throw new Error('stream finished')
    }
    return buffered_read
  }

  await waitForReadable()

  return await readSmall(n)
}

async function readLarge(n) {
  const readBuf = await new Promise((resolve, reject) => {
    let bufs = [Buffer.from("")]
    let bufsLen = 0

    const onData = data => {
      bufs.push(data)
      bufsLen += data.length

      assert(bufsLen <= n)

      if (bufsLen === n) {
        resolve(Buffer.concat(bufs))
        process.stdin.off('data', onData)
      }
    }

    process.stdin.on('data', onData)
  })

  return readBuf
}

const write = data => {
  return new Promise((resolve, reject) => {
    if (!process.stdout.write(data)) {
      process.stdout.once('drain', resolve);
    } else {
      resolve()
    }
  })
}

(async function main() {
  while (true) {
    const toRead = Number(await readSmall(32))

    const jsText = await readLarge(toRead)

    const { code } = await Terser.minify(jsText.toString(), {
      mangle: false,
      compress: {
        sequences: false
      },
    })

    const codeBuf = Buffer.from(code)

    await write(codeBuf.length.toString().padEnd(32, ' '))
    await write(codeBuf)
  }
}()).catch(e => {
  console.error(e)
  process.exit(1)
})
