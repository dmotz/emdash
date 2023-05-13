import {spawn} from 'child_process'
import {writeFileSync} from 'fs'
import {fileURLToPath} from 'url'
import {dirname} from 'path'
import puppeteer from 'puppeteer'

const server = spawn('npm', ['run', 'preview'])

server.stdout.on('data', async d => {
  if (!d.toString().includes('localhost')) {
    return
  }

  const browser = await puppeteer.launch({headless: 'new'})
  const page = await browser.newPage()
  await page.goto('http://localhost:4173/')
  await page.waitForSelector('.anim')
  await page.evaluate(() => (document.querySelector('.anim').innerHTML = ''))
  writeFileSync(
    dirname(fileURLToPath(import.meta.url)) + '/../dist/index.html',
    await page.content()
  )
  await browser.close()
  server.kill()
})
