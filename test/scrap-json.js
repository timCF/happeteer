const puppeteer = require('puppeteer');

(async () => {
  const browser = await puppeteer.launch({args: process.argv.slice(3)});
  const page = await browser.newPage();
  await page.goto(process.argv[2], {
    waitUntil: ["load", "domcontentloaded", "networkidle0"],
    timeout: 0,
  });
  const json = await page.evaluate(() => {
    return JSON.stringify({"hello":"world"});
  });
  console.log(json);
  await browser.close();
})();
