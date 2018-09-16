const puppeteer = require('puppeteer');

(async () => {
  const browser = await puppeteer.launch({args: process.argv.slice(3)});
  const page = await browser.newPage();
  await page.goto(process.argv[2],
                  {
                    waitUntil: ["load", "domcontentloaded", "networkidle0"],
                    timeout: 0,
                  });
  const resolved_url = await page.evaluate(() => {
    return window.location.href;
  });
  console.log(resolved_url);
  await browser.close();
})();
