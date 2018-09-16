const puppeteer = require('puppeteer');

(async () => {
  const browser = await puppeteer.launch({args: process.argv.slice(3)});
  const page = await browser.newPage();
  await page.goto(process.argv[2],
                  {
                    waitUntil: ["load", "domcontentloaded", "networkidle0"],
                    timeout: 0,
                  });

  const data = await page.evaluate(async () => {
    function arrayBufferToBase64(buffer){
      var binary = '';
      var bytes = [].slice.call(new Uint8Array(buffer));
      bytes.forEach((b) => binary += String.fromCharCode(b));
      return window.btoa(binary);
    };

    const response = await fetch(document.querySelector("img").src);
    if (!response.ok) {
      throw new Error(`Could not fetch image, (status ${response.status}`);
    }

    const image_binary = await response.arrayBuffer();
    return arrayBufferToBase64(image_binary);
  });

  console.log(data);
  await browser.close();
})();
