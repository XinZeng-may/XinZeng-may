La Page 3.0 — 免费词典 + 翻译公开测试版

使用：Netlify 进入同一个项目 → Deploys → 把「La_Page_3.0_发布文件夹」整个文件夹拖入上传区。保持同一个域名，同一浏览器里的旧版文章和词汇通常可读取。升级前建议在 La Page 2.0/2.1 的「学习档案备份」先导出 JSON。

在线服务：
MyMemory (https://api.mymemory.translated.net/) 用于点击句子翻译；
FreeDictionaryAPI (https://freedictionaryapi.com/) 用于点击单词释义、词性等；内容源自 Wiktionary（CC BY-SA 4.0），网页带有必要归属和词条链接。

不保证无条件免费、永不下线：两服务有请求频率与每日字符限制，故翻译和查询都只在用户主动点击时进行。API 拒绝、CORS 失败或限额时页面会显示提示。

数据存储：所有文章、单词、翻译笔记默认只放在当前浏览器 localStorage；不支持跨设备自动同步。查询时，选中的句子/词组会发送给相应的第三方 API。切勿上传个人敏感或受保密限制的文件。

支持 TXT, MD, PDF, DOCX（网页通过免费的 CDN 加载 pdf.js/mammoth 来提取文本，扫描 PDF 不支持 OCR）。

更新包不包含独立服务器，无需 API Key。测试联网翻译需实际联网打开网页。