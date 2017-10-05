<img src="https://raw.githubusercontent.com/okue/Haskyapi/master/html/img/logo.png" width="60%">

## What is Haskyapi ?

HTTP sever implemented in Haskell.

```
$ make
$ ./bin/haskyapictl -p 8080 -r .
port: 8080 root: .
listen on 8080
http://localhost:8080/
http://localhost:8080/index.html
http://localhost:8080/hoge.md
```

- [x] Open Markdown file
- `-p` : port number
- `-r` : root directory

## TODO

- [ ] How to use Database easily
- [ ] How to implement RESTful api easily
- [ ] Automatic generator of api reference document
- [ ] HTTPS

## Now

Haskyapi works at [okue.site:80](http://okue.site/)

- [Simple TODO application](http://ftodo.okue.site/)
- [/index.html](http://okue.site/)
- [/page.html](http://okue.site/page.html)
- [/markdown-page.md](http://okue.site/markdown-page.md)
- [/api/title?url=https://github.com/okue/Haskyapi](http://okue.site/api/title?url=https://github.com/okue/Haskyapi)
