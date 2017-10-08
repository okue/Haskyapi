<img src="https://raw.githubusercontent.com/okue/Haskyapi/master/html/img/logo.png" width="60%">

[![Build Status](https://travis-ci.org/okue/Haskyapi.svg?branch=master)](https://travis-ci.org/okue/Haskyapi)

## What is Haskyapi ?

HTTP sever implemented in Haskell.

### Build

```sh
$ stack build
$ stack install
$ haskyapictl -p 8080 -r .
port: 8080 root: .
listen on 8080
http://localhost:8080/
http://localhost:8080/index.html
http://localhost:8080/hoge.md
```

### options

- `-p` : port number
- `-r` : root directory
- `-h` : help


## TODO

- [x] Open Markdown file
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

