<img src="https://raw.githubusercontent.com/okue/Haskyapi/master/html/img/logo.png" width="60%">

[![Build Status](https://travis-ci.org/okue/Haskyapi.svg?branch=master)](https://travis-ci.org/okue/Haskyapi)

## What is Haskyapi?

Haskyapi is a HTTP server implemented in Haskell.

### Build

```sh
$ stack build
$ stack install
$ haskyapi migrate
$ haskyapi runserver -p 8080 -r .
root: .
listen on 8080
http://localhost:8080/
http://localhost:8080/index.html
http://localhost:8080/hoge.md
```

or

```sh
$ cabal build
```

### Options

- `-p, --port` : port number
- `-r, --root` : root directory
- `-h, --help` : help


## TODO

- [x] Open Markdown file
- [ ] How to use Database easily
- [ ] How to implement RESTful api easily
- [ ] Automatic generator of api reference document
- [ ] HTTPS

## Bash-completion

`.haskyapi.bash` is a bash-completion setting file for **haskyapi** command.

## Now

Haskyapi works at [okue.site:80](http://okue.site/)

- [Simple TODO](http://ftodo.okue.site/)
- [/markdown-page.md](http://okue.site/markdown-page.md)
- [/api/title?url=https://github.com/okue/Haskyapi](http://okue.site/api/title?url=https://github.com/okue/Haskyapi)

