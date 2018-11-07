<img src="https://raw.githubusercontent.com/okue/Haskyapi/master/html/img/logo.png" width="60%">

[![Build Status](https://travis-ci.org/okue/Haskyapi.svg?branch=master)](https://travis-ci.org/okue/Haskyapi)

## What is Haskyapi?

Haskyapi is a HTTP server implemented in Haskell.

### Build

```sh
$ stack build
$ stack install
$ haskyapi migrate
$ haskyapi runserver --port 8080 --root .
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


`setting.yml` is a configuration file for these options.


## How to use as web framework

`app/Main.hs` in this repository is an example program using haskyapi as web framework.

Here is a very simple example.

```hs
module Main where
import Web.Haskyapi.Console.Cli (haskyapi)
import Web.Haskyapi.Header (
  Api,
  ApiFunc,        -- type of api functions, Query -> Body -> IO String
  Method(..),     -- GET, POST, ...
  ContentType(..) -- Cplain, Cjson, ...
  )

routing :: [Api]
routing = [
             (GET,  "/test", test , Cplain)
            ,(POST, "/test", test2, Cplain)
          ]

test :: ApiFunc
test qry bdy = return "This is GET."

test2 :: ApiFunc
test2 qry bdy = return "This is POST."

main = haskyapi routing
```


```sh
$ runhaskell Main.hs runserver
$ curl -XGET http://localhost:8080/api/test
This is GET.
$ curl -XPOST http://localhost:8080/api/test
This is POST.
```


## TODO

- [x] Open Markdown file
- [ ] Use Database easily
- [ ] Implement RESTful api easily
- [ ] Automatic generator of api reference document
- [ ] HTTPS

## Bash-completion

`.haskyapi.bash` is a bash-completion setting file for **haskyapi** command.

## Now

Haskyapi works at [okue.site:80](http://okue.xyz/)

- [/markdown-page.md](http://okue.xyz/markdown-page.md)
- [/api/title?url=https://github.com/okue/Haskyapi](http://okue.xyz/api/title?url=https://github.com/okue/Haskyapi)
