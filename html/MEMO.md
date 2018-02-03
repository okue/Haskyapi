## Request Example

```python
import requests
requests.post("http://0.0.0.0:8080/api/add", params={"x":10, "y":1000})
```

```sh
curl "http://localhost:8080/api/test" -H 'Content-Type:application/json' -d "{ \"key\" : \"1\" }"
```

## libapi.so

```sh
LD_LIBRARY_PATH=path/to/Haskyapi
export LD_LIBRARY_PATH
```

```sh
LD_LIBRARY_PATH=.   bin/haskyapictl -p 8000
DYLD_LIBRARY_PATH=. bin/haskyapictl -p 8000
```

```sh
SRC_C=src/Api/Capi.c
SO=libapi.so
gcc -shared $SRC_C -o $SO -fPIC
```

## background run

```sh
nohup bin/haskyapictl > /dev/null 2>&1 &
nohup sudo LD_LIBRARY_PATH=. bin/haskyapictl -p 80 > /dev/null 2>&1 &
```

## kill

```sh
pkill haskyapictl
```

## run script

```sh
#! /usr/bin/env sh
DYLD_LIBRARY_PATH=~/Sites/haskyapi/src ~/Sites/haskyapi/bin/haskyapictl $1 $2 $3 $4
```

## RESTful

- `GET /entries`
- `POST /entries`
- `GET /entries/$entry_id`
- `PUT /entries/$entry_id`
- `DELTE /entries/$entry_id`


## Responsive

```html
<meta name="viewport" content="width=device-width, initial-scale=1, minimum-scale=1, maximum-scale=1, user-scalable=no">
```

```
git log -p
git show ...
git log --stat
```

## bash-completion

```sh
cp .haskyapi.bash ~/.haskyapi.bash
```

## Apache Bench

```sh
ab -n 100 -c 10 http://0.0.0.0
```
