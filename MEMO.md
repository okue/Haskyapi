## Request Example

```python
import requests
requests.post("http://0.0.0.0:8080/api/add", params={"x":10, "y":1000})
```

## libapi.so

```sh
LD_LIBRARY_PATH=path/to/Haskyapi
export LD_LIBRARY_PATH
```

```sh
sudo LD_LIBRARY_PATH=. bin/haskyapictl -p 8000
```

```sh
DYLD_LIBRARY_PATH=. bin/haskyapictl -p 8000
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
DYLD_LIBRARY_PATH=~/Sites/haskyapi/ ~/Sites/haskyapi/bin/haskyapictl
```

## RESTful

- `GET /entries`
  - エントリー一覧を取得する
- `POST /entries`
  - エントリーを追加する
- `GET /entries/$entry_id`
  - 特定のエントリーを取得する
- `PUT /entries/$entry_id`
  - 特定のエントリーを置き換える
- `DELTE /entries/$entry_id`
  - 特定のエントリーを削除する


## Responsive

```html
<meta name="viewport" content="width=device-width, initial-scale=1, minimum-scale=1, maximum-scale=1, user-scalable=no">
```
