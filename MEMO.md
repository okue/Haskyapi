## Request Example

```python
import requests
requests.post("http://0.0.0.0:8080/api/add", params={"x":10, "y":1000})
```

## libapi.so

```
LD_LIBRARY_PATH=path/to/Haskyapi
export LD_LIBRARY_PATH
```

```
sudo LD_LIBRARY_PATH=. bin/haskyapictl 80
```

```
DYLD_LIBRARY_PATH=. bin/haskyapictl 80
```

## background run

```
nohup bin/haskyapictl > /dev/null 2>&1 &
nohup sudo LD_LIBRARY_PATH=. bin/haskyapictl 80 > /dev/null 2>&1 &
```

## kill

```
pkill haskyapictl
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
