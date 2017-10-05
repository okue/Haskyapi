# This page is written in Markdown

# このページはマークダウンで書かれています

```python
import requests
requests.post("http://0.0.0.0:8080/api/add", params={"x":10, "y":1000})
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
