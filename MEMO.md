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

## background run

```
nohup bin/haskyapictl > /dev/null 2>&1 &
```

## kill

```
pkill haskyapictl
```
