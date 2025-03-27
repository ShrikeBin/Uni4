## First steps:
```bash
>>>a=IP(ttl=10)
>>>a
< IP ttl=10 |>
>>>a.src
’127.0.0.1’
>>>a.dst="192.168.1.1"
>>>a
< IP ttl=10 dst=192.168.1.1 |>
>>>a.src
’192.168.8.14’
>>>del(a.ttl)
>>>a
< IP dst=192.168.1.1 |>
>>>a.ttl
64```