Worker
===

The aim of this library is to test solutions.

`worker` is a `gen_server` which works in background and automatically
fetches solutions from `problem_queue`. When `worker` have a solution to check
it does some defined sequence of steps:
* Unpack problem archive
* Prepare (via `make conditer`)
* Check solution (via `make check`)
* Load results from `results.json` and save it to db

`worker` relies on some files:
* `problem.json`

### problem.json
```json
{
    "name":"The great problem",
    "result": {
        "accepted": {},
        "memory": {},
        "time": {}
    }
}
```

