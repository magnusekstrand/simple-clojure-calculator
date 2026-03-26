# simple-clojure-calculator

A Clojure library designed to do simple calculations supporting just a few operators:
- "+"
- "-"
- "*"
- "/"
- "^"

## Usage

### CLI

```
$ lein run

--- Functional Clojure Calculator ---
Enter expressions like: 1 + (2 * 3)
Type 'exit' to quit.
```

### WEB UI

```
$ lein run -m shadow.cljs.devtools.cli watch app
$ open http://localhost:8080
```
