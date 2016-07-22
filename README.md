# mazicus

This project features a number of algorithms to generate mazes in Clojure (according to Mazes for Programmers book).
Throughout coding this I was also learning Clojure. Feel free to comment on code or contribute via PR.

## Usage

Install Leiningen
```
$ lein run -- -help
  -a, --algorithm NAME  :binary  Algorithm name [binary sidewinder aldousbroder wilson huntnkill backtrack]
  -m, --maze TYPE       :square  Maze type [square polar]
  -s, --size SIZE       10       Maze size
  -h, --help
```

Sample run
```
$ lein run -a backtrack -s 20 -m polar
```

## TODO

- unit tests
- nicer output
- jar builds on commit
- CI integration

- hexagon grid
- triangular grid
- weaving 
- more sensible metrics
