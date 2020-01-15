# eoc-cl

Common Lisp solutions for the exercises from [Essentials of Compilation - An Incremental Approach](http://jeapostrophe.github.io/courses/2019/spring/406/notes/book.pdf). The source code is organised under branches, one for each chapter. The master branch represents the latest completed chapter.

### Installation

Clone the repo into some directory [quicklisp](https://www.quicklisp.org) can find it, then:

```lisp
(ql:quickload "eoc-cl")
```

### Testing

```lisp
(asdf:test-system 'eoc-cl)
```

Tests are performed by generating random programs and comparing the output of evaluating those programs directly under the host system (or under an interpreter), with the output of running (or interpreting) the transformed program. To that end, the tests expect that gcc is in your PATH.

## License

BSD

