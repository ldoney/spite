# spite

This is a fork of the language Loot from CMSC 430 with an emphasis on interactability with the system. It includes libraries, file I/O, socket handling, and includes some useful libraries.

Some sample code is included in `sample/`. Some sample libraries are included in `lib/`. Unit tests are included in `test/`. The compiler itself is contained in the root directory.

## Compiling & Running a Program

Make must be called from the root of the program. Supposing we have the file `42.rkt`, you can compile it using `make 42.run`, which generates an executable, `42.run`, which can be run through sh as `./42.run`.

To compile code in the `sample/` directory to use the example code, it must be called from the root directory. For example, to compile the echo server, run `make sample/echo_server.run` from the root directory of the project. `sample/web_server.rkt` must be run in the same directory as `index.html`, as it references that

## Running unit tests

To run unit tests, you can call `raco test test/compile.rkt`, which will call all of the unit tests for language completeness. These do not test most of the side effect features of the language, but they do test completeness of the base language features.

## Cleanup

`make clean` removes all generated files in the project.
