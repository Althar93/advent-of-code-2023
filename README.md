# advent-of-code-2023
A repository for the Advent of Code 2023 puzzle solutions.

Haskell is not my first language (or my second, or my third...) & I am very much inexperienced with it. As a result while my solutions *do* work, the algorithms & syntax employed should **by no means be considered optimal and/or even good**...

My solutions will tend towards explicit/verbose solutions over clever/compact ones : there is a mix of existing library functions where convenient as well as bespoke reimplementations of existing functions when I decide I would "reinvent the wheel".

## Building the project

*advent-of-code-2023* is built using the standard legacy **cabal** build steps. It can be configured and built as follows :

```
#!/bin/bash
cabal configure
cabal build
cabal run
```