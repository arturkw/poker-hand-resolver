#limitations
- Missing input verification for arguments: invalid input leads to invalid output
- Missing input verification for classes derived from **com.evolution.bootcamp.assignment.poker.HandValue**.
  The following code executes without exception/error/warnings:
```
new FullHouse(Seq(card("Th"), card("Ac"), card("9d")), Seq(card("5c"), card("9h")))
```
- Refactoring is needed: methods in **com.evolution.bootcamp.assignment.poker.Solver** class should
  validate arguments and convert them to implementations of **com.evolution.bootcamp.assignment.poker.Model** as soon as possible
# Startup instructions
To start the application run the following command in project home directory:
```
sbt
```
When the program asks for arguments:
```
[info] running com.evolution.bootcamp.assignment.poker.Main 
Five card draw 4s5hTsQh9h Qc8d7cTcJd 5s5d7s4dQd 3cKs4cKdJs 2hAhKh4hKc 7h6h7d2cJc As6d5cQsAc
4s5hTsQh9h Qc8d7cTcJd 5s5d7s4dQd 7h6h7d2cJc 3cKs4cKdJs 2hAhKh4hKc As6d5cQsAc
```
pass the arguments to the command line:
```
[info] running com.evolution.bootcamp.assignment.poker.Main 
Five card draw 4s5hTsQh9h Qc8d7cTcJd 5s5d7s4dQd 3cKs4cKdJs 2hAhKh4hKc 7h6h7d2cJc As6d5cQsAc
```