# Poker Hand Strength Evaluator

## Task Description

Your task is to develop an algorithm for sorting poker hands according to their strength.

### Poker Games

You should support one to three poker game types (more is better):
* Texas Hold'em - input is a board of 5 cards, and multiple hands of 2 cards each.

  A value of a Texas Hold'em hand is the best possible value out of all possible subsets of
  5 cards from the 7 cards which are formed by 5 board cards and 2 hand cards.

  See [Texas Hold'em rules](https://en.wikipedia.org/wiki/Texas_hold_%27em).

* Omaha Hold'em - input is a board of 5 cards, and multiple hands of 4 cards each.

  A value of an Omaha Hold'em hand is the best possible value out of all possible 5 card combinations
  which are formed from 3 out of 5 board cards and 2 out of 4 hand cards.

  See [Omaha Hold'em rules](https://en.wikipedia.org/wiki/Omaha_hold_%27em).

* Five Card Draw - input is multiple hands of 5 cards each.

  A value of a Five Card Draw hand is the value of the 5 hand cards.

  See [Five Card Draw rules](https://en.wikipedia.org/wiki/Five-card_draw).

### Hand Value

The hand values for 5 cards are as follows (in descending order - from strongest to weakest):

* `Straight Flush` - a `Straight` (see below) which is also a `Flush` (see below)
* `Four of a kind` - Four cards of the same rank
* `Full House` - a combination of `Three of a kind` and a `Pair`
* `Flush` - 5 cards of the same suit
* `Straight` - a sequence of 5 cards of consecutive rank (note an exception - `A` can both precede `2` and follow `K`)
* `Three of a kind` - three cards with the same rank
* `Two pairs` - two `Pair`-s
* `Pair` - two cards of the same rank
* `High card` - the "fallback" in case no other hand value rule applies

In case of ties the ranks of the cards forming the combinations decide the highest value.

In case of further ties, the ranks of the remaining cards decide the highest value.

All suits are considered equal in strength.

When comparing `Full House`-s, the `Three of a kind` rank comparison is more important than the `Pair` rank
comparison, for example, `QQQ88 > 999KK`, `KKK77 > QQQJJ` and `KKK77 > KKK66`.

When comparing `Straight`-s, the `A2345` `Straight` is the weakest one and the `TJQKA` one the strongest one,
for example, `23456 > A2345` and `TJQKA > 9TJQK`.

If any of these rules are not clear, please consult [Poker hand and categories](https://en.wikipedia.org/wiki/List_of_poker_hands#Hand-ranking_categories) and
the game rules of the specific poker games.

## Implementation

Your need to implement the `process` method in the [Solver.scala](src/main/scala/com/evolution/bootcamp/assignment/poker/Solver.scala)
class.

All the unit tests in [TexasHoldemSolverTest.scala](src/test/scala/com/evolution/bootcamp/assignment/poker/TexasHoldemSolverTest.scala), [OmahaHoldemSolverTest.scala](src/test/scala/com/evolution/bootcamp/assignment/poker/OmahaHoldemSolverTest.scala), and [FiveCardDrawSolverTest.scala](src/test/scala/com/evolution/bootcamp/assignment/poker/FiveCardDrawSolverTest.scala) should pass. There will also be other tests run after you submit your solver. 

### Input of Test Cases

The input is to be read as a string:

```
<game-type> [<5 board cards>] <hand 1> <hand 2> <...> <hand N>
```

...where:

* `game-type` specifies the game type for this test case, one of:
  * `texas-holdem` - for Texas Hold'em
  * `omaha-holdem` - for Omaha Hold'em
  * `five-card-draw` - for Five Card Draw

* `<5 board cards>` is a 10 character string where each 2 characters encode a card, only used for Texas and
  Omaha Hold' ems

* `<hand X>` is a 4, 8 or 10 character string (depending on game type) where each 2 characters encode a card
* `<card>` is a 2 character string with the first character representing the rank
  (one of `A`, `K`, `Q`, `J`, `T`, `9`, `8`, `7`, `6`, `5`, `4`, `3`, `2`) and the second character representing
  the suit (one of `h`, `d`, `c`, `s`). Jokers are not used.

### Output of Solutions

The output should be returned as a string:

```
<hand block 1> <hand block 2> <...> <hand block n>
```
... where:

* `<hand block 1>` is the hand block with the weakest value
* `<hand block 2>` is the hand block with the second weakest value
* ... and so forth.
* `<hand block n>` is the hand block with the strongest value

Each hand block consists of one or multiple hands (each represented by 4, 8 or 10 character string, depending
on game type, with 2 characters to encode a card) with equal hand value.

In case there are multiple hands with the same value on the same board they should be ordered alphabetically
and separated by `=` signs.

The order of the cards in each hand should remain the same as in the input, e.g., don't reorder `2h3s` into
`3s2h`.

#### Examples

Example input:
```
texas-holdem 4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d
texas-holdem 2h3h4h5d8d KdKs 9hJh
omaha-holdem 3d3s4d6hJc Js2dKd8c KsAsTcTs Jh2h3c9c Qc8dAd6c 7dQsAc5d
five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c
```

Example output:
```
Ac4d=Ad4s 5d6d As9s KhKd
KdKs 9hJh
Qc8dAd6c KsAsTcTs Js2dKd8c 7dQsAc5d Jh2h3c9c
4c8h2h6c9c Ah9d6s2cKh Kd9sAs3cQs 7h4s4h8c9h Tc5h6dAc5c
```

### Requirements for Implementation

Include a `ReadMe.md` file which documents all known limitations of your solution, in terms of functionality
implemented, known defects, or considerations how to build and run it.

The code, comments and documentation should be in English.

## Grading Notes

For extra credit you should do the following (one or more):
* Implement the algorithm using Scala
* Use a Functional Programming approach in implementation
* Support two or, even better, three of the aforementioned game types

A complete, working solution in a non-Scala language will be preferred to an incomplete solution in Scala.

Yielding correct results and code readability is more important than the algorithmic complexity of the
solution.

## Submitting your solution

Please push your changes to the `main branch` of this repository. You can push one or more commits. <br>

Once you are finished with the task, please click the `Complete task` link on <a href="https://app.codescreen.com/#/codescreentest61f2b270cdf0aa010256472e" target="_blank">this screen</a>.