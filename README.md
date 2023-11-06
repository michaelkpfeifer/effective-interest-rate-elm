### Introduction

The EffectiveInterestRate module exposes the `effectiveInterestRate`
function for computing the effective interest rate of a stream of
payments in a very general case.

### Assumptions

The current README file assumes that Elm is installed using `yarn`
within the current repository.  If you are using `npm` of if you have
installed Elm somewhere in the search path of your shell, you may need
to adjust the examples below. The `yarn repl` and `yarn test` commands
will most likely not work.

### Installation

Run
```bash
yarn install
yarn run elm make src/EffectiveInterestRate.elm
```
in the top level directory of the repository.

### Payment Streams

The Payment type alias is defined as follows:
```elm
type alias Payment =
    { amount : Float
    , date : Date.Date
    }
```
A payment consists of an amount (in whatever currency) and a date.
The amount can be positive or negative.

For example
```elm
{ amount = -2000, date = Date.fromCalendarDate 2015 Time.Jan 1 }
```
represents an amount of -2000 transferred at Jan 01, 2015.

A *payment stream* is a list of payments.

### Example

```
$ yarn repl
yarn run v1.22.19
$ yarn run elm repl
$ .../effective-interest-rate-elm/node_modules/.bin/elm repl
---- Elm 0.19.1 ----------------------------------------------------------------
Say :help for help and :exit to exit! More at <https://elm-lang.org/0.19.1/repl>
--------------------------------------------------------------------------------
> import Time
> import Date
> import List.Min2Items
> import EffectiveInterestRate
> payment1 = { amount = -2000, date = Date.fromCalendarDate 2015 Time.Jan 1 }
{ amount = -2000, date = RD 735599 }
    : { amount : number, date : Date.Date }
> payment2 = { amount = 1000, date = Date.fromCalendarDate 2016 Time.Jan 1 }
{ amount = 1000, date = RD 735964 }
    : { amount : number, date : Date.Date }
> payment3 = { amount = 1000, date = Date.fromCalendarDate 2017 Time.Jan 1 }
{ amount = 1000, date = RD 736330 }
    : { amount : number, date : Date.Date }
> payment4 = { amount = 200, date = Date.fromCalendarDate 2017 Time.Jan 1 }
{ amount = 200, date = RD 736330 }
    : { amount : number, date : Date.Date }
> paymentStream = List.Min2Items.Min2Items payment1 payment2 [ payment3, payment4 ]
Min2Items { amount = -2000, date = RD 735599 } { amount = 1000, date = RD 735964 } [{ amount = 1000, date = RD 736330 },{ amount = 200, date = RD 736330 }]
    : List.Min2Items.Min2Items { amount : number, date : Date.Date }
> EffectiveInterestRate.effectiveInterestRate paymentStream
Ok 0.06394102980498531 : Result String Float
```

### Tests

Run `yarn test`
