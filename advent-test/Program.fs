module advent_test

open advent.DayOneTest
open Expecto
open FsCheck

let generatorOfSemiAlphaNumericAsciiString =
  [ Gen.choose (48, 90); Gen.choose (97, 124) ]
  |> Gen.oneof
  |> Gen.map char
  |> Gen.nonEmptyListOf
  |> Gen.map (Seq.toArray >> System.String)

let spelledOutNumbers =
  Gen.choose (1, 9)
  |> Gen.map (fun n ->
    match n with
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine")

let generatorOfStringsThatOftenContainSpelledOutNumbers =
  Gen.frequency [ (4, generatorOfSemiAlphaNumericAsciiString); (1, spelledOutNumbers) ]
  |> Gen.listOfLength 4
  |> Gen.map (String.concat "")

type MyGenerators =
  static member StringGen() =
    { new Arbitrary<string>() with
        override _.Generator = generatorOfStringsThatOftenContainSpelledOutNumbers
        override _.Shrinker _ = Seq.empty }


[<EntryPoint>]
let rec main args =

  runTestsWithCLIArgs [] args
  <| testList
    "Tests for Advent of Code 2023"
    [ testList
        "Day One tests"
        [ test "Test getNumber from one line" { testGetNumber () }
          test "Test decode spelled out numbers" { testDecodeNumber () }
          test "Test for the final answer" { testCalcSum () }
          test "Using String.Replace is faster than using regex" { benchmarkTests () }
          test "Decode works on all single digit numbers except zero" {
                ({ Config.Verbose with Arbitrary = [ typeof<MyGenerators> ] }, propertyTestDecodeNumber) |> Check.One
          }
        ]
        
      testList "Day Two tests" [ test "asdf" { true } ] ]
