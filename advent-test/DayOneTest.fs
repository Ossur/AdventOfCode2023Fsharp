module advent.DayOneTest

open Expecto
open advent.DayOne

let equal a b =
  if a <> b then
    failwith $"Value '{a}' does not equal '{b}'."

let readInput () = readInput ()

let testGetNumber () =
  let testWithDecoder decoderDescription decoder =
    getNumber decoder "0" |> equal 0
    getNumber decoder "01" |> equal 1
    getNumber decoder "10" |> equal 10
    getNumber decoder "123" |> equal 13
    getNumber decoder "asdf8asdf" |> equal 88
    getNumber decoder "1asdf8asdf9" |> equal 19
    getNumber decoder "twone" |> equal 21
    getNumber decoder "sevennine" |> equal 79 
    getNumber decoder "sevenine" |> equal 79 
    getNumber decoder "fivesix" |> equal 56 
    OssLib.prenta $"All tests to get number from string - using decoder {decoderDescription} - passed!"

  testWithDecoder "based on interpreted regex" decodeSpelledOutNumbersRegex
  testWithDecoder "based on String.Replace" decodeSpelledOutNumbersStringReplace
  testWithDecoder "based on precompiled regex" decodeSpelledOutNumbersPrecompiledRegex


let testDecodeNumber () =
  decodeSpelledOutNumbersRegex "one two three four five six seven eight nine"
  |> equal "1 2 3 4 5 6 7 8 9"

  decodeSpelledOutNumbersStringReplace "one two three four five six seven eight nine"
  |> equal "1 2 3 4 5 6 7 8 9"

  decodeSpelledOutNumbersPrecompiledRegex "one two three four five six seven eight nine"
  |> equal "1 2 3 4 5 6 7 8 9"



let propertyTestDecodeNumber (s: string) =
  let actualTest () =
    let numbersAsWords = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]
    let outputToTest = decodeSpelledOutNumbersStringReplace s
    let outputContains (s: string) = outputToTest.Contains(s)
    Seq.exists outputContains numbersAsWords |> not
  match s with
  | null -> true // just skip the nulls, man
  | _ -> actualTest()

let isRegexOrStringMatchFaster () =
  Expect.isFasterThan calcSum calcSumStringReplace "Regex is faster"

let checkCalcSum () = calcSum () |> OssLib.prenta

let checkCalcSumStringReplace () =
  calcSumStringReplace () |> OssLib.prentaJason

let testCalcSum () =
  let testAlgoVariation variation =
    Expect.equal
    <| variation ()
    <| 55614
    <| "Trebuchet calibration calculations were correct"

  testAlgoVariation calcSum
  testAlgoVariation calcSumPrecompiledRegex
  testAlgoVariation calcSumStringReplace

let benchmarkTests () =
  Expect.isFasterThan calcSumPrecompiledRegex calcSum "Precompiled regex is faster than interpreted regex"
  Expect.isFasterThan calcSumStringReplace calcSum "Using String.Replace is faster than interpreted regex"

  Expect.isFasterThan
    calcSumStringReplace
    calcSumPrecompiledRegex
    "Using String.Replace is faster than precompiled Regex"
    
