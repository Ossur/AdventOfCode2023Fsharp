module advent.DayOne

open System.IO

let readInput () = File.ReadLines "inputs/dayOne"

open System.Text.RegularExpressions

let (|Both|One|Neither|) (r1: Regex, r2: Regex, s: string) =
  match r1.Match(s), r2.Match(s) with
  | m1, m2 when m1.Success && m2.Success -> Both(m1.Value, m2.Value)
  | m1, _ when m1.Success -> One m1.Value
  | _, m2 when m2.Success -> One m2.Value
  | _ -> Neither

let getNumberStringFromLine =
  function
  | Both(f, b) -> $"{f}{b}"
  | One s -> $"{s}"
  | Neither -> "0"

let stringReplace (s: string, f: string, r: string) = s.Replace(f, r)

let decodeSpelledOutNumbersStringReplace (text: string) =
  [ ("one", "1")
    ("two", "2")
    ("three", "3")
    ("four", "4")
    ("five", "5")
    ("six", "6")
    ("seven", "7")
    ("eight", "8")
    ("nine", "9") ]
  |> Seq.fold (fun (txt: string) -> txt.Replace) text

let decodeSpelledOutNumbersRegex (text: string) =
  [ ("one", "1")
    ("two", "2")
    ("three", "3")
    ("four", "4")
    ("five", "5")
    ("six", "6")
    ("seven", "7")
    ("eight", "8")
    ("nine", "9") ]
  |> Seq.fold (fun (txt: string) (p: string, r: string) -> Regex.Replace(txt, p, r)) text

let precompiledRegexes =
  [ (Regex("one"), "1")
    (Regex("two"), "2")
    (Regex("three"), "3")
    (Regex("four"), "4")
    (Regex("five"), "5")
    (Regex("six"), "6")
    (Regex("seven"), "7")
    (Regex("eight"), "8")
    (Regex("nine"), "9") ]

let decodeSpelledOutNumbersPrecompiledRegex (text: string) =
  precompiledRegexes
  |> Seq.fold (fun (txt: string) (p: Regex, r: string) -> p.Replace(txt, r)) text

let regexString = @"([0-9]|one|two|three|four|five|six|seven|eight|nine)"

let regexForFirst, regexForLast =
  (Regex(regexString), Regex(regexString, RegexOptions.RightToLeft))

let getNumber decode (text: string) =
   (regexForFirst, regexForLast, text) |> getNumberStringFromLine |> decode |> int

let calcSum () =
  readInput () |> Seq.map (getNumber decodeSpelledOutNumbersRegex) |> Seq.sum

let calcSumStringReplace () =
  readInput ()
  |> Seq.map (getNumber decodeSpelledOutNumbersStringReplace)
  |> Seq.sum

let calcSumPrecompiledRegex () =
  readInput ()
  |> Seq.map (getNumber decodeSpelledOutNumbersPrecompiledRegex)
  |> Seq.sum
