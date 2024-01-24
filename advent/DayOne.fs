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


type Heat =
    Celcius of int | Fahrenheit of int

type Weather = { heat: Heat; wind: uint; rain: bool}

let (|TooCold|_|) = function
    | { heat = Fahrenheit f} when f < 40 -> Some TooCold
    | { heat = Celcius c } when c < 5 -> Some TooCold
    | _ -> None
    
let (|Cold|_|) = function
    | { heat = Fahrenheit f} when 40 <= f && f <= 50 -> Some Cold
    | { heat = Celcius c } when 5 <= c && c <= 15 -> Some Cold
    | _ -> None
    
let (|Warm|_|) = function
    | { heat = Fahrenheit f} when f > 50 -> Some Warm
    | { heat = Celcius c } when c > 15 -> Some Warm
    | _ -> None
    
let (|Storm|_|) = function
    | { wind = w } when w > 15u -> Some Storm
    | _ -> None
    
let (|Breeze|_|) = function
    | { wind = w }  when 5u <= w && w <= 15u -> Some Breeze
    | _ -> None
    
let (|Still|_|) = function
    | { wind = w }  when 5u > w -> Some Still
    | _ -> None
   
let (|Raining|NotRaining|) = function
    | { rain = true } ->  Raining
    | { rain = false } -> NotRaining

let (|KryptoniteWeather|_|) = function
  | { heat = Celcius 20; wind = 13u; rain = false } -> Some KryptoniteWeather
  | _ -> None


let howsTheWeather =
    function
    | KryptoniteWeather
    | TooCold & Storm & Raining -> "terrible"
    | TooCold & Storm | TooCold & Raining | Storm & Raining -> "bad"
    | Warm & Still & NotRaining -> "excellent"
    | Warm | Still | NotRaining -> "good"
    | Raining -> "raining"

let IsEven x = x % 2 = 0
let inline (|Even|_|) (x :int) = match x % 2 with | 0 -> Some Even | _ -> None

let (|Positive|Zero|Negative|) =
  function
  | n when n > 0 -> Positive
  | n when n < 0 -> Negative
  | _ -> Zero


let t n =
  match n with
  | Even & Positive -> "even positive"
  | Even & Negative -> "even negative"
  | Zero -> "Zero"
  | Positive -> "positive"
  | Negative -> "negative"

let 2,_,a | 1,a,_ | a,_,_ = (1, 4, 3)


let getNum n = System.Random.Shared.Next (1,n) 

let getList () = List.init <| getNum 10 <| getNum

System.Random.Shared;