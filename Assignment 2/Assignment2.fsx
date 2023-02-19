// 2.1
let downto1 n = 
    if n > 0 then [n .. - 1 .. 1]
    else []

(* downto1 10 *)

let downto2 n =
    match n with
    | n when n > 0 -> [n .. -1 .. 1]
    | n when n <= 0 -> []
    | _ -> []

(* downto2 10 *)

// 2.2 
let rec recursiveOddRemoval acc = 
    function 
    | [] -> []
    | x::xs when acc % 2 <> 0 -> recursiveOddRemoval (acc+1) xs 
    | x::xs -> x::recursiveOddRemoval (acc+1) xs 

let removeOddIdx (list: 'a list) = recursiveOddRemoval 0 list

(* removeOddIdx ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece";
 "was"; "white"; "as"; "snow"];; *)

// 2.3 
let rec recursivePairing acc =
    function
    | [] -> []
    | x::y::xs when acc % 2 = 0 -> (x, y) :: recursivePairing (acc+1) xs
    | x::xs when xs.Length > 0 -> recursivePairing (acc+1) (x::xs)
    | _ -> []

let combinePair list : 'a list = recursivePairing 0 list


combinePair [1; 2; 3; 4; 5]
(* combinePair ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece";
 "was"; "white"; "as"; "snow"];; *)

// 2.4
type complex = 
    { 
    real : float
    imaginary : float 
    }

let mkComplex a b =
    let result = { real = a; imaginary = b }
    result

let complexToPair complex =
    let (a, b) = complex.real, complex.imaginary
    a, b

let (|+|) a b =
    let result = { real = (a.real + b.real); imaginary = (a.imaginary + b.imaginary) }
    result

let (|*|) a b = 
    let result = { real = (a.real * b.real - a.imaginary * b.imaginary); imaginary = (a.imaginary * b.real + a.real * b.imaginary) }
    result

let (|-|) a b = 
    let negativeB = { real = -b.real; imaginary = -b.imaginary }
    a |+| negativeB

let (|/|) a b =
    let inverseB = { real = b.real/((b.real * b.real) + (b.imaginary * b.imaginary)); imaginary = -b.imaginary/((b.real * b.real) + (b.imaginary * b.imaginary)) }
    a |*| inverseB

(mkComplex -3.3 10.3) |/| (mkComplex -3.2 -2.0) |> complexToPair

// 2.5
let explode1 (s : string) =
    let array = s.ToCharArray()
    let list = List.ofArray(array)
    list

(* explode1 "Hello World!" *)

let rec explode2 = 
    function
    | "" -> []
    | s -> s.[0]::explode2(s.Remove(0, 1))

(* explode2 "Hello World!" *)

// 2.6
let implode (cs : char list) =
    let result = List.fold(fun x y -> x + y.ToString()) "" cs
    result

(* implode ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!'];; *)

let implodeRev (cs : char list) =
    let result = List.foldBack(fun x y -> y + x.ToString()) cs ""
    result

(* implodeRev ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!'];; *)

// 2.7
// let toUpper s = s |> explode1 |> List.map(fun x -> System.Char.ToUpper x) |> implode
let toUpper = explode1 >> List.map(fun x -> System.Char.ToUpper x) >> implode

(* toUpper "Hello World!" *)

// 2.8
let rec ack (m, n) =
    match (m, n) with
    | (0,_) -> n + 1
    | (m, 0) when m > 0 -> ack (m-1, 1)
    | (m, n) when m > 0 && n > 0 -> ack (m-1, ack (m, n-1))
    | _ -> failwith "Input is invalid. Please try with valid input."

(* ack (3, 5) *)

// Yellow exercises
// 2.9
let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

(* time (fun () -> ack (3, 11)) *)

let timeArg1 f a =
    time (fun () -> f a)

(* timeArg1 ack (3,11) *)

// 2.10
let rec downto3 f n e =
    match n with
    | n when n <= 0 -> e
    | n when n > 0 -> f (downto3 f (n-1) e) 
    | _ -> e

// 2.11
type word = (char * int) list

let hello : word = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1);]

// 2.12
type squareFun = word -> int -> int -> int

let singleLetterScore (word : word) (pos : int) (acc : int) =
    let l, p = word.[pos]
    p + acc

let doubleLetterScore (word : word) (pos : int) (acc : int) =
    let l, p = word.[pos]
    (p * 2) + acc

let tripleLetterScore (word : word) (pos : int) (acc : int) =
    let l, p = word.[pos]
    (p * 3) + acc

(* doubleLetterScore hello 4 42 *)

// 2.13
let doubleWordScore (word : word) (pos : int) (acc : int) =
    acc * 2

let tripleWordScore (word : word) (pos : int) (acc : int) =
    acc * 3

(* doubleWordScore hello 12345 42 *)

// 2.14
(* let rec oddConsonants (word : word) (pos : int) (acc : int) =
    let l, p = word.[pos]
    if pos = word.Length then acc
    elif pos = word.Length && acc % 2 <> 0 then -acc
    elif "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ".Contains l then oddConsonants word (pos+1) (acc+1)
    else oddConsonants word (pos+1) acc *)
  
// 2.15
type square = (int * squareFun) list

let SLS : square = [(0, singleLetterScore)]
let DLS : square = [(0, doubleLetterScore)]
let TLS : square = [(0, tripleLetterScore)]
let DWS : square = SLS @ [(1, doubleWordScore)]
let TWS : square = SLS @ [(1, tripleWordScore)]

let calculatePoints (squares : square list) (word : word) =
    squares |> List.mapi(fun x y -> List.map(fun x -> x) [x]) |> 
    List.fold(fun x y -> x @ y)