// 1.1
let sqr x = x * x
(* sqr 5 *)

// 1.2
let pow x n = System.Math.Pow(x, n)
(* pow 2 5 *)

// 1.3
let rec sum =
    function
    | 0 -> 0
    | n -> n + sum(n - 1);;
(* sum 10 *)

// 1.4
let rec fib = 
    function
    | 0 -> 0   
    | 1 -> 1
    | n -> fib(n-1) + fib(n-2)
(* fib 20 *)

// 1.5
let dup (s : string) = s + s
(* dup "Hi " *)

// 1.6
let rec dupn (s : string) (n : int) = 
    match n with
    | 0 -> ""
    | _ -> s + dupn s (n-1)
(* dupn "Hi " 3 *)

// 1.7
let rec bin (n, k) = 
    match n, k with
    | (n, k) when k = 0 -> 1
    | (n, k) when n = k -> 1
    | _ -> (bin ((n-1), (k-1))) + (bin ((n-1), k))
(* bin 4 2 *)

// 1.8
let timediff (t1 : int * int) (t2 : int * int) = 
    let (t1hour, t1min) = t1
    let (t2hour, t2min) = t2
    let t1InMinutes = t1hour * 60 + t1min
    let t2InMinutes = t2hour * 60 + t2min
    let difference = t2InMinutes - t1InMinutes
    difference
(* timediff (12, 34) (11, 35);; *)

// 1.9
let minutes (t1 : int * int) = 
    let difference = timediff (00, 00) t1
    difference
(* minutes (14, 24) *)

// 1.10
let curry f a b = f(a,b)
(* curry (fun (x, y) -> x + y) 5 3;; *)

let uncurry f (a,b) = f a b
(* uncurry (fun x y -> x + y) (5, 3);; *)

// 1.11
let empty (letter, pointValue) = fun pos -> letter, pointValue
let theLetterA : int -> char * int = empty ('A', 1)
(* theLetterA 1 *)

// 1.12
let add newPos (cv :char * int) (word : int -> char * int) = 
    fun pos -> 
    if newPos = pos then cv
    else word pos
let theLettersAB = add 1 ('B', 3) theLetterA;;
(* theLettersAB 1 *)

// 1.13
let hello = 
    empty ('H', 4)
    |> add 1 ('E', 1)
    |> add 2 ('L', 1)
    |> add 3 ('L', 1)
    |> add 4 ('O', 1) 
(* theLettersHELLO 4 *)

// 1.14
let singleLetterScore (word : int -> char * int) pos = 
    let letter, pointValue = word pos
    pointValue
    

let doubleLetterScore (word : int -> char * int) pos = 
    let letter, pointValue = word pos
    pointValue * 2

let TripleLetterScore (word : int -> char * int) pos = 
    let letter, pointValue = word pos
    pointValue * 3

(* singleLetterScore theLettersHELLO 4
doubleLetterScore theLettersHELLO 4
TripleLetterScore theLettersHELLO 4 *)