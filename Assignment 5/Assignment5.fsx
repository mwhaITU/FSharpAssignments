// 5.1
let sum m n = 
    let rec sumA m n acc = 
        match n with
        | 0 -> acc + m + n
        | n -> sumA m (n - 1) (acc + m + n)
    sumA m n 0

sum 100 10

// 5.2
let length (lst : 'a list) = 
    let rec lengthA (lst : 'a list) acc =
        match lst with
        | [] -> acc
        | x :: xs -> lengthA xs (acc + 1)
    lengthA lst 0

length [0; 1; 2; 3; 4; 5]

// 5.3
let foldBack folder lst acc =
    let rec foldbackC lst c =
        match lst with
        | [] -> c acc
        | x :: xs -> foldbackC xs (fun r -> c (folder x r))
    foldbackC lst id

foldBack (fun x r -> x + r) [1; 2; 3] 0

// 5.4
let factC x =
    let rec factC2 x c =
        match x with
        | 0 -> c 1
        | x -> factC2 (x - 1) (fun r -> c (x * r))
    factC2 x id

factC 5

// Yellow Exercises

// 5.5

let fibW x =
 let mutable res1 = 0
 let mutable res2 = 1
 let mutable i = 1
 while (i <= x) do
    let temp = res1
    res1 <- res2
    res2 <- temp + res2
    i <- i + 1
 res1

fibW 10

let fibA (input : int) = 
    let rec fibA2 (input : int) (acc1 : int) (acc2: int) =
        match input with
        | 0 -> 0
        | 1 -> acc2
        | input -> fibA2(input - 1) (acc2) (acc1 + acc2)
    fibA2 input 0 1

fibA 10

let fibC (input : int) = 
    let rec fibC2 (input : int) (c : (int -> int)) =
        match input with
        | 0 -> 0
        | 1 -> c 1
        | input -> fibC2 (input - 1) (fun r -> c (r + fibC2 (input - 2) (fun r -> r)))
    fibC2 input id

fibC 10

let stopWatch = System.Diagnostics.Stopwatch.StartNew()
stopWatch.Start()
fibW 10
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
// 0.026100

stopWatch.Reset()
stopWatch.Start()
fibA 10
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
// 0.000500

stopWatch.Reset()
stopWatch.Start()
fibC 10
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
// 0.142800

// 5.6
let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> 1 :: c res) (n - 1)

(* Is the reason for stack overflow because it creates a new list
every time it calls the continuation function recursively?
TA: Essentially, yes. It happens because we call the
continuation function with the result and not the concatenation
of 1 with the result. Fix = c (1 :: res) Fixes problem with
stack frames. *)

// Red Exercises

// 5.7
type word = (char * int) list
type aExp =
    | N of int (* Integer literal *)
    | V of string (* Variable reference *)
    | WL (* Word length *)
    | PV of aExp (* Point value lookup at word index *)
    | Add of aExp * aExp (* Addition *)
    | Sub of aExp * aExp (* Subtraction *)
    | Mul of aExp * aExp (* Multiplication *)
    | CharToInt of cExp (* NEW: Cast to integer *)
and cExp =
    | C of char (* Character literal *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp (* Convert character to upper case *)
    | ToLower of cExp (* Convert character to lower case *)
    | IntToChar of aExp (* NEW: Cast to character *)

type state = Map<string, int>

let rec arithEvalSimple (a : aExp) (w : word) (s : state) =
    match a with
    | N n -> n
    | V v -> 
        match Map.tryFind v s with 
        | None -> 0
        | Some x -> x
    | WL -> w.Length
    | PV a -> snd w.[arithEvalSimple a w s]
    | Add(a, b) ->
        arithEvalSimple a w s + arithEvalSimple b w s
    | Sub(a, b) ->
        arithEvalSimple a w s - arithEvalSimple b w s
    | Mul(a, b) ->
        arithEvalSimple a w s * arithEvalSimple b w s
    | CharToInt c -> int (charEvalSimple c w s)
    
and charEvalSimple (c : cExp) (w : word) (s : state) =
    match c with
    | C c -> c
    | CV c -> fst w.[arithEvalSimple c w s]
    | ToUpper c -> System.Char.ToUpper(charEvalSimple c w s)
    | ToLower c -> System.Char.ToLower(charEvalSimple c w s)
    | IntToChar a -> char (arithEvalSimple a w s)

// 5.8
let rec arithEvalTail (a : aExp) (w : word) (s : state) (ct) =
    match a with
    | N n -> ct n
    | V v ->
        match Map.tryFind v s with
        | None -> ct 0
        | Some x -> ct x
    | WL -> ct w.Length
    | PV a -> arithEvalTail a w s (fun v -> ct (snd w.[v]))
    | Add(a, b) -> arithEvalTail a w s (fun v1 -> arithEvalTail b w s (fun v2 -> ct (v1 + v2)))
    | Sub(a, b) -> arithEvalTail a w s (fun v1 -> arithEvalTail b w s (fun v2 -> ct (v1 - v2)))
    | Mul(a, b) -> arithEvalTail a w s (fun v1 -> arithEvalTail b w s (fun v2 -> ct (v1 * v2)))
    | CharToInt c -> charEvalTail c w s (fun v -> ct (int (v)))

and charEvalTail (c : cExp) (w : word) (s : state) (ct) =
    match c with
    | C c -> ct c
    | CV a -> arithEvalTail a w s (fun v -> ct (fst w.[v]))
    | ToUpper c -> charEvalTail c w s (fun v -> ct (System.Char.ToUpper(v)))
    | ToLower c -> charEvalTail c w s (fun v -> ct (System.Char.ToLower(v)))
    | IntToChar a -> arithEvalTail a w s (fun v -> ct (char (v)))

let arithEval a w s = arithEvalTail a w s id
let charEval c w s = charEvalTail c w s id