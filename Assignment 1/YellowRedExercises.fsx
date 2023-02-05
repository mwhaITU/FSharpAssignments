// 1.8
let timediff (t1 : int * int) (t2 : int * int) = 
    let (t1hour, t1min) = t1
    let (t2hour, t2min) = t2
    let t1InMinutes = t1hour * 60 + t1min
    let t2InMinutes = t2hour * 60 + t2min
    let difference = t2InMinutes - t1InMinutes
    difference
timediff (12, 34) (11, 35);;

// 1.9
let minutes (t1 : int * int) = 
    let difference = timediff (00, 00) t1
    difference
minutes (14, 24)

// 1.10
let curry f a b = f(a,b)
curry (fun (x, y) -> x + y) 5 3;;

let uncurry f (a,b) = f a b
uncurry (fun x y -> x + y) (5, 3);;

// 1.11
let empty (letter, pointValue) = fun pos -> letter, pointValue
let theLetterA : int -> char * int = empty ('A', 1)
theLetterA 1

// 1.12
let add newPos (cv :char * int) (word : int -> char * int) = 
    fun pos -> 
    if newPos = pos then cv
    else word pos
let theLettersAB = add 1 ('B', 3) theLetterA;;
theLettersAB 1

// 1.13
let theLettersHELLO = 
    empty ('H', 4)
    |> add 1 ('E', 1)
    |> add 2 ('L', 1)
    |> add 3 ('L', 1)
    |> add 4 ('O', 1) 
theLettersHELLO 4

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

singleLetterScore theLettersHELLO 4
doubleLetterScore theLettersHELLO 4
TripleLetterScore theLettersHELLO 4