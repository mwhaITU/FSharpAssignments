// Green Exercises

// 3.1
type aExp =
 | N of int // Integer value
 | V of string // Variable
 | WL // Length of the word
 | PV of aExp // Point value of character at specific word index
 | Add of aExp * aExp // Addition
 | Sub of aExp * aExp // Subtraction
 | Mul of aExp * aExp // Multiplication
let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let a1 = N 42;;
let a2 = N 4 .+. (N 5 .-. N 6);;
let a3 = N 4 .*. N 2 .+. N 34;;
let a4 = (N 4 .+. N 2) .*. N 34;;
let a5 = N 4 .+. (N 2 .*. N 34);;

let rec arithEvalSimple (a : aExp) =
    match a with
    | N n -> n
    | Add(a, b) ->
        let va = arithEvalSimple a
        let vb = arithEvalSimple b
        va + vb
    | Sub(a, b) ->
        let va = arithEvalSimple a
        let vb = arithEvalSimple b
        va - vb
    | Mul(a, b) ->
        let va = arithEvalSimple a
        let vb = arithEvalSimple b
        va * vb

arithEvalSimple a5

// 3.2
let a6 = V "x";;
let a7 = N 4 .+. (V "y" .-. V "z");;

type state = Map<string, int>

let rec arithEvalState (a : aExp) (s : state) =
    match a with
    | N n -> n
    | V v -> 
        match s.TryFind v with 
        | None -> 0
        | Some x -> x
    | Add(a, b) ->
        let va = arithEvalState a s
        let vb = arithEvalState b s
        va + vb
    | Sub(a, b) ->
        let va = arithEvalState a s
        let vb = arithEvalState b s
        va - vb
    | Mul(a, b) ->
        let va = arithEvalState a s
        let vb = arithEvalState b s
        va * vb

arithEvalState a7 (Map.ofList [("y", 4); ("z", 5)])

// 3.3
let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

type word = (char * int) list

let rec arithEval (a : aExp) (w : word) (s : state) =
    match a with
    | N n -> n
    | V v -> 
        match s.TryFind v with 
        | None -> 0
        | Some x -> x
    | WL -> w.Length
    | PV a -> snd w.[arithEval a w s]
    | Add(a, b) ->
        arithEval a w s + arithEval b w s
    | Sub(a, b) ->
        arithEval a w s - arithEval b w s
    | Mul(a, b) ->
        arithEval a w s * arithEval b w s


let hello : word = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1);]

arithEval WL [] Map.empty
arithEval WL hello Map.empty
arithEval (PV (N 0)) hello Map.empty
arithEval arithTripleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)])

// 3.4
type cExp =
| C of char (* Character value *)
| ToUpper of cExp (* Converts lower case to upper case character,
non-letters are unchanged *)
| ToLower of cExp (* Converts upper case to lower case character,
non-letters are unchanged *)
| CV of aExp (* Character lookup at word index *)

let rec charEval (c : cExp) (w : word) (s : state) =
    match c with
    | C c -> c
    | ToUpper c -> System.Char.ToUpper(charEval c w s)
    | ToLower c -> System.Char.ToLower(charEval c w s)
    | CV c -> fst w.[arithEval c w s]

charEval (C 'H') [] Map.empty
charEval (CV (V "x" .-. N 1)) hello (Map.ofList [("x", 5)])


// 3.5
type bExp = 
| TT (* true *)
| FF (* false *)
| AEq of aExp * aExp (* numeric equality *)
| ALt of aExp * aExp (* numeric less than *)
| Not of bExp (* boolean not *)
| Conj of bExp * bExp (* boolean conjunction *)
| IsDigit of cExp (* check for digit *)
| IsLetter of cExp (* check for letter *)
| IsVowel of cExp (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)
 
let (.=.) a b = AEq (a, b) 
let (.<.) a b = ALt (a, b) 
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let isVowel (c : char) =
    if "aeiouyæøå".Contains(System.Char.ToLower c) then true
    else false

let rec boolEval (b : bExp) (w : word) (s : state) =
    match b with
    | TT -> true
    | FF -> false
    | AEq(a, b) -> arithEval a w s = arithEval b w s
    | ALt(a, b) -> arithEval a w s < arithEval b w s
    | Not b when boolEval b w s = true -> false
    | Not b when boolEval b w s = false -> true
    | Conj(a, b) -> (boolEval a w s) && (boolEval b w s)
    | IsDigit c -> System.Char.IsDigit(charEval c w s)
    | IsLetter c -> System.Char.IsLetter(charEval c w s)
    | IsVowel c -> isVowel(charEval c w s)
    
boolEval TT [] Map.empty
boolEval ((V "x" .+. V "y") .=. (V "y" .-. V "x")) [] (Map.ofList [("x", 5); ("y", 7)])
boolEval (IsLetter (CV (V "x"))) hello (Map.ofList [("x", 4)])
boolEval (IsDigit (CV (V "x"))) hello (Map.ofList [("x", 4)])

// Yellow Exercises

// 3.6
let isConsonant (c : cExp) = 
    Not (IsVowel c)

boolEval (isConsonant (C 'H')) [] Map.empty
boolEval (isConsonant (CV (V "x"))) hello (Map.ofList [("x", 0)])
boolEval (isConsonant (CV (V "x"))) hello (Map.ofList [("x", 1)])

// 3.7 
type stmnt =
| Skip (* does nothing *)
| Ass of string * aExp (* variable assignment *)
| Seq of stmnt * stmnt (* sequential composition *)
| ITE of bExp * stmnt * stmnt (* if-then-else statement *) 
| While of bExp * stmnt (* while statement *)

let rec evalStmnt (stm : stmnt) (w : word) (s : state) =
    match stm with
    | Skip -> s
    | Ass (x, a) -> 
        let v = arithEval a w s
        let s' = s.Add (x, v)
        s'
    | Seq (stm1, stm2) -> 
        let s' = evalStmnt stm1 w s
        evalStmnt stm2 w s'
    | ITE (guard, stm1, stm2) ->
        let b = boolEval guard w s
        if b = true then evalStmnt stm1 w s
        else evalStmnt stm2 w s
    | While (guard, stm) -> 
        let b = boolEval guard w s
        if b = true then
            let s' = evalStmnt stm w s
            evalStmnt (While(guard, stm)) w s'
        else
            s

evalStmnt Skip [] Map.empty
evalStmnt (Seq (Ass ("x", WL), Ass ("y", N 7))) hello Map.empty
evalStmnt (ITE (WL .>=. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty
evalStmnt (While (V "x" .<=. WL,
 Seq (Ass ("y", V "y" .+. V "x"),
 Ass ("x", V "x" .+. N 1))))
 hello Map.empty;;
evalStmnt (While (V "x" .<=. WL,
 Seq (Ass ("y", V "y" .+. V "x"),
 Ass ("x", V "x" .+. N 1))))
 hello (Map.ofList [("x", 3); ("y", 100)])

// 3.8
type squareFun = word -> int -> int -> int

let stmntToSquareFun (stm : stmnt) =
    fun (w : word) (pos : int) (acc : int) -> 
        let state = evalStmnt stm w (Map.ofList [("_pos_", pos); ("_acc_", acc)])
        state.Item("_result_")

let singleLetterScore = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore))
let doubleLetterScore = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore))
let tripleLetterScore = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore))
let doubleWordScore = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore))
let tripleWordScore = stmntToSquareFun (Ass ("_result_", arithTripleWordScore))

let containsNumbers =
    stmntToSquareFun
        (Seq (Ass ("_result_", V "_acc_"),
        While (V "i" .<. WL,
        ITE (IsDigit (CV (V "i")),
        Seq (
        Ass ("_result_", V "_result_" .*. N -1),
        Ass ("i", WL)),
        Ass ("i", V "i" .+. N 1)))))

singleLetterScore hello 0 0
doubleLetterScore hello 0 0
tripleLetterScore hello 0 0
containsNumbers hello 5 50
containsNumbers (('0', 100)::hello) 5 50
containsNumbers (hello @ [('0', 100)]) 5 50

// Red Exercises

// 3.9
