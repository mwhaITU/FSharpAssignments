﻿module ImpParser

    open Eval
    open Types

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    // 7.1

    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"
    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"
    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"

    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    // 7.2

    let whitespaceChar = satisfy (fun c -> System.Char.IsWhiteSpace c) <?> "whitespace"
    let pletter        = satisfy (fun c -> System.Char.IsLetter c) <?> "letter"
    let palphanumeric  = satisfy (fun c -> System.Char.IsLetterOrDigit c) <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "spaces"
    let spaces1        = many1 whitespaceChar <?> "space1"

    // 7.3

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  =  p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2

    // 7.4

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'

    let curlybrackets p = pchar '{' >*>. p .>*> pchar '}'

    // 7.5

    let pid = 
        (pchar '_' <|> pletter) .>>. many (pchar '_' <|> palphanumeric) |>> fun (x,y) -> (x::y) |> System.String.Concat
    
    // 7.6

    let unop (a : Parser<'a>) (p1 : Parser<'b>) = a >*>. p1

    // 7.7
    
    let binop a p1 p2 = p1 .>*> a .>*>. p2

    // 7.8

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CTermParse, ctref = createParserForwardedToRef<cExp>()
    let CProdParse, cpref = createParserForwardedToRef<cExp>()
    let CAtomParse, caref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NegationParse = unop (pchar '-') AtomParse |>> (fun x -> N -1, x) |>> Mul <?> "Negation"
    let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
    let VParse = pid |>> V <?> "String"
    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let CharToIntParse = unop pCharToInt CTermParse |>> CharToInt <?> "charToInt"
    do aref := choice [CharToIntParse; NegationParse; NParse; PVParse; VParse; ParParse]

    let AexpParse = TermParse      

    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "C"
    let CParParse = parenthesise CTermParse
    let CVParse = unop pCharValue AtomParse |>> CV <?> "CV"
    let ToUpperParse = unop pToUpper CTermParse |>> ToUpper <?> "toUpper"
    let ToLowerParse = unop pToLower CTermParse |>> ToLower <?> "toLower"
    let IntToCharParse = unop pIntToChar AtomParse |>> IntToChar <?> "intToChar"
    do ctref := choice [ToLowerParse; ToUpperParse; IntToCharParse; CParse; CVParse; CParParse]

    let CexpParse = CTermParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"


    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    let mkBoard (bp : boardProg) : board = failwith "not implemented"

