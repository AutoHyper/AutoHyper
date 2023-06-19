(*    
    Copyright (C) 2022-2023 Raven Beutner

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

module HyperLTL 

open FsOmegaLib.LTL

open System
open System.IO

type TraceQuantifierType = 
    | FORALL 
    | EXISTS

    member this.Flip = 
        match this with 
            | FORALL -> EXISTS
            | EXISTS -> FORALL

type HyperLTL<'L when 'L: comparison> = 
    {
        QuantifierPrefix : list<TraceQuantifierType>
        LTLMatrix : LTL<'L * int>
    }

module HyperLTL = 
    let isConsistent (hyperltl : HyperLTL<'L>) =
        hyperltl.LTLMatrix
        |> LTL.allAtoms
        |> Set.forall (fun (_, i) -> i <= hyperltl.QuantifierPrefix.Length)
            
    let print (varNames : 'L -> String) (hyperltl : HyperLTL<'L>) =
        let strWriter = new StringWriter()
        for t in hyperltl.QuantifierPrefix do
            match t with 
                | FORALL -> strWriter.Write("forall ")
                | EXISTS -> strWriter.Write("exists ")

        let varStringer (x, i) = "\"" + varNames x + "\"_" + string(i) 

        strWriter.Write(LTL.printInSpotFormat varStringer hyperltl.LTLMatrix)

        strWriter.ToString()

type NamedHyperLTL<'L when 'L: comparison> = 
    {
        QuantifierPrefix : list<TraceQuantifierType * String>
        LTLMatrix : LTL<'L * String>
    }

module NamedHyperLTL =
    let toHyperLTL (nhyperltl : NamedHyperLTL<'L>) = 
        let names = nhyperltl.QuantifierPrefix |> List.map snd

        let nameMap = 
            names 
            |> List.mapi (fun i n -> n, i)
            |> Map.ofList

        {
            HyperLTL.QuantifierPrefix = nhyperltl.QuantifierPrefix |> List.map fst 
            LTLMatrix =
                nhyperltl.LTLMatrix
                |> LTL.map (fun (x, n) -> (x, nameMap.[n]))
        }

    let print (varNames : 'L -> String) (nhyperltl : NamedHyperLTL<'L>)  =
        let strWriter = new StringWriter()
        for t in nhyperltl.QuantifierPrefix do
            match t with 
                | FORALL, x -> strWriter.Write("forall " + x + ".")
                | EXISTS, x -> strWriter.Write("exists " + x + ".")

        let varStringer (x, i) = "\"" + varNames x + "\"_" + i

        strWriter.Write(LTL.printInSpotFormat varStringer nhyperltl.LTLMatrix)

        strWriter.ToString()

let extractBlocks (qf : list<TraceQuantifierType>) = 
    let rec helper t count q = 
        match q with 
            | [] -> [count]
            | x::xs -> 
                if x = t then 
                    helper t (count + 1) xs
                else 
                    count::helper x 1 xs

    helper qf.[0] 0 qf





module SymbolicHyperLTL = 
    open TransitionSystemLib.SymbolicSystem

    type RelationalAtom = 
        | UnaryPred of Expression * int
        | RelationalEq of Expression * int * Expression * int

    type SymbolicHyperLTL = 
        {
            QuantifierPrefix : list<TraceQuantifierType>
            LTLMatrix : LTL<RelationalAtom>
        }

    type NamedRelationalAtom = 
        | NamedUnaryPred of Expression * String
        | NamedRelationalEq of Expression * String * Expression * String


    type NamedSymbolicHyperLTL = 
        {
            QuantifierPrefix : list<TraceQuantifierType * String>
            LTLMatrix : LTL<NamedRelationalAtom>
        }

    module NamedSymbolicHyperLTL = 

        let toSymbolicHyperLTL (f : NamedSymbolicHyperLTL) = 
            let names = f.QuantifierPrefix |> List.map snd

            let nameMap = 
                names 
                |> List.mapi (fun i n -> n, i)
                |> Map.ofList

            {
                SymbolicHyperLTL.QuantifierPrefix = f.QuantifierPrefix |> List.map fst 
                LTLMatrix = 
                    f.LTLMatrix
                    |> LTL.map (fun x -> 
                        match x with 
                            | NamedUnaryPred (e, n) -> 
                                UnaryPred(e, nameMap.[n])
                            | NamedRelationalEq (e1, n1, e2, n2) -> 
                                RelationalEq (e1, nameMap.[n1], e2, nameMap.[n2])
                    )
            }
            
module Parser =
    open FParsec

    let private keywords = 
        [
            "X"
            "G"
            "F"
            "U"
            "W"
            "R"
        ]

    let traceVariableParser = 
        attempt (
            pipe2
                letter
                (manyChars (letter <|> digit))
                //(manyChars letter)
                (fun x y -> string(x) + y)
            >>= fun s ->
                if List.contains s keywords then
                    fail ""
                else preturn s
            )

    let namedPrefixParser = 
        let eParser = 
            skipString "exists " >>. spaces >>. traceVariableParser .>> spaces .>> pchar '.'
            |>> fun x -> EXISTS, x

        let uParser = 
            skipString "forall " >>. spaces >>. traceVariableParser .>> spaces .>> pchar '.'
            |>> fun x -> FORALL, x
        
        spaces >>.
        many1 ((eParser <|> uParser) .>> spaces)

    let private namedHyperltlParser (atomParser : Parser<'T, unit>) = 
        let ap : Parser<'T * String, unit>= 
            atomParser .>> pchar '_' .>>. traceVariableParser

        pipe2 
            namedPrefixParser
            (FsOmegaLib.LTL.Parser.ltlParser ap)
            (fun x y -> {NamedHyperLTL.QuantifierPrefix = x; LTLMatrix = y})

    let parseNamedHyperLTL (atomParser : Parser<'T, unit>) s =    
        let full = namedHyperltlParser atomParser .>> spaces .>> eof
        let res = run full s
        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err
        
    // ####################################################################
    // Parsing for NuSMV HyperLTL HyperLTL properties 

    let private namedSymbolicHyperltlParser = 
        let indexedExpressionParser =   
            tuple2
                (skipChar '{' >>. spaces >>. TransitionSystemLib.SymbolicSystem.Parser.expressionParser .>> spaces .>> skipChar '}')
                (spaces >>. skipChar '_' >>. spaces >>. traceVariableParser) 

        let unaryAtomParser = 
            indexedExpressionParser
            |>> SymbolicHyperLTL.NamedUnaryPred

        let relationalAtomParser = 
            pipe2
                (indexedExpressionParser .>> spaces .>> skipChar '=')
                (spaces >>. indexedExpressionParser)
                (fun (e1, pi1) (e2, pi2) -> SymbolicHyperLTL.NamedRelationalEq(e1, pi1, e2, pi2))

        let atomParser = 
            attempt(relationalAtomParser) <|> unaryAtomParser

        pipe2 
            namedPrefixParser
            (FsOmegaLib.LTL.Parser.ltlParser atomParser)
            (fun x y -> {SymbolicHyperLTL.NamedSymbolicHyperLTL.QuantifierPrefix = x; SymbolicHyperLTL.NamedSymbolicHyperLTL.LTLMatrix = y})

    let parseNamedSymbolicHyperltl s =    
        let full = namedSymbolicHyperltlParser .>> spaces .>> eof
        let res = run full s
        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err

    // ####################################################################
    // Parsing for Boolean Program HyperLTL properties 

    let private relVarParserBit : Parser<(String * int), unit>= 
        pstring "{" >>. 
            pipe3
                (spaces >>. many1Chars letter)
                (pchar '_')
                (pint32 .>> pstring "}")
                (fun x _ y  -> (x, y))

    let parseBooleanProgramNamedHyperLTL s =    
        let full = namedHyperltlParser relVarParserBit .>> spaces .>> eof
        let res = run full s
        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err
        