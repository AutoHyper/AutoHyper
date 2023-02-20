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

module Parser =
    open FParsec

    let namedPrefixParser = 
        let eParser = 
            skipString "exists " >>. spaces >>. (many1Chars letter) .>> spaces .>> pchar '.'
            |>> fun x -> EXISTS, x

        let uParser = 
            skipString "forall " >>. spaces >>. (many1Chars letter) .>> spaces .>> pchar '.'
            |>> fun x -> FORALL, x
        
        spaces >>.
        many1 ((eParser <|> uParser) .>> spaces)

    let private namedHyperltlParser (atomParser : Parser<'T, unit>) = 
        let ap : Parser<'T * String, unit>= 
            atomParser .>> pchar '_' .>>. (many1Chars letter)

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