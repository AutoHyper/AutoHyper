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

module TransitionSystem

open System.IO

type TransitionSystem<'T, 'L when 'T : comparison and 'L : comparison> = 
    {
        States : Set<'T>
        InitialStates : Set<'T>
        APs : list<'L>
        Edges : Map<'T, Set<'T>>
        ApEval : Map<'T, list<bool>>
    }

module TransitionSystem =

    let convertStatesToInt (ts : TransitionSystem<'T, 'L>) = 
        let idDict = 
            ts.States
            |> Set.toList
            |> List.mapi (fun i x -> x, i)
            |> Map.ofList

        {
            TransitionSystem.States =
                ts.States |> Set.map (fun x -> idDict.[x])

            InitialStates =
                ts.InitialStates |> Set.map (fun x -> idDict.[x])

            APs = ts.APs 

            Edges = 
                ts.Edges
                |> Map.toSeq
                |> Seq.map (fun (k, v) -> idDict.[k], Set.map (fun x -> idDict.[x]) v)
                |> Map.ofSeq

            ApEval = 
                ts.ApEval
                |> Map.toSeq
                |> Seq.map (fun (k, v) -> idDict.[k], v)
                |> Map.ofSeq
        }
        
    let mapAPs (f : 'L -> 'U) (ts : TransitionSystem<'T, 'L>) = 
        {
            States = ts.States
            InitialStates = ts.InitialStates
            APs = ts.APs |> List.map f
            Edges = ts.Edges
            ApEval = ts.ApEval
        }
       
    let isConsistent (ts : TransitionSystem<'T, 'L>) =
        (
            ts.States
            |> Seq.forall (fun x -> ts.Edges.ContainsKey x && ts.ApEval.ContainsKey x && ts.ApEval.[x].Length = ts.APs.Length)
        )
        &&
        (
            ts.InitialStates
            |> Seq.forall (fun x -> ts.States.Contains x)
        )
        &&
        (
            ts.Edges.Values
            |> Seq.forall (fun x ->
                x
                |> Seq.forall (fun z -> ts.States.Contains z)
            )
        )

    let print (stringer : 'T -> string) (stringerAp : 'L -> string) (ts : TransitionSystem<'T, 'L>) = 

        let strWriter = new StringWriter()
       
        strWriter.Write("aps ")
        for x in ts.APs do 
            strWriter.Write("\"" + stringerAp(x) + "\"" + " ")
        strWriter.Write('\n')

        strWriter.Write("init ")
        for x in ts.InitialStates do 
            strWriter.Write(stringer(x) + " ")
        strWriter.Write('\n')


        strWriter.Write("--BODY--\n")

        for s in ts.States do
            let sucs = ts.Edges.[s]
            let apEval = ts.ApEval.[s]

            strWriter.Write("State: " + stringer(s) + " ")
            strWriter.Write (
                "[" +
                (apEval
                |> List.map (fun x -> if x then "t" else "f")
                |> Util.combineStringsWithSeperator " ")
                + "]"
                )
            strWriter.Write('\n')

            for x in sucs do 
                strWriter.Write(stringer(x) + " ")

            strWriter.Write('\n')

        strWriter.ToString()

module Parser = 
    open FParsec 

    let private apsatParser = 
        let trueParser = 
            charReturn 't' true 
        let falseParser = 
            charReturn 'f' false 
        skipChar '[' >>. spaces >>. many ((trueParser <|> falseParser) .>> spaces)  .>> spaces .>> skipChar ']'

    let private stateParser = 
        pstring "State:" >>.
            pipe3
                (spaces >>. pint32)
                (spaces >>. apsatParser)
                (spaces >>. many (pint32 .>> spaces))
                (fun id ap sucs -> (id, (sucs, ap)))

    let private bodyParser = 
        spaces >>. many (stateParser .>> spaces)

    let private tsParser = 
        pipe3
            (spaces >>. skipString "aps" >>. spaces >>. many1 (Util.ParserUtil.escapedStringParser .>> spaces))
            (spaces >>. skipString "init" >>. spaces >>. many1 (pint32 .>> spaces))
            (spaces >>. skipString "--BODY--" >>. bodyParser)
            (fun aps init st -> 
                {
                    TransitionSystem.States = st |> List.map fst |> set
                    InitialStates = set init
                    APs = aps;
                    Edges = 
                        st 
                        |> List.map (fun (k, (a, _)) -> k, set a)
                        |> Map.ofList
                    ApEval = 
                        st 
                        |> List.map (fun (k, (_, b)) -> k, b)
                        |> Map.ofList
                }       
                )
    
    let parseTS (s: string) =
        let full = tsParser .>> spaces .>> eof
        let res = run full s
        match res with
            | Success (res, _, _) -> Result.Ok res
            | Failure (err, _, _) -> 
                Result.Error ("Transition System could not be parsed: " + err)