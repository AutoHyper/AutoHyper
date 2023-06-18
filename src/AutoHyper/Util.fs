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

module Util 
#nowarn "59"

open System
open System.Collections.Generic

/// If set to true, we raise exceptions
let DEBUG = false

/// Given a number n, computes all lists of booleans of length n 
let rec computeBooleanPowerSet n =
    if n = 0 then
        Seq.singleton []
    else
        let r = computeBooleanPowerSet (n-1)
        Seq.append (Seq.map (fun x -> true::x) r) (Seq.map (fun x -> false::x) r)

/// Compute the cartesian product of a list of sets
let rec cartesianProduct (LL: list<seq<'a>>) =
    match LL with
    | [] -> Seq.singleton []
    | L :: Ls ->
        seq {
            for x in L do
                for xs in cartesianProduct Ls -> x :: xs
        }

/// Compute the powerset of a given set
let powerset (s : Set<'a>) =
    let asList = Set.toList s 

    let rec computeFiniteChoices (A : list<'a>) =
        match A with
            | [] -> Seq.singleton Set.empty
            | x::xs ->
                let r = computeFiniteChoices xs
                Seq.append r (Seq.map (fun y -> Set.add x y) r)

    computeFiniteChoices asList

/// Given a map A -> set<B> compute all possible maps A -> B that are obtained by picking some element from that set for each key in A
let cartesianProductMap (m : Map<'A, Set<'B>>) =
    let keysAsList = Seq.toList m.Keys

    keysAsList
    |> Seq.toList
    |> List.map (fun x -> m.[x] |> seq)
    |> cartesianProduct
    |> Seq.map (fun x -> 
        List.zip keysAsList x
        |> Map
        )

let rec combineStringsWithSeperator (s: String) (l: list<String>) = 
    match l with 
    | [] -> ""
    | [x] -> x
    | x::y::xs -> 
        x + s + combineStringsWithSeperator s (y::xs)

let dictToMap (d : Dictionary<'A, 'B>) = 
    d 
    |> Seq.map (fun x -> x.Key, x.Value)
    |> Map.ofSeq

/// Parser for variables used in HyperLTL specifications
module ParserUtil = 
    open FParsec

    /// Parser that parses everything between two '"'
    let escapedStringParser : Parser<string, unit> = 
        skipChar '\"' >>. manyChars (satisfy (fun c -> c <> '\"')) .>> skipChar '\"'    

    // A parser for the LTL atoms in programs
    let relVarParserBit : Parser<(String * int), unit>= 
        pstring "{" >>. 
            pipe3
                (spaces >>. many1Chars letter)
                (pchar '_')
                (pint32 .>> pstring "}")
                (fun x _ y  -> (x, y))


module SubprocessUtil = 
    type SubprocessResult = 
        {
            Stdout : String 
            Stderr : String 
            ExitCode : int
        }

    let executeSubprocess (cmd: string) (arg: string) = 
        let psi =
            System.Diagnostics.ProcessStartInfo(cmd, arg)

        psi.UseShellExecute <- false
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.CreateNoWindow <- true
        let p = System.Diagnostics.Process.Start(psi)
        let output = System.Text.StringBuilder()
        let error = System.Text.StringBuilder()
        p.OutputDataReceived.Add(fun args -> output.Append(args.Data) |> ignore)
        p.ErrorDataReceived.Add(fun args -> error.Append(args.Data) |> ignore)
        p.BeginErrorReadLine()
        p.BeginOutputReadLine()
        p.WaitForExit()

        {
            SubprocessResult.Stdout = output.ToString();
            Stderr = error.ToString()
            ExitCode = p.ExitCode
        }

      