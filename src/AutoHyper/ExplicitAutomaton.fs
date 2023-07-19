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

module ExplicitAutomaton

open System 
open System.IO

open Util
open RunConfiguration
open FsOmegaLib.SAT
open FsOmegaLib.NBA

open FsOmegaLib.Operations

type ExplicitNBA<'T, 'L when 'T: comparison> = 
    {
        States : Set<'T>
        InitialState : 'T
        Alphabet: list<'L>
        Edges: Map<'T, list<int * 'T>>
        AcceptingStates : Set<'T>
    }

module ExplicitNBA = 
    let convertStatesToInt (nba : ExplicitNBA<'T, 'L>) = 
        let idDict = 
            nba.States
            |> Seq.mapi (fun i x -> x, i)
            |> Map.ofSeq

        {
            ExplicitNBA.States = 
                nba.States |> Set.map (fun x -> idDict.[x]);

            InitialState = idDict.[nba.InitialState]

            Alphabet = nba.Alphabet;

            Edges = 
                nba.Edges 
                |> Map.toSeq
                |> Seq.map 
                    (fun (k, v) -> 
                        idDict.[k], List.map (fun (g, s) -> g, idDict.[s]) v
                    ) 
                |> Map.ofSeq;

            AcceptingStates = 
                nba.AcceptingStates |> Set.map (fun x -> idDict.[x]);
        }

    let toBAString (stateStringer : 'T -> String) (alphStringer : 'L -> String) (nba : ExplicitNBA<'T, 'L>) = 
        let s = new StringWriter() 

        s.WriteLine (stateStringer(nba.InitialState))

        for n in nba.States do 
            for (l, n') in nba.Edges.[n] do 
                s.WriteLine (alphStringer (nba.Alphabet.[l]) + "," + stateStringer(n) + "->" + stateStringer(n'))

        for n in nba.AcceptingStates do 
            s.WriteLine (stateStringer(n))

        s.ToString()

    let convertFromSymbolicNBA (nba : NBA<'T, 'L>) = 
        let alphabet = 
            Util.computeBooleanPowerSet (nba.APs.Length)
            |> Seq.toList

        let idDict = 
            nba.States
            |> Seq.mapi (fun i x -> x, i + 1)
            |> Map.ofSeq

        let newStates = set [0..nba.States.Count]
        
        let newEdges = 
            nba.Skeleton.Edges 
            |> Map.toSeq
            |> Seq.map (fun (k, v) -> 
                let sucs = 
                    v 
                    |> List.map (fun (g, t) -> 
                        [0..alphabet.Length - 1]
                        |> List.choose (fun i -> 
                            if DNF.eval (fun j -> alphabet.[i].[j]) g then 
                                Some (i, idDict.[t])
                            else 
                                None
                            )
                    )
                    |> List.concat
                
                idDict.[k], sucs
                )
            |> Map.ofSeq

        let initSucs = 
            nba.InitialStates
            |> Set.toList
            |> List.map (fun s -> 
                nba.Edges.[s]
                |> List.map (fun (g, t) -> 
                    [0..alphabet.Length - 1]
                    |> List.choose (fun i -> 
                        if DNF.eval (fun j -> alphabet.[i].[j]) g then 
                            Some (i, idDict.[t])
                        else 
                            None
                        )
                )
                |> List.concat
            )
            |> List.concat
            
        let newEdges = 
            newEdges
            |> Map.add 0 initSucs

        let newAcceptingStates = 
            nba.AcceptingStates
            |> Set.map (fun x -> idDict.[x])
            |> Set.add 0

        {
            ExplicitNBA.States = newStates
            InitialState = 0
            Edges = newEdges
            Alphabet = alphabet
            AcceptingStates = newAcceptingStates
        }

module AutomataChecks = 

    exception private AutomatonCheckException of FsOmegaLibError

    let checkNBAContainmentBait debug mainPath baitPath (enba1 : ExplicitNBA<int, 'L>) (enba2 : ExplicitNBA<int, 'L>)  = 
        try 
            assert(enba1.Alphabet = enba2.Alphabet)

            let alphMapper = 
                enba1.Alphabet
                |> List.mapi (fun i x -> x, "l" + string i)
                |> Map.ofList

            let s1 = ExplicitNBA.toBAString string (fun x -> alphMapper.[x]) enba1
            let s2 = ExplicitNBA.toBAString string (fun x -> alphMapper.[x]) enba2

            let path1 = Path.Combine [|mainPath; "aut1.ba"|]
            let path2 = Path.Combine [|mainPath; "aut2.ba"|]

            File.WriteAllText(path1, s1)
            File.WriteAllText(path2, s2)

            let arg = "-jar " + baitPath + " -a " + path1 + " -b " + path2
            let res = Util.SubprocessUtil.executeSubprocess "java" arg

            match res with 
            | {ExitCode = 0; Stderr = ""; Stdout = c} | {ExitCode = 1; Stderr = ""; Stdout = c} -> 
                if c.Contains "Inclusion holds: true" then 
                    FsOmegaLib.Operations.AutomataOperationResult.Success true
                elif c.Contains "Inclusion holds: false" then 
                    FsOmegaLib.Operations.AutomataOperationResult.Success false
                else 
                    FsOmegaLib.Operations.AutomataOperationResult.Fail {Info = $"Error by BAIT"; DebugInfo = $"Unexpected output by BAIT; (containment); %s{c}"}
            | {ExitCode = exitCode; Stderr = stderr}  -> 
                if exitCode <> 0 && exitCode <> 1 then 
                    raise <| AutomatonCheckException {Info = $"Unexpected exit code by BAIT"; DebugInfo = $"Unexpected exit code by BAIT; (containsment); %i{exitCode}"}
                else   
                    raise <| AutomatonCheckException {Info = $"Error by BAIT"; DebugInfo = $"Error by BAIT; (containment); %s{stderr}"}

        with 
        | _ when debug -> reraise() 
        | AutomatonCheckException err -> 
            Fail (err)
        | e -> 
            Fail {Info = $"Unexpected error"; DebugInfo = $"Unexpected error; (BAIT, containment); %s{e.Message}"}

    let checkNBAContainmentRabit (debug: bool) mainPath rabitPath (enba1 : ExplicitNBA<'T, 'L>) (enba2 : ExplicitNBA<'T, 'L>)  = 
        try
            assert(enba1.Alphabet = enba2.Alphabet)

            let alphMapper = 
                enba1.Alphabet
                |> List.mapi (fun i x -> x, "l" + string i)
                |> Map.ofList

            let s1 = ExplicitNBA.toBAString string (fun x -> alphMapper.[x]) enba1
            let s2 = ExplicitNBA.toBAString string (fun x -> alphMapper.[x]) enba2

            let path1 = Path.Combine [|mainPath; "aut1.ba"|]
            let path2 = Path.Combine [|mainPath; "aut2.ba"|]

            File.WriteAllText(path1, s1)
            File.WriteAllText(path2, s2)

            let arg = "-jar " + rabitPath + " " + path1 + " " + path2 + " -fast"

            let res = Util.SubprocessUtil.executeSubprocess "java" arg

            match res with 
            | {ExitCode = 0; Stderr = ""; Stdout = c} | {ExitCode = 1; Stderr = ""; Stdout = c} -> 
                if c.Contains "Not included." then 
                    FsOmegaLib.Operations.AutomataOperationResult.Success false
                elif c.Contains "Included." then 
                    FsOmegaLib.Operations.AutomataOperationResult.Success true
                else 
                    FsOmegaLib.Operations.AutomataOperationResult.Fail {Info = $"Error by RABIT"; DebugInfo = $"Unexpected output by RABIT; (containment); %s{c}"}
            | {ExitCode = exitCode; Stderr = stderr}  -> 
                if exitCode <> 0 && exitCode <> 1 then 
                    raise <| AutomatonCheckException {Info = $"Unexpected exit code by RABIT"; DebugInfo = $"Unexpected exit code by RABIT;  (containsment); %i{exitCode}"}
                else   
                    raise <| AutomatonCheckException {Info = $"Error by RABIT"; DebugInfo = $"Error by RABIT; (containment); %s{stderr}"}

        with 
        | _ when debug -> reraise() 
        | AutomatonCheckException err -> 
            Fail (err)
        | e -> 
            Fail {Info = $"Unexpected error"; DebugInfo = $"Unexpected error; (RABIT, containment); %s{e.Message}"}

    let checkNBAContainmentForklift debug mainPath forkliftPath (enba1 : ExplicitNBA<'T, 'L>) (enba2 : ExplicitNBA<'T, 'L>)  = 
        try 
            assert(enba1.Alphabet = enba2.Alphabet)

            let alphMapper = 
                enba1.Alphabet
                |> List.mapi (fun i x -> x, "l" + string i)
                |> Map.ofList

            let s1 = ExplicitNBA.toBAString string (fun x -> alphMapper.[x]) enba1
            let s2 = ExplicitNBA.toBAString string (fun x -> alphMapper.[x]) enba2

            let path1 = Path.Combine [|mainPath; "aut1.ba"|]
            let path2 = Path.Combine [|mainPath; "aut2.ba"|]

            File.WriteAllText(path1, s1)
            File.WriteAllText(path2, s2)

            let arg = "-jar " + forkliftPath + " " + path1 + " " + path2
            let res = Util.SubprocessUtil.executeSubprocess "java" arg

            match res with 
            | {ExitCode = 0; Stderr = ""; Stdout = c} | {ExitCode = 1; Stderr = ""; Stdout = c} -> 
                if c.Contains "OUTPUT:false" then 
                    FsOmegaLib.Operations.AutomataOperationResult.Success false
                elif c.Contains "OUTPUT:true" then 
                    FsOmegaLib.Operations.AutomataOperationResult.Success true
                else 
                    FsOmegaLib.Operations.AutomataOperationResult.Fail {Info = $"Error by FORKLIFT"; DebugInfo = $"Unexpected output by FORKLIFT; (containment); %s{c}"}
            | {ExitCode = exitCode; Stderr = stderr}  -> 
                if exitCode <> 0 && exitCode <> 1 then 
                    raise <| AutomatonCheckException {Info = $"Unexpected exit code by FORKLIFT"; DebugInfo = $"Unexpected exit code by FORKLIFT;  (containsment); %i{exitCode}"}
                else   
                    raise <| AutomatonCheckException {Info = $"Error by FORKLIFT"; DebugInfo = $"Error by FORKLIFT; (containment); %s{stderr}"}
   
        with 
        | _ when debug -> reraise() 
        | AutomatonCheckException err -> 
            Fail (err)
        | e -> 
            Fail {Info = $"Unexpected error"; DebugInfo = $"Unexpected error; (FORKLIFT, containment); %s{e.Message}"}