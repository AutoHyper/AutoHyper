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

module ModelChecking

open FsOmegaLib.LTL
open FsOmegaLib.NBA
open FsOmegaLib.Operations

open TransitionSystemLib.TransitionSystem

open Util
open RunConfiguration
open HyperLTL

let private swComplement = System.Diagnostics.Stopwatch()
let private swProduct = System.Diagnostics.Stopwatch()
let private swInclusion = System.Diagnostics.Stopwatch()
let private swEmptiness = System.Diagnostics.Stopwatch()
let private swLTLtoNBA = System.Diagnostics.Stopwatch()

type TimeSummary = 
    {
        LTL2NBATime : int; 
        ProductTime : int; 
        InclusionTime : int; 
        ComplementationTime : int; 
        EmptinessTime : int; 
        TotalTime : int 
    }

type InclusionChecker = 
    | SPOT 
    | RABIT
    | BAIT 
    | FORKLIFT

type Mode = 
    | COMP 
    | INCL of InclusionChecker

let private inclusionTest (config : Configuration)  (tslist : list<TransitionSystem<'L>>) (aut : NBA<int, 'L * int>) (inclusionChecker : InclusionChecker) = 

    // Map each index to the APs that are used by aut at that position
    let automatonAps = 
        aut.APs
        |> List.groupBy snd
        |> List.map (fun (i, l) -> i, List.map fst l)
        |> Map.ofList

    // Make sure that the automaton only references the position at most |tslist| - 1
    assert(automatonAps.Keys |> Seq.forall (fun x -> x < tslist.Length))
    assert(
        automatonAps 
        |> Map.toSeq
        |> Seq.forall (fun (i, aps) -> aps |> List.forall (fun x -> List.contains x tslist.[i].APs))
    )

    let product = 
        ProductConstruction.constructSelfCompositionAutomaton tslist ([0..tslist.Length - 1] |> List.map (fun x -> if automatonAps.ContainsKey x then automatonAps.[x] else [] ))
        |> NBA.convertStatesToInt

    swInclusion.Start()
    let res = 
        match inclusionChecker with 
        | SPOT -> 
            if config.SolverConfig.AutfiltPath |> Option.isNone then 
                raise <| AnalysisException "Required spot's autfilt for inclusion check, but autfilt is not given"
            FsOmegaLib.Operations.AutomataChecks.isContained Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.AutfiltPath.Value product aut
            
        | RABIT -> 
            let nba1, nba2 = NBA.bringPairToSameAPs product aut
            let enba1 = ExplicitAutomaton.ExplicitNBA.convertFromSymbolicNBA nba1
            let enba2 = ExplicitAutomaton.ExplicitNBA.convertFromSymbolicNBA nba2

            if config.SolverConfig.RabitJarPath |> Option.isNone then 
                raise <| AnalysisException "Required RABIT for inclusion check, but no path to RABIT is given"
            ExplicitAutomaton.AutomataChecks.checkNBAContainmentRabit Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.RabitJarPath.Value enba1 enba2
        | BAIT -> 
            let nba1, nba2 = NBA.bringPairToSameAPs product aut
            let enba1 = ExplicitAutomaton.ExplicitNBA.convertFromSymbolicNBA nba1
            let enba2 = ExplicitAutomaton.ExplicitNBA.convertFromSymbolicNBA nba2

            if config.SolverConfig.BaitJarPath |> Option.isNone then 
                raise <| AnalysisException "Required BAIT for inclusion check, but no path to BAIT is given"
            ExplicitAutomaton.AutomataChecks.checkNBAContainmentBait Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.BaitJarPath.Value enba1 enba2
        | FORKLIFT -> 
            let nba1, nba2 = NBA.bringPairToSameAPs product aut
            let enba1 = ExplicitAutomaton.ExplicitNBA.convertFromSymbolicNBA nba1
            let enba2 = ExplicitAutomaton.ExplicitNBA.convertFromSymbolicNBA nba2

            if config.SolverConfig.ForkliftJarPath |> Option.isNone then 
                raise <| AnalysisException "Required FORKLIFT for inclusion check, but no path to FORKLIFT is given"
            ExplicitAutomaton.AutomataChecks.checkNBAContainmentForklift Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.ForkliftJarPath.Value enba1 enba2
                
    swInclusion.Stop()
    res

let rec private modelCheckComplementationRec (config : Configuration)  (tslist : list<TransitionSystem<'L>>) (qf : list<int>) (isNegated : bool) (aut : NBA<int, 'L * int>) m = 
    assert (tslist.Length = List.sum qf)

    if qf.Length = 0 then
        assert (aut.APs.Length = 0)

        swEmptiness.Start()

        let isNotEmpty = 
            if config.SolverConfig.AutfiltPath |> Option.isNone then 
                raise <| AnalysisException "Required spot's autfilt for emptiness check, but autfilt is not given"

            match FsOmegaLib.Operations.AutomataChecks.isEmpty Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.AutfiltPath.Value aut with 
            | Success x -> not x 
            | Fail err -> 
                config.LoggerN err.DebugInfo // Log the detailled infos
                raise <| AnalysisException err.Info

        swEmptiness.Stop()
        
        if isNegated then 
            not isNotEmpty
        else 
            isNotEmpty
            
    elif qf.Length = 1 && (match m with INCL _ -> true | _ -> false) then
        let inclusionChecker = 
            match m with 
            | INCL x -> x 
            | _ -> raise <| AnalysisException "Should not happen"
        
        config.Logger "Starting Inclusion Check..."
        if isNegated then
            raise <| AnalysisException "Automaton is negated but should not be"
            
        swInclusion.Start()
        let res = 
            match inclusionTest config tslist aut inclusionChecker with 
            | Success x -> x 
            | Fail err -> 
                config.LoggerN err.DebugInfo
                raise <| AnalysisException err.Info
            
        swInclusion.Stop()

        config.LoggerN "Done"

        res
    else
        let lastQuantifierType = if qf.Length % 2 = 1 then FORALL else EXISTS

        let needsComplement = (lastQuantifierType = EXISTS && isNegated) || (lastQuantifierType = FORALL && not isNegated)
        
        let possiblyNegatedAut = 
            if needsComplement then 
                swComplement.Start()
                let r = 
                    if config.SolverConfig.AutfiltPath |> Option.isNone then 
                        raise <| AnalysisException "Required spot's autfilt for NBA complementation, but autfilt is not given"

                    match FsOmegaLib.Operations.AutomataOperations.complementToNBA Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.AutfiltPath.Value LOW aut with 
                    | Success x -> x 
                    | Fail err -> 
                        config.LoggerN err.DebugInfo
                        raise <| AnalysisException err.Info

                swComplement.Stop()
                r
            else 
                aut
        
        let projStartIndex = qf[0..qf.Length-2] |> List.sum 
        let projEndIndex = List.sum qf - 1

        config.Logger "Starting Product Construction..."
        swProduct.Start()
        let nextAut =  
            ProductConstruction.constructAutomatonSystemProduct  possiblyNegatedAut tslist[projStartIndex..projEndIndex] [projStartIndex..projEndIndex]
            |> NBA.convertStatesToInt

        swProduct.Stop()

        config.LoggerN "Done"

        let isNegated = if lastQuantifierType = FORALL then true else false

        modelCheckComplementationRec config tslist[0..projStartIndex-1] qf[0..qf.Length-2] isNegated nextAut m
       
let private modelCheckInit (config : Configuration)  (tslist : list<TransitionSystem<'L>>) (qfPrefix : list<int>) (ltlMatrix : LTL<'L * int>) (m : Mode) = 
    assert (tslist.Length = List.sum qfPrefix)
    
    assert(qfPrefix.Length >= 2)
   
    let lastQuantifierType = if qfPrefix.Length % 2 = 1 then FORALL else EXISTS

    let isNegated = 
        if lastQuantifierType = EXISTS then 
            false
        else 
            true

    config.Logger "Starting LTL2NBA..."
    swLTLtoNBA.Start()
    let currentNBA =
        let f =    
            if lastQuantifierType = EXISTS then 
                ltlMatrix
            else 
                FsOmegaLib.LTL.Not ltlMatrix

        if config.SolverConfig.Ltl2tgbaPath |> Option.isNone then 
            raise <| AnalysisException "Required spot's ltl2tgba, but ltl2tgba is not given"

        match FsOmegaLib.Operations.LTLConversion.convertLTLtoNBA Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.Ltl2tgbaPath.Value f with 
        | Success aut -> aut 
        | Fail err -> 
            config.LoggerN err.DebugInfo
            raise <| AnalysisException err.Info
        
    swLTLtoNBA.Stop()

    config.LoggerN "Done"
    
    modelCheckComplementationRec config tslist qfPrefix isNegated currentNBA m
    

let private modelCheckAlternationFree (config : Configuration) (tslist : list<TransitionSystem<'L>>) (prop : HyperLTL<'L>) =  
    config.LoggerN "Verify alternation free formula"
    assert(extractBlocks prop.QuantifierPrefix |> List.length = 1)

    let shouldNegate = if prop.QuantifierPrefix.[0] = FORALL then true else false

    let matrix = 
        if shouldNegate then 
            LTL.Not prop.LTLMatrix
        else 
            prop.LTLMatrix

    if config.SolverConfig.Ltl2tgbaPath |> Option.isNone then 
        raise <| AnalysisException "Required spot's ltl2tgba, but ltl2tgba is not given"
    
    swLTLtoNBA.Start()
    let nba = 
        match FsOmegaLib.Operations.LTLConversion.convertLTLtoNBA Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.Ltl2tgbaPath.Value matrix with 
        | Success aut -> aut 
        | Fail err -> 
            config.LoggerN err.DebugInfo
            raise <| AnalysisException err.Info

    swLTLtoNBA.Stop()

    let isNotEmpty = 
        // Project the automaton on the transition systems
        swProduct.Start()
        let productAut =  
            ProductConstruction.constructAutomatonSystemProduct  nba tslist [0..tslist.Length - 1]
            |> NBA.convertStatesToInt
        swProduct.Stop()

        // All quantifiers should be eliminated by now
        assert (productAut.APs.Length = 0)

        // Check emptiness of this automaton
        swEmptiness.Start()
        if config.SolverConfig.AutfiltPath |> Option.isNone then 
            raise <| AnalysisException "Required spot's autfilt for emptiness check, but autfilt is not given"

        let isNotEmpty = 
            match FsOmegaLib.Operations.AutomataChecks.isEmpty Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.AutfiltPath.Value productAut with 
            | Success res -> not res 
            | Fail err -> 
                config.LoggerN err.DebugInfo
                raise <| AnalysisException err.Info

        swEmptiness.Stop()

        isNotEmpty
        
    // If negated we return the opposite result
    if shouldNegate then 
        not isNotEmpty
    else 
        isNotEmpty
    
let modelCheck (config : Configuration) (tslist : list<TransitionSystem<'L>>) (prop : HyperLTL<'L>) m = 
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()

    swLTLtoNBA.Reset()
    swEmptiness.Reset()
    swInclusion.Reset()
    swProduct.Reset()
    swComplement.Reset()
    
    tslist
    |> List.iteri (fun i ts ->
        match TransitionSystem.findError ts with 
        | None -> ()
        | Some msg -> 
            raise <| AnalysisException $"Found error in the %i{i}th system: %s{msg}"
            )
    
    if HyperLTL.isConsistent prop |> not then
        raise <| AnalysisException $"The HyperLTL property is not consistent."
    
    prop.LTLMatrix
    |> LTL.allAtoms
    |> Set.toList
    |> List.iter (fun (x, i) ->
        if List.length tslist <= i then 
            raise <| AnalysisException $"AP (%A{x}, %i{i}) is used in the HyperLTL property but only %i{List.length tslist} systems are given."
        if List.contains x tslist.[i].APs |> not then
            raise <| AnalysisException $"AP (%A{x}, %i{i}) is used in the HyperLTL property but AP %A{x} does not exists in the %i{i}th transition system."
    )
        
    // Convert the prefix to a block prefix
    let blockPrefix = extractBlocks prop.QuantifierPrefix

    let res = 
        if List.length blockPrefix = 1 then 
            // The formula is alternation-free, use a direct product construction
            modelCheckAlternationFree config tslist prop
        else 
            // The formula contains a quantifier alternation.
            // If the outermost quantifier is \exists, we negate everything so we can always work with a property starting with a \forall quantifier
            // This way, verification in the outermost alternation can always be reduced to a language inclusion check (if desired)
            let shouldNegate = if prop.QuantifierPrefix.[0] = EXISTS then true else false

            let matrix = 
                if shouldNegate then 
                    LTL.Not prop.LTLMatrix
                else 
                    prop.LTLMatrix

            let res = modelCheckInit config tslist blockPrefix matrix m

            if shouldNegate then 
                not res
            else 
                res

    sw.Stop()
    
    let t = 
        {
            TotalTime = int(sw.ElapsedMilliseconds)
            LTL2NBATime = int(swLTLtoNBA.ElapsedMilliseconds)
            ProductTime = int(swProduct.ElapsedMilliseconds)
            InclusionTime = int(swInclusion.ElapsedMilliseconds)
            ComplementationTime = int(swComplement.ElapsedMilliseconds)
            EmptinessTime = int(swEmptiness.ElapsedMilliseconds)
        }
    res, t
