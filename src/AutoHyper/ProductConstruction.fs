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

module ProductConstruction

open System.Collections.Generic

open TransitionSystemLib.TransitionSystem

open FsOmegaLib.SAT
open FsOmegaLib.AutomatonSkeleton
open FsOmegaLib.NBA

let constructAutomatonSystemProduct (aut : NBA<'Astate, 'L * int>) (tslist : list<TransitionSystem<'L>>) (indexList : list<int>) = 

    // Reverse the mapping of indexList, i.e., map every index that should be mapped to the index (position) in indexlist
    // All keys in copyToTsMapper should thus be eliminated with the product and all others remain unchanged
    let copyToTsMapper = 
        indexList
        |> List.mapi (fun i j -> j, i)
        |> Map.ofList

    // Each AP that takes part in the product is mapped to the index of the ts in tslist and the index within the AP list of that transition system
    let quickApIndexFinderPos = 
        aut.APs
        |> List.mapi (fun j (l, i) -> 
            if copyToTsMapper.Keys.Contains i then 
                // The copy (i) where the jth AP is refering to is part of the product.
                // Find the position within the APs of the copyToTsMapper.[i] ts for a faster lookup
                let tsIndex = copyToTsMapper.[i]
                Some (j, (tsIndex, tslist.[tsIndex].APs |> List.findIndex (fun l' -> l = l')))
            else
                None
            )
        |> List.choose id 
        |> Map.ofList

    // Compute the APs that will be used in the computed automaton where the indices in indexList are projected away
    let newAps = 
        aut.APs
        |> List.filter (fun (_, i) -> copyToTsMapper.Keys.Contains i |> not)

    // Map the index from the (old) aut.APs to the APs of the automaton we will construct
    let quickApIndexFinderRemaining =
        newAps
        |> List.mapi (fun i x -> aut.APs |> List.findIndex (fun y -> y = x), i)
        |> Map.ofList

    let initStates = 
        tslist 
        |> List.map (fun x -> x.InitialStates |> seq) 
        |> Util.cartesianProduct
        |> Seq.allPairs aut.InitialStates 
        |> Seq.map (fun (astate, tstates) -> tstates, astate)
        
    let queue = new Queue<_>(initStates)

    let allStates = new HashSet<_>(initStates)
    let newEdges = new Dictionary<_,_>()
    
    while queue.Count <> 0 do 
        let n = queue.Dequeue()
        let tstates, astate = n 

        // All possible combinations of successor states
        let systemSucs = 
            tstates
            |> List.mapi (fun i x -> tslist.[i].Edges.[x])
            |> List.map seq
            |> Util.cartesianProduct

        let apEvaluations = 
            tstates
            |> List.mapi (fun i x -> tslist.[i].ApEval.[x])

        // This map fixes all propositions that refer to a system that take part in the product
        let fixingMap = 
            quickApIndexFinderPos
            |> Map.map (fun _ (index, pos) -> 
                Set.contains pos apEvaluations.[index])

        let automatonSucs = 
            aut.Edges.[astate]
            |> List.choose (fun (g, t) -> 
                let guardFixed = DNF.fixValues fixingMap g 
                // We only consider this guard if it is actually sat (after having fixed the projected atoms)
                if DNF.isSat guardFixed then 
                    let remappedGuard = DNF.map (fun x -> quickApIndexFinderRemaining.[x]) guardFixed

                    Some (remappedGuard, t) 
                else 
                    None
                )

        let sucs = 
            Seq.allPairs systemSucs automatonSucs 
            |> Seq.map (fun (tstates', (g, astate')) -> 
                // Add states for further exploration
                if allStates.Contains (tstates', astate') |> not then 
                    allStates.Add (tstates', astate') |> ignore
                    queue.Enqueue (tstates', astate')


                (g, (tstates', astate'))
            )
            |> Seq.toList

        newEdges.Add(n, sucs)
         
    {
        NBA.Skeleton = 
            {
                AutomatonSkeleton.States = set allStates;
                APs = newAps
                Edges = Util.dictToMap newEdges
            };
        InitialStates = set initStates;
        AcceptingStates = 
            allStates
            |> Seq.filter (fun (_, astate) -> aut.AcceptingStates.Contains astate)
            |> set
    }
    

let constructSelfCompositionAutomaton (tslist : list<TransitionSystem<'L>>) (interestingAps : list<list<'L>>) : NBA<_,_> = 
    // Assert that all interesting aps are also aps in the transition system
    assert(
        tslist
        |> List.map (fun x -> x.APs )
        |> List.zip interestingAps
        |> List.forall (fun (a, b) -> 
            a
            |> List.forall (fun x -> List.contains x b)
            )
        )

    let apMapping = 
        interestingAps
        |> List.mapi (fun i aplist -> 
            aplist
            |> List.map (fun a -> a, List.findIndex (fun a' -> a = a') tslist.[i].APs )
            |> Map.ofList
        )

    // Collect the aps that are interesting, indexed by their position
    let combinedAps = 
        interestingAps
        |> List.mapi (fun i x -> 
            x 
            |> List.distinct
            |> List.map (fun c -> c, i)
        )
        |> List.concat
    
    let allInitStates =
        tslist
        |> List.map (fun x -> x.InitialStates |> seq)
        |> Util.cartesianProduct
        |> set

    let queue = new Queue<_>(allInitStates)
    let newEdges = new Dictionary<_,_>()
    let allStates = new HashSet<_>(allInitStates)

    while queue.Count <> 0 do 
        let tstates = queue.Dequeue()

        let guardDNF : DNF<int> = 
            tstates
            |> List.mapi (fun i x -> 
                let apEval = tslist.[i].ApEval.[x]

                interestingAps.[i]
                |> List.map (fun  a -> 
                    let b = Set.contains (apMapping.[i].[a]) apEval
                    
                    let newApIndex = List.findIndex (fun c -> c = (a, i)) combinedAps
                    if b then Literal.PL newApIndex else Literal.NL newApIndex
                    )
            )
            |> List.concat
            |> List.singleton

        let allSuccessorStates = 
            tstates
            |> List.mapi (fun i x -> tslist.[i].Edges.[x] |> seq)
            |> Util.cartesianProduct
            |> Seq.map (fun x -> guardDNF, x)
            |> Seq.toList
            

        newEdges.Add(tstates, allSuccessorStates)
        
        for _, tstates' in allSuccessorStates do 
            if allStates.Contains tstates' |> not then 
                allStates.Add tstates' |> ignore 
                queue.Enqueue tstates'

    {
        NBA.Skeleton = 
            {
                AutomatonSkeleton.States = set allStates;
                APs = combinedAps
                Edges = Util.dictToMap newEdges
            };
        InitialStates = allInitStates
        AcceptingStates = set allStates
    }