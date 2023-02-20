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

module BooleanPrograms 

open System
open System.Collections.Generic

open RunConfiguration
open TransitionSystem

type Var = String 

type Value = list<bool>

type VarAssignment = Map<Var, Value>

type ProgramExpression = 
    | True 
    | False
    | Variable of Var
    | And of ProgramExpression * ProgramExpression
    | Or of ProgramExpression * ProgramExpression
    | Not of ProgramExpression 
    | Proj of ProgramExpression * int 
    | Concat of ProgramExpression * ProgramExpression
    | Dup of ProgramExpression * int 

    member this.UsedVars = 
        match this with 
            | True | False -> Set.empty
            | Variable x -> Set.singleton x
            | And(e1, e2) | Or(e1, e2) | Concat(e1, e2) -> 
                Set.union e1.UsedVars e2.UsedVars
            | Not e | Proj(e, _) | Dup(e, _) -> 
                e.UsedVars
            
    member this.Eval (A : VarAssignment) : Value = 
        match this with 
            | True -> [true]
            | False -> [false]
            | Variable x -> A.[x]
            | And(e1, e2) -> 
                List.zip (e1.Eval A) (e2.Eval A)
                |> List.map (fun (x, y) -> x && y)
            | Or(e1, e2) ->
                List.zip (e1.Eval A) (e2.Eval A)
                |> List.map (fun (x, y) -> x || y)
            | Not e -> 
                e.Eval A |> List.map not
            | Proj(e, n) -> [(e.Eval A).[n]]
            | Concat(e1, e2) -> List.append (e1.Eval A) (e2.Eval A)
            | Dup(e, n) -> 
                let t = e.Eval A
                
                List.init n (fun _ -> t)
                |> List.concat 

    member this.InferBitWidth (env : Map<Var, int>) = 
        match this with 
            | True -> Some 1
            | False -> Some 1
            | Variable x -> Some env.[x]
            | And(e1, e2) -> 
                match e1.InferBitWidth env, e2.InferBitWidth env with 
                    | Some i1, Some i2 when i1 = i2 -> Some i1 
                    | _ ->  None
            | Or(e1, e2) ->
                match e1.InferBitWidth env, e2.InferBitWidth env with 
                    | Some i1, Some i2 when i1 = i2 -> Some i1 
                    | _ -> None
            | Not e -> 
                e.InferBitWidth env
            | Proj(e, n) -> 
                match e.InferBitWidth env with 
                    | Some b -> 
                        if n >= b then 
                            None 
                        else 
                            Some 1
                    | None -> None 
            | Concat(e1, e2) -> 
                match e1.InferBitWidth env, e2.InferBitWidth env with 
                    | Some i1, Some i2-> Some (i1 + i2) 
                    | _ -> 
                        None
            | Dup(e, n) -> 
                match e.InferBitWidth env with 
                    | Some b -> 
                        Some (n * b)
                    | None -> None 


type ProgramStatement = 
    | Terminated
    | Skip
    | Assignment of Var * ProgramExpression 
    | If of ProgramExpression * ProgramStatement * ProgramStatement
    | Nondet of ProgramStatement * ProgramStatement
    | Read of  Var 
    | Seq of list<ProgramStatement>
    | While of ProgramExpression * ProgramStatement

    member this.UsedVars = 
        match this with 
            | Terminated | Skip -> Set.empty
            | Assignment(v, e) ->
                Set.add v e.UsedVars
            | If(e, s1, s2) -> 
                [e.UsedVars; s1.UsedVars; s2.UsedVars]
                |> Set.unionMany
            | Nondet(s1, s2) ->
                Set.union s1.UsedVars s2.UsedVars
            | Read x -> 
                Set.singleton x 
            | Seq slist -> 
                slist 
                |> List.map (fun x -> x.UsedVars)
                |> Set.unionMany

            | While(e, s) -> 
                Set.union e.UsedVars s.UsedVars

    member this.IsWellFormed dom = 
        match this with 
            | Terminated -> true
            | Skip -> true
            | Assignment(v, e) ->
                match e.InferBitWidth dom with 
                    | Some i ->  
                        i = dom.[v]
                    | None -> false
            | If(e, s1, s2) -> 
                match e.InferBitWidth dom with 
                    | Some 1 ->  
                        s1.IsWellFormed dom && s2.IsWellFormed dom
                    | _ -> false
            | Nondet(s1, s2) ->
                s1.IsWellFormed dom && s2.IsWellFormed dom
            | Read _ -> 
                true 
            | Seq slist -> 
                slist 
                |> List.forall (fun x -> x.IsWellFormed dom)
            | While(e, s) -> 
                match e.InferBitWidth dom with 
                    | Some 1 ->  
                        s.IsWellFormed dom
                    | _ -> false

    member this.OneStep(A : VarAssignment) = 
        match this with 
            | Terminated -> Seq.singleton (this, A)
            | Skip -> Seq.singleton (Terminated, A)
            | Assignment(v, e) ->
                let newVal = e.Eval A
                let newAssignment = Map.add v newVal A
                Seq.singleton (Terminated, newAssignment)
            | If(e, s1, s2) -> 
                let res = e.Eval A
                if res.[0] then 
                    Seq.singleton (s1, A)
                else
                    Seq.singleton (s2, A)
            | Nondet(s1, s2) ->
                seq {(s1, A); (s2, A)}
            | Read v -> 
                let width = A.[v].Length
                seq {for b in Util.computeBooleanPowerSet width do (Terminated,  Map.add v b A)}
            | Seq slist -> 
                match slist with 
                    | [] -> Seq.singleton (Terminated, A)
                    | [x] -> x.OneStep A
                    | x::xs -> 
                        x.OneStep A
                        |> Seq.map (fun (s, A') -> 
                            match s with 
                                | Terminated -> (Seq xs, A')
                                | t -> (Seq(t::xs), A')
                            )
            | While(e, s) -> 
                let res = e.Eval A 
                if res.[0] then 
                    Seq.singleton (Seq[s; While(e, s)], A)
                else
                    Seq.singleton (Terminated, A)
                    
type Program = 
    {
        Name : String
        DomainMap : Map<Var, int>
        Statement : ProgramStatement
    }

    member this.CheckConsistent() = 
        let usedVars = this.Statement.UsedVars

        usedVars
        |> Set.iter (fun x -> 
            if this.DomainMap.ContainsKey x |> not then 
                raise <| AnalysisException $"Varibale %s{x} is used the program but not defined in the domain."
            )

        if this.Statement.IsWellFormed this.DomainMap |> not then 
            raise <| AnalysisException "The bitwidths of the varaibles do not match. Type mismatch."
 
module Compilation = 
    type ProgramState = ProgramStatement * VarAssignment

    let compileProgramToTS (P : Program) (relevantAps : list<Var * int>) = 
        let initialState : ProgramState = (P.Statement, Map.map (fun _ x -> List.init x (fun _ -> false)) P.DomainMap)

        let allStates = new HashSet<_>()
        let queue = new Queue<_>()
        queue.Enqueue initialState
        allStates.Add initialState |> ignore

        let edgeDict = new Dictionary<_,_>()
        let apEvalDict = new Dictionary<_,_>()

        while queue.Count <> 0 do 
            let s = queue.Dequeue() 
            let p, A = s

            let sucs = 
                p.OneStep A
                |> set

            let apEval =   
                relevantAps 
                |> List.map (fun (v, i) -> 
                    A.[v].[i]
                    )

            for s' in sucs do 
                if allStates.Contains s' |> not then 
                    queue.Enqueue s' 
                    allStates.Add s' |> ignore

            edgeDict.Add(s, sucs)
            apEvalDict.Add(s, apEval)
            
        {
            States = set allStates
            InitialStates = Set.singleton initialState;
            APs = relevantAps;
            Edges = Util.dictToMap edgeDict;
            ApEval = Util.dictToMap apEvalDict
        }

module Parser =
    open FParsec

    let private ws = spaces
    
    let private varParser = 
        many1Chars letter 

    let private expParser, private expParserRef = createParserForwardedToRef()

    let private trueParser = 
        stringReturn "t" True

    let private falseParser = 
        stringReturn "f" False 

    let private variableParser = 
        attempt
            (
                varParser 
                >>= (fun x -> if x <> "t" && x <> "f" then Variable x |> preturn else fail "")
            )
    
    let private andParser = 
        pstring "(&" >>. spaces >>.
            pipe2 
                expParser
                expParser
                (fun x y -> And(x, y))
        .>> spaces .>> pstring ")"

    let private orParser = 
        pstring "(|" >>. spaces >>.
            pipe2 
                expParser
                expParser
                (fun x y -> Or(x, y))
        .>> spaces .>> pstring ")"

    let private concatParser = 
        pstring "(@" >>. spaces >>.
            pipe2 
                expParser
                expParser
                (fun x y -> Concat(x, y))
        .>> spaces .>> pstring ")"

    let private notParser = 
        pstring "(!" >>. expParser .>> spaces .>> pstring ")"
        |>> Not

    let private parParser = 
        pstring "(" >>. expParser .>> pstring ")"

    let private projParser = 
        pstring "(#" >>.
            pipe2 
                expParser 
                (spaces >>. pint32)
                (fun x y -> Proj(x, y))
        .>> spaces .>> pstring ")" 

    let private dupParser = 
        pstring "(x" >>.
            pipe2 
                expParser 
                (spaces >>. pint32)
                (fun x y -> Dup(x, y))
        .>> spaces .>> pstring ")" 

    do expParserRef := 
        spaces >>. choice [
                andParser
                orParser
                concatParser
                projParser
                dupParser
                notParser
                parParser
                variableParser
                trueParser
                falseParser
        ] .>> spaces

    let private statementParser, private statementParserRef = createParserForwardedToRef() 

    let private assignmentParser = 
        pipe3 
            varParser
            (spaces .>> pstring ":=" .>> spaces)
            expParser
            (fun x _ y -> Assignment(x, y))

    let private ifParser = 
        pstring "IF" >>. spaces >>. pstring "(" >>.
            pipe3 
                (expParser .>> spaces .>> pstring ")" .>> spaces .>> pstring "THEN" .>> spaces .>> pstring "{")
                (statementParser .>> spaces .>> pstring "}" .>> spaces .>> pstring "ELSE" .>> spaces .>> pstring "{")
                (statementParser .>> spaces .>> pstring "}")
                (fun x y z -> If(x, y, z))
    
    let private ndetParser = 
        pstring "NONDET" >>. spaces >>. pstring "THEN" >>. spaces >>. pstring "{" >>.
            pipe2
                (statementParser .>> spaces .>> pstring "}" .>> spaces .>> pstring "ELSE" .>> spaces .>> pstring "{")
                (statementParser .>> spaces .>> pstring "}")
                (fun x y -> Nondet(x, y))

    let private inParser = 
        pstring "READ" >>. spaces >>. pstring "(" >>. varParser .>> spaces .>> pstring ")"
        |>> Read
    
    let private skipParser = 
        stringReturn "SKIP" Skip

    let private whileParser = 
        pstring "WHILE" >>. spaces >>. pstring "(" >>.
            pipe2
                (expParser .>> spaces .>> pstring ")" .>> spaces .>> pstring "{")
                (statementParser .>> spaces .>> pstring "}")
                (fun x y -> While(x, y))

    let private seqParser = 
        between (pchar '[') (pchar ']') (sepBy (statementParser .>> spaces) (pchar ';'))
        |>> Seq

    do statementParserRef := 
        spaces >>. choice [
                ifParser
                ndetParser
                inParser
                skipParser
                whileParser
                seqParser
                assignmentParser
        ] .>> spaces

    let private parseHeader = 
        spaces >>. pstring "dom:" >>. spaces >>. pchar '[' >>. spaces >>.
        sepBy (between (pchar '(') (pchar ')')  (varParser .>>. (spaces >>. pint32) .>> spaces)) (pchar ',' .>> spaces)
        .>> spaces .>> pchar ']'
        |>> Map.ofList

    let private programParser = 
        pipe2
            parseHeader
            (spaces >>. statementParser)
            (fun x y -> {Program.DomainMap = x; Name = ""; Statement = y})

    let parseProgram (s : String) = 
        let full =
            programParser .>> spaces .>> eof

        let res = run full s

        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err