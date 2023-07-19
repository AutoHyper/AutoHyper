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

module CommandLineParser

open System

open RunConfiguration
open ModelChecking

open JSON

let VERSION = "1.1"

type ExecutionMode = 
    | ExplictSystem of list<String> * String
    | BooleanProgram of list<String> * String
    | NusmvSystem of list<String> * String

type CommandLineArguments = 
    {
        ExecMode : option<ExecutionMode>
        Mode : option<ModelChecking.Mode>
        DebugOutputs : bool
        ComputeBisimulation : bool
    }

    static member Default = 
        {
            ExecMode = None
            Mode = Option.None
            DebugOutputs = false
            ComputeBisimulation = true
        }

let parseConfigFile (s : string) =
    match JSON.Parser.parseJsonString s with 
    | Result.Error err -> raise <| AnalysisException $"Could not parse config file: %s{err}"
    | Result.Ok x -> 
        {
            MainPath = "./"
            AutfiltPath =  
                (JSON.tryLookup "autfilt" x)
                |> Option.bind (fun x -> JSON.tryGetString x)
            Ltl2tgbaPath = 
                (JSON.tryLookup "ltl2tgba" x)
                |> Option.bind (fun x -> JSON.tryGetString x)
            BaitJarPath = 
                (JSON.tryLookup "bait" x)
                |> Option.bind (fun x -> JSON.tryGetString x)
            RabitJarPath = 
                (JSON.tryLookup "rabit" x)
                |> Option.bind (fun x -> JSON.tryGetString x)
            ForkliftJarPath  = 
                (JSON.tryLookup "forklift" x)
                |> Option.bind (fun x -> JSON.tryGetString x)
        }

let rec private splitByPredicate (f : 'T -> bool) (xs : list<'T>) = 
    match xs with 
        | [] -> [], []
        | x::xs -> 
            if f x then 
                [], x::xs 
            else 
                let r1, r2 = splitByPredicate f xs 
                x::r1, r2

let parseCommandLineArguments (args : list<String>) =
    let rec parseArgumentsRec (args : list<String>) (opt : CommandLineArguments) = 

        match args with 
            | [] -> Result.Ok opt
            | x::xs -> 
                match x with 
                | "--exp" -> 
                    let args, ys = splitByPredicate (fun (x : String) -> x.[0] = '-') xs
        
                    if List.length args < 2 then 
                        Result.Error "Option --exp must be followed by at least two arguments"
                    else 
                        let propertyFile = args[args.Length - 1]
                        let systemFiles = args[0..args.Length - 2]
                        parseArgumentsRec ys {opt with ExecMode = ExplictSystem(systemFiles, propertyFile) |> Some}
                | "--nusmv" -> 
                    let args, ys = splitByPredicate (fun (x : String) -> x.[0] = '-') xs
        
                    if List.length args < 2 then 
                        Result.Error "Option --nusmv must be followed by at least two arguments"
                    else 
                        let propertyFile = args[args.Length - 1]
                        let systemFiles = args[0..args.Length - 2]
                        parseArgumentsRec ys {opt with ExecMode = NusmvSystem(systemFiles, propertyFile)  |> Some}

                | "--bp" -> 
                    let args, ys = splitByPredicate (fun (x : String) -> x.[0] = '-') xs
        
                    if List.length args < 2 then 
                        Result.Error "Option --bp must be followed by at least two arguments"
                    else 
                        let propertyFile = args[args.Length - 1]
                        let systemFiles = args[0..args.Length - 2]
                        parseArgumentsRec ys {opt with ExecMode = BooleanProgram(systemFiles, propertyFile)  |> Some}
                | "-m" -> 
                    match xs with 
                        | [] -> 
                            Result.Error "Option -m must be followed by an argument" 
                        | y::ys -> 
                            try     
                                match y with 
                                    | "comp" -> parseArgumentsRec ys { opt with Mode = Some COMP }
                                    | "incl_spot" -> parseArgumentsRec ys { opt with Mode = Some (INCL SPOT) }
                                    | "incl_rabit" -> parseArgumentsRec ys { opt with Mode = Some (INCL RABIT) }
                                    | "incl_bait" -> parseArgumentsRec ys { opt with Mode = Some (INCL BAIT) }
                                    | "incl_forklift" -> parseArgumentsRec ys { opt with Mode = Some (INCL FORKLIFT) }
                                    | _ -> Result.Error ("Unsupported Mode: " + y)
                            with _ -> Result.Error ("Unsupported Mode: " + y)
                | "--debug" -> 
                    parseArgumentsRec xs { opt with DebugOutputs = true}
                | "--no-bisim" -> 
                    parseArgumentsRec xs { opt with ComputeBisimulation = true}
                | "-h" | "--help" | "-help" -> 
                    printfn $"AutoHyper (Version %s{VERSION})"
                    printfn ""
                    printfn "Copyright (C) 2022-2023 Raven Beutner"
                    printfn "This program comes with ABSOLUTELY NO WARRANTY."
                    printfn "This is free software, and you are welcome to redistribute it"
                    printfn "under certain conditions; use `--license' for details."
                    printfn ""
                    printfn "You have the following options: "
                    printfn ""
                    printfn "  --exp             verify a HyperLTL property on an explicit-state system"
                    printfn "  --nusmv           verify a HyperLTL property on an NuSMV system"
                    printfn "  --bp              verify a HyperLTL property on an boolean program"
                    printfn ""
                    printfn "  All of the above options are used in the form '(-e | -nusmv | -bp) <systemFile(s)> <propFile>'"
                    printfn "  where <systemFile(s)> is a (list of) files to systems and <propFile> the file containing the specification."
                    printfn "  In case only a single systems is given, it will be used for all quantifier. Otherwise the number of systems must match the quantifier prefix in the specification."
                    printfn ""
                    printfn "  -m               specifies the mode to be used by AutoHyper. Options are 'comp', 'incl_spot', 'incl_rabit', 'incl_bait', 'incl_forklift'. "
                    printfn "                   If left unspecified AutoHyper uses incl_spot. "
                    printfn ""
                    printfn "  --debug          if set, AutoHyper outputs additional infos and more"
                    printfn ""
                    printfn "  --version        prints the current version of AutoHyper."
                    printfn ""
                    printfn "  --license        prints important information about the license used for AutoHyper."
                    printfn ""
                    printfn "  --help | -h      displays this help message."

                    exit 0

                | "--version"  -> 
                    printfn $"AutoHyper (Version %s{VERSION})"

                    exit 0

                | "--license"  -> 
                    printfn $"AutoHyper (Version %s{VERSION})"
                    printfn ""
                    printfn "This program is free software: you can redistribute it and/or modify"
                    printfn "it under the terms of the GNU General Public License as published by"
                    printfn "the Free Software Foundation, either version 3 of the License, or"
                    printfn "(at your option) any later version."
                    printfn ""
                    printfn "This program is distributed in the hope that it will be useful,"
                    printfn "but WITHOUT ANY WARRANTY; without even the implied warranty of"
                    printfn "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
                    printfn "GNU General Public License for more details."
                    printfn ""
                    printfn "You should have received a copy of the GNU General Public License"
                    printfn "along with this program.  If not, see <https://www.gnu.org/licenses/>"

                    exit 0

                | _ -> Result.Error ("Option " + x + " is not supported" )
        
    parseArgumentsRec args CommandLineArguments.Default
                                