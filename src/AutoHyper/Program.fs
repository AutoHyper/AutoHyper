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

module Program

open System 
open System.IO 

open RunConfiguration
open ModelChecking

open CommandLineParser
open ModelCheckingEntryPoint

let private run (args: array<string>) =

    let swtotal = System.Diagnostics.Stopwatch()
    swtotal.Start()

    let args = 
        if args.Length = 0 then 
            // In case no command line arguments are given, we assume the user wants the help message
            [|"--help"|]
        else 
            args

    // Parse the command line args
    let cmdArgs =
        match CommandLineParser.parseCommandLineArguments (Array.toList args) with
            | Result.Ok x -> x
            | Result.Error e ->
                   raise <| AnalysisException $"%s{e}"
                   
    // By convention the paths.json file is located in the same directory as the HyPA executable
    let configPath = 
        System.IO.Path.Join [|System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location); "paths.json"|]
        
                   
    // Check if the path to the config file is valid , i.e., the file exists
    if System.IO.FileInfo(configPath).Exists |> not then 
        raise <| AnalysisException "The paths.json file does not exist in the same directory as the executable"            
    
    // Parse the config File
    let configContent = 
        try
            File.ReadAllText configPath
        with 
            | _ -> 
                raise <| AnalysisException "Could not open paths.json file"

    let logger s = 
        if cmdArgs.DebugOutputs then 
            printf $"%s{s}"             
    
    let solverConfig = CommandLineParser.parseConfigFile configContent

    if solverConfig.AutfiltPath.IsSome then 
        if solverConfig.AutfiltPath.Value <> "" && (System.IO.FileInfo(solverConfig.AutfiltPath.Value).Exists |> not) then 
            raise <| AnalysisException "The path to the spot's autfilt is incorrect"

    if solverConfig.Ltl2tgbaPath.IsSome then 
        if solverConfig.Ltl2tgbaPath.Value <> "" && (System.IO.FileInfo(solverConfig.Ltl2tgbaPath.Value).Exists |> not) then 
            raise <| AnalysisException "The path to the spot's ltl2tgba is incorrect"
        
    if solverConfig.RabitJarPath.IsSome then 
        if System.IO.FileInfo(solverConfig.RabitJarPath.Value).Exists |> not then 
            raise <| AnalysisException "The path to the RABIT jar is incorrect"
        
    if solverConfig.BaitJarPath.IsSome then 
        if System.IO.FileInfo(solverConfig.BaitJarPath.Value).Exists |> not then 
            raise <| AnalysisException "The path to the BAIT jar is incorrect"

    if solverConfig.ForkliftJarPath.IsSome then 
        if System.IO.FileInfo(solverConfig.ForkliftJarPath.Value).Exists |> not then 
            raise <| AnalysisException "The path to the FORKLIFT jar is incorrect"
    
    let config = 
        {
            Configuration.SolverConfig = solverConfig
            Logger = logger
        }

    let mode = Option.defaultValue (INCL SPOT) cmdArgs.Mode
   
    match cmdArgs.ExecMode with 
        | None -> 
            raise <| AnalysisException "Must specify an exeuction mode"
        | Some (ExplictSystem (systemPaths, propPath)) -> 
            explictSystemVerification config systemPaths propPath mode
        | Some (BooleanProgram (systemPaths, propPath)) -> 
            booleanProgramVerification config systemPaths propPath mode

        | Some (NusmvSystem (systemPaths, propPath)) -> 
            nuSMVSystemVerification config systemPaths propPath mode

    swtotal.Stop()
    config.LoggerN $"Time %i{swtotal.ElapsedMilliseconds}ms (~=%.2f{double(swtotal.ElapsedMilliseconds) / 1000.0}s)"

    0

[<EntryPoint>]
let main args =
    try 
        run args
    with 
        | _  when Util.DEBUG -> reraise()
        | AnalysisException err -> 
            printfn "Error during the analysis:"
            printfn "%s" err
            exit -1
        | e -> 
            printfn "Unexpected Error during the analysis:"
            printfn "%s" e.Message
            exit -1