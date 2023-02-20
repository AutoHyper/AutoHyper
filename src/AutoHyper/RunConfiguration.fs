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

module RunConfiguration 

open System

exception AnalysisException of String 
exception TimeoutException 


/// Records the run configuration (the location) of each supported automaton tool
type SolverConfiguration = 
    {
        MainPath : String
        AutfiltPath: option<String>
        Ltl2tgbaPath: option<String>
        BaitJarPath: option<String>
        RabitJarPath: option<String>
        ForkliftJarPath : option<String>
    }

    static member Default = 
        {
            MainPath = "./"
            AutfiltPath = Option.None
            Ltl2tgbaPath = Option.None
            BaitJarPath = Option.None
            RabitJarPath = Option.None
            ForkliftJarPath  = Option.None
        }
        
type VerbosityLevel = 
    | ZERO
    | ONE 
    | TWO 
    | THREE
    | FOUR 

/// A configuration summarizes the location to each solver and a printing function that is called for all non-fatal printouts
type Configuration = 
    {
        SolverConfig : SolverConfiguration
        Logger : list<VerbosityLevel> -> String -> unit
    }
