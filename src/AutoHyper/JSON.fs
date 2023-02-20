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

module JSON

exception JsonError

type Json = 
    | JString of string
    | JNumber of float
    | JBool   of bool
    | JNull
    | JList   of Json list
    | JObject of Map<string, Json>

module JSON = 

    let rec toString (json : Json) = 
        match json with 
        | JString str -> "\"" + str + "\""
        | JNumber f -> string f
        | JBool b -> if b then "true" else "false"
        | JNull -> "null"
        | JList l -> 
            l 
            |> List.map (fun x -> toString x)
            |> Util.combineStringsWithSeperator ", "
            |> fun x -> "[" + x + "]"

        | JObject m -> 
            m 
            |> Map.toList
            |> List.map (fun (k, v) -> "\"" + k + "\": " + toString v)
            |> Util.combineStringsWithSeperator ", "
            |> fun x -> "{" + x + "}"

    let lookup s (json : Json) = 
        match json with 
        | JObject m -> 
            if Map.containsKey s m then    
                m.[s]
            else 
                raise JsonError
        | _ -> raise JsonError

    let tryLookup s (json : Json) = 
        match json with 
        | JObject m -> 
            if Map.containsKey s m then    
                Some m.[s]
            else 
                None
        | _ -> None

    
    let getString (json : Json) = 
        match json with 
        | JString s -> s 
        | _ -> raise JsonError
    
    let getNumber (json : Json) = 
        match json with 
        | JNumber n -> n
        | _ -> raise JsonError

    let getBool (json : Json) = 
        match json with 
        | JBool b -> b
        | _ -> raise JsonError
    
    let getList (json : Json) = 
        match json with 
        | JList l -> l
        | _ -> raise JsonError

    let getMap (json : Json) = 
        match json with 
        | JObject s -> s 
        | _ -> raise JsonError

    let tryGetString (json : Json) = 
        match json with 
        | JString s -> Some s 
        | _ -> None
    
    let ryGetNumber (json : Json) = 
        match json with 
        | JNumber n -> Some n
        | _ -> None

    let tryGetBool (json : Json) = 
        match json with 
        | JBool b -> Some b
        | _ -> None
    
    let tryGetList (json : Json) = 
        match json with 
        | JList l -> Some l
        | _ -> None

    let tryGetMap (json : Json) = 
        match json with 
        | JObject s -> Some s 
        | _ -> None
    


module Parser = 
    open FParsec

    let private jsonParser, private jsonParserRef = createParserForwardedToRef()

    let private stringLiteral =
        let escape =  
            anyOf "\"\\/bfnrt"
            |>> function
                | 'b' -> "\b"
                | 'f' -> "\u000C"
                | 'n' -> "\n"
                | 'r' -> "\r"
                | 't' -> "\t"
                | c   -> string c

        let unicodeEscape =
            pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                let hex2int c = (int c &&& 15) + (int c >>> 6)*9
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string
            )

        between (pstring "\"") (pstring "\"")
                (stringsSepBy (manySatisfy (fun c -> c <> '"' && c <> '\\'))
                            (pstring "\\" >>. (escape <|> unicodeEscape)))


    let private stringParser = stringLiteral |>> JString

    let private numberParser = pfloat |>> JNumber 

    let private trueParser  = stringReturn "true"  (JBool true)
    let private falseParser = stringReturn "false" (JBool false)
    let private nullParser  = stringReturn "null" JNull

    let private listParser   = 
        between (pchar '[' .>> spaces) (pchar ']') (sepBy (jsonParser .>> spaces) (pchar ',' .>> spaces))
        |>> JList

    let private objectParser   = 
        between (pchar '{' .>> spaces) (pchar '}') (sepBy (tuple2 stringLiteral (spaces >>. pstring ":" >>. spaces >>. jsonParser) .>> spaces) (pchar ',' .>> spaces))
        |>> (fun x -> x |> Map.ofList |> JObject)
    

    do jsonParserRef.Value <-
        choice [
                objectParser
                listParser
                stringParser
                numberParser
                trueParser
                falseParser
                nullParser
            ]

    let parseJsonString str = 
        let p = spaces >>. jsonParser .>> spaces .>> eof
        match run p str with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err
    