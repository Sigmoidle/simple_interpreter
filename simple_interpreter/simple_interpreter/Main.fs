module simple_interpreter.Main

open Parser
open Lexer
open Symbols
open Runtime
open System.IO

let public getTokens input = input |> lex

let public getParseTree input = input |> lex |> parse

let public getParseTreeAsString input = input |> lex |> parse |> sprintf "%A"

let public getInitialSymbolTable input = (input |> lex |> parse |> createSymbols)._symbolTable

let public getOutput input = input |> lex |> parse |> createSymbols |> runtime |> snd

let public getFinishedSymbolTable input = (input |> lex |> parse |> createSymbols |> runtime |> fst)._symbolTable

let public interpret input = input |> lex |> parse |> createSymbols |> runtime

[<EntryPoint>]
let main _ =
    System.Console.OutputEncoding <- System.Text.Encoding.Unicode
    let aplProgram = File.ReadAllText("test_program.apl")
    let parseTree = aplProgram |> lex |> parse
    let runtimeData = parseTree |> createSymbols
    let out = runtimeData |> runtime
    printfn "Simple Interpreter for APL"
    printfn "-----"
    printfn $"Input: \n%A{aplProgram}"
    printfn "-----"
    printfn $"ParseTree: \n%A{parseTree}"
    printfn "-----"
    printfn $"SymbolTable before running: \n%A{runtimeData._symbolTable}"
    printfn "-----"
    printfn $"Output: %A{snd out}"
    printfn "-----"
    printfn $"Finished Symbol Table: \n%A{(fst out)._symbolTable}"
    0
