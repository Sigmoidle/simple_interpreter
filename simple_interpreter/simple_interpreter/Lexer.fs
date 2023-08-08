module simple_interpreter.Lexer

open System
open System.IO

// All Apl tokens
type Token =
    | Plus // + 
    | Hyphen // - 
    | Division // /
    | Multiplication // *
    | LessThan // < 
    | Equals // =
    | GreaterThan // > 
    | LessOrEqual
    | GreaterOrEqual
    | NotEqual
    | LeftBracket // (
    | RightBracket // )
    | Assign // == 
    // Misc
    | Comment // ⍝
    | NewLine // \n
    // Types
    | Number of float
    //| String of string
    // Identifiers
    | Identifier of string
    // End of file
    | EndOfFile
    // Statements
    | If
    | Else
    | End
    | While
    | EndWhile

let private isIndicationOfArray t =
    match t with
    | Token.Identifier _ -> true
    | Token.RightBracket -> true
    | _ -> false

let private isNewLine c = c = '\n'

let private isLetter c = Char.IsLetter c

let private isBlank c = Char.IsWhiteSpace c && not (c.Equals('\n'))

let private isDigit c = Char.IsDigit c

let rec private makeTokens (tokenList: Token list) characters =
    match characters with
    // Numbers
    | '-' :: digit :: tail when isDigit digit && tokenList.Head  ->
        let newRest, number = makeNumberToken 0.0 (digit :: tail)

        makeTokens (Number(-number) :: tokenList) newRest
    | digit :: tail when isDigit digit ->
        let newRest, number = makeNumberToken 0.0 (digit :: tail)

        makeTokens (Number(number) :: tokenList) newRest
    // Tokens
    | ':' :: 'W' :: 'h' :: 'i' :: 'l' :: 'e' :: tail -> makeTokens (While :: tokenList) tail
    | ':' :: 'E' :: 'n' :: 'd' :: 'W' :: 'h' :: 'i' :: 'l' :: 'e' :: tail -> makeTokens (EndWhile :: tokenList) tail
    | ':' :: 'I' :: 'f' :: tail -> makeTokens (If :: tokenList) tail
    | ':' :: 'E' :: 'l' :: 's' :: 'e' :: tail -> makeTokens (Else :: tokenList) tail
    | ':' :: 'E' :: 'n' :: 'd' :: tail -> makeTokens (End :: tokenList) tail
    | '=' :: '=' :: tail -> makeTokens (Assign :: tokenList) tail
    | '+' :: tail -> makeTokens (Plus :: tokenList) tail
    | '-' :: tail -> makeTokens (Hyphen :: tokenList) tail
    | '(' :: tail -> makeTokens (LeftBracket :: tokenList) tail
    | ')' :: tail -> makeTokens (RightBracket :: tokenList) tail
    | '*' :: tail -> makeTokens (Multiplication :: tokenList) tail
    | '/' :: tail -> makeTokens (Division :: tokenList) tail
    | '<' :: '=' :: tail -> makeTokens (LessOrEqual :: tokenList) tail
    | '<' :: tail -> makeTokens (LessThan :: tokenList) tail
    | '=' :: tail -> makeTokens (Equals :: tokenList) tail
    | '>' :: '=' :: tail -> makeTokens (GreaterOrEqual :: tokenList) tail
    | '>' :: tail -> makeTokens (GreaterThan :: tokenList) tail
    | '!' :: '=' :: tail -> makeTokens (NotEqual :: tokenList) tail
    // Identifiers
    | letter :: tail when isLetter letter ->
        let newRest, calculatedString = makeStringToken "" (letter :: tail)

        makeTokens (Identifier(calculatedString) :: tokenList) newRest
    // Whitespaces
    | whitespace :: tail when isBlank whitespace -> makeTokens tokenList tail
    // NewLines
    | newLine :: tail when isNewLine newLine -> makeTokens (NewLine :: tokenList) tail
    // Comments
    | '⍝' :: tail ->
        let newRest = handleComment tail

        makeTokens tokenList newRest
    // Empty character array
    | [] -> EndOfFile :: tokenList |> List.rev
    // Error, no matches
    | error :: _ -> failwith $"tokenization error at character: {error} | After token: {tokenList.Head}"

and calculateAfterDecimal float scale characters =
    match characters with
    | '0' :: tail -> calculateAfterDecimal (float * 10.0) (scale * 10.0) tail
    | '1' :: tail -> calculateAfterDecimal (float * 10.0 + 1.0) (scale * 10.0) tail
    | '2' :: tail -> calculateAfterDecimal (float * 10.0 + 2.0) (scale * 10.0) tail
    | '3' :: tail -> calculateAfterDecimal (float * 10.0 + 3.0) (scale * 10.0) tail
    | '4' :: tail -> calculateAfterDecimal (float * 10.0 + 4.0) (scale * 10.0) tail
    | '5' :: tail -> calculateAfterDecimal (float * 10.0 + 5.0) (scale * 10.0) tail
    | '6' :: tail -> calculateAfterDecimal (float * 10.0 + 6.0) (scale * 10.0) tail
    | '7' :: tail -> calculateAfterDecimal (float * 10.0 + 7.0) (scale * 10.0) tail
    | '8' :: tail -> calculateAfterDecimal (float * 10.0 + 8.0) (scale * 10.0) tail
    | '9' :: tail -> calculateAfterDecimal (float * 10.0 + 9.0) (scale * 10.0) tail
    // Empty character array
    | [] -> ([], float / scale)
    // Finished finding numbers
    | _ -> (characters, float / scale)

and makeNumberToken float characters =
    match characters with
    // Number characters
    | '0' :: tail -> makeNumberToken (float * 10.0) tail
    | '1' :: tail -> makeNumberToken (float * 10.0 + 1.0) tail
    | '2' :: tail -> makeNumberToken (float * 10.0 + 2.0) tail
    | '3' :: tail -> makeNumberToken (float * 10.0 + 3.0) tail
    | '4' :: tail -> makeNumberToken (float * 10.0 + 4.0) tail
    | '5' :: tail -> makeNumberToken (float * 10.0 + 5.0) tail
    | '6' :: tail -> makeNumberToken (float * 10.0 + 6.0) tail
    | '7' :: tail -> makeNumberToken (float * 10.0 + 7.0) tail
    | '8' :: tail -> makeNumberToken (float * 10.0 + 8.0) tail
    | '9' :: tail -> makeNumberToken (float * 10.0 + 9.0) tail
    // Detect and calculate float number
    | '.' :: digit :: tail when isDigit digit ->
        let newRest, number = calculateAfterDecimal 0.0 1.0 (digit :: tail)

        makeNumberToken (float + number) newRest
    // Empty character array
    | [] -> ([], float)
    // Finished finding numbers
    | _ -> (characters, float)

and handleComment characters =
    match characters with
    | newline :: tail when isNewLine newline -> newline :: tail
    | [] -> characters
    | _ :: tail -> handleComment tail

and makeStringToken (calculatedString: string) characters =
    match characters with
    | letter :: tail when isLetter letter || letter = '_' -> makeStringToken (calculatedString + string letter) tail
    | [] -> ([], calculatedString)
    | _ -> (characters, calculatedString)

let public lex (inputString: string) = inputString |> Seq.toList |> makeTokens []

let public testLexer =
    let aplProgram = File.ReadAllText("test_program.apl")

    aplProgram |> lex |> printfn "%A"
