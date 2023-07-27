module simple_interpreter.Lexer

open System
open System.IO

// All Apl tokens
type Token =
    | AddReduce // +/ Gets the sum of the vector to the right
    | DivideReduce // ÷/ Adds division between each element in the vector on the right to get a result
    | MultiplyReduce // ×/ Gets the product of the vector to the right
    | SubtractReduce // -/ Adds subtraction between each element in the vector on the right to get a result
    | Plus // + Dyadic, returning the result of adding 2 arguments | Monadic, returns the complex congugate of the argument. Real numbers are returned unchanged
    | Hyphen // - Dyadic, returns the result of subtracting 2 arguments | Monadic, changes the sign of the argument
    | Multiplication // × Dyadic, returns the result of multiplying 2 arguments | Monadic, returns the sign on the argument. -1 if A<0, 0 if A=0, 1 if A>0
    | Division // ÷ Dyadic, returns the result of dividing 2 arguments | Monadic, returns the reciprical of the argument
    | Tally // ≢ Monadic, returns the length of a numeric vector or string.
    | Select // ⊇ Dyadic, returns multiple items from its right argument using a vector of indicies as the left argument
    | LeftCeiling // ⌈
    | LeftFloor // ⌊
    | Asterisk // *
    //| CircleStar // ⍟
    | VerticalBar // | Dyadic, returns the remainder of the division of both arguments | Monadic, returns the absolute of the argument
    | QuestionMark // ? Dyadic, A?B, returns A distinct integers selected randomly from the first B integers | Monadic, ?B, returns 1 integer selected randomly from the first B integers
    //| WhiteCircle // ○
    //| ExclamationMark // !
    | Tilde // ~ Monadic, returns not of argument e.g ~1 = 0, ~0 = 1
    | LogicalAnd // ∧ Dyadic, returns 1 (true) if both A and B = 1, 0 (false) otherwise
    | LogicalOr // ∨ Dyadic, returns 1 (true) if A or B = 1, 0 (false) otherwise
    | LogicalNand // ⍲ Dyadic, returns 0 if A and B = 1, 1 otherwise
    | LogicalNor // ⍱ Dyadic, returns 1 if A and B = 0, 1 otherwise
    | LessThan // < Dyadic, returns 1 if A is less than B, 0 otherwise
    | LessOrEqual // ≤ Dyadic, returns 1 if A is less or equal to B, 0 otherwise
    | Equals // = Dyadic, returns 1 if A is equal to B, 0 otherwise
    | GreaterOrEqual // ≥ Dyadic, returns 1 if A is greater or equal to B, 0 otherwise
    | GreaterThan // > Dyadic, returns 1 if A is greater than B, 0 otherwise
    | NotEqual // ≠ Dyadic, returns 1 if A is not equal to B, 0 otherwise
    //| Rho // ⍴
    | Comma // , Dyadic, returns the result of appending both arguments | Monadic, changes the arugment into a vector
    //| LeftSquareBracket // [
    //| RightSquareBracket // ]
    | Iota // ⍳ Dyadic, A⍳B returns the index of B in A | Monadic, returns the vector of the first B integers
    | Range // … Dyadic, returns a vector of consecutive integers based on the start and end values given by its arguments
    //| UpwardPointingArrow // ↑
    //| DownwardPointingArrow // ↓
    | GradeUp // ⍋ Monadic, returns the rearrangement of the argument in ascending order
    | GradeDown // ⍒ Monadic, returns the rearrangement of the argument in descending order
    | Slash // / Used with Dyadic operators to perform the operator on the entire vector, e.g Sum
    //| Backslash // \
    //| BackslashBar // ⍀
    //| CircleStile // ⌽
    //| CircledMinus // ⊖
    //| CircleBackslash // ⍉
    | SmallElementOf // ∊
    //| Decode // ⊥
    //| Encode // ⊤
    //| FullStop // .
    //| OuterProduct // ∘.
    | LeftBracket // (
    | RightBracket // )
    | Assign // ← Dyadic, assigns the value on the right to the variable to the left
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

let rec private makeTokens tokenList characters =
    match characters with
    // Tokens
    | ':' :: 'W' :: 'h' :: 'i' :: 'l' :: 'e' :: tail -> makeTokens (While :: tokenList) tail
    | ':' :: 'E' :: 'n' :: 'd' :: 'W' :: 'h' :: 'i' :: 'l' :: 'e' :: tail -> makeTokens (EndWhile :: tokenList) tail
    | ':' :: 'I' :: 'f' :: tail -> makeTokens (If :: tokenList) tail
    | ':' :: 'E' :: 'l' :: 's' :: 'e' :: tail -> makeTokens (Else :: tokenList) tail
    | ':' :: 'E' :: 'n' :: 'd' :: tail -> makeTokens (End :: tokenList) tail
    | '×' :: '/' :: tail -> makeTokens (MultiplyReduce :: tokenList) tail
    | '÷' :: '/' :: tail -> makeTokens (DivideReduce :: tokenList) tail
    | '+' :: '/' :: tail -> makeTokens (AddReduce :: tokenList) tail
    | '-' :: '/' :: tail -> makeTokens (SubtractReduce :: tokenList) tail
    | '←' :: tail -> makeTokens (Assign :: tokenList) tail
    | '+' :: tail -> makeTokens (Plus :: tokenList) tail
    | '-' :: tail -> makeTokens (Hyphen :: tokenList) tail
    | '~' :: tail -> makeTokens (Tilde :: tokenList) tail
    | '?' :: tail -> makeTokens (QuestionMark :: tokenList) tail
    | '(' :: tail -> makeTokens (LeftBracket :: tokenList) tail
    | ')' :: tail -> makeTokens (RightBracket :: tokenList) tail
    | '×' :: tail -> makeTokens (Multiplication :: tokenList) tail
    | '÷' :: tail -> makeTokens (Division :: tokenList) tail
    | '≢' :: tail -> makeTokens (Tally :: tokenList) tail
    | '…' :: tail -> makeTokens (Range :: tokenList) tail
    | '⍳' :: tail -> makeTokens (Iota :: tokenList) tail
    | '⊇' :: tail -> makeTokens (Select :: tokenList) tail
    | '⍋' :: tail -> makeTokens (GradeUp :: tokenList) tail
    | '⍒' :: tail -> makeTokens (GradeDown :: tokenList) tail
    | '∧' :: tail -> makeTokens (LogicalAnd :: tokenList) tail
    | '∨' :: tail -> makeTokens (LogicalOr :: tokenList) tail
    | '⍲' :: tail -> makeTokens (LogicalNand :: tokenList) tail
    | '⍱' :: tail -> makeTokens (LogicalNor :: tokenList) tail
    | '<' :: tail -> makeTokens (LessThan :: tokenList) tail
    | '≤' :: tail -> makeTokens (LessOrEqual :: tokenList) tail
    | '=' :: tail -> makeTokens (Equals :: tokenList) tail
    | '≥' :: tail -> makeTokens (GreaterOrEqual :: tokenList) tail
    | '>' :: tail -> makeTokens (GreaterThan :: tokenList) tail
    | '≠' :: tail -> makeTokens (NotEqual :: tokenList) tail
    | '|' :: tail -> makeTokens (VerticalBar :: tokenList) tail
    | '⌊' :: tail -> makeTokens (LeftFloor :: tokenList) tail
    | ',' :: tail -> makeTokens (Comma :: tokenList) tail
    | '∊' :: tail -> makeTokens (SmallElementOf :: tokenList) tail
    | '⌈' :: tail -> makeTokens (LeftCeiling :: tokenList) tail
    | '*' :: tail -> makeTokens (Asterisk :: tokenList) tail
    // Identifiers
    | letter :: tail when isLetter letter ->
        let newRest, calculatedString = makeStringToken "" (letter :: tail)

        makeTokens (Identifier(calculatedString) :: tokenList) newRest
    // Numbers
    | '¯' :: digit :: tail when isDigit digit ->
        let newRest, number = makeNumberToken 0.0 (digit :: tail)

        makeTokens (Number(-number) :: tokenList) newRest
    | digit :: tail when isDigit digit ->
        let newRest, number = makeNumberToken 0.0 (digit :: tail)

        makeTokens (Number(number) :: tokenList) newRest
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
