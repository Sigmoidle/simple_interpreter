module simple_interpreter.Symbols

open Parser

type RuntimeData = { _program: Program; _symbolTable: Map<string, float list> }

let createSymbols (program: Program) : RuntimeData =

    let originalProgram = program

    let rec _Program (data: RuntimeData) =
        match data._program with
        | Program.EndOfProgram -> { data with _program = originalProgram }
        | Program.NewLine newProgram -> _Program { data with _program = newProgram }
        | Program.Statement (statement, program) ->
            let newSymbolTable = _Statement (statement, data._symbolTable)
            _Program { data with _program = program; _symbolTable = newSymbolTable }
        | Program.Expression (expression, program) ->
            let newSymbolTable = _Expression (expression, data._symbolTable)
            _Program { data with _program = program; _symbolTable = newSymbolTable }

    and _Statement (statement, symbolTable) =
        match statement with
        | IfElse (expression, program1, program2) ->
            let newSymbolTable = _Expression (expression, symbolTable)
            let data = _Program { _program = program1; _symbolTable = newSymbolTable }
            let newData = _Program { _program = program2; _symbolTable = data._symbolTable }
            newData._symbolTable
        | While (expression, program) ->
            let newSymbolTable = _Expression (expression, symbolTable)
            let data = _Program { _program = program; _symbolTable = newSymbolTable }
            data._symbolTable

    and _Expression (expression, symbolTable) =
        match expression with
        | Expression.Assign (symbolName, _) -> symbolTable.Add(symbolName, [ 0 ])
        | _ -> symbolTable


    _Program { _program = program; _symbolTable = Map.empty }
