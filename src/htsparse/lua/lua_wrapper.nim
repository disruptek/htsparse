import
  hmisc / wrappers/treesitter,
  hmisc / core/all,
  hmisc / types/colorstring,
  std/strutils
export treesitter
type
  LuaNodeKind* = enum
    luaDeclaration                          ## declaration
    luaExpression                           ## expression
    luaStatement                            ## statement
    luaVariable                             ## variable
    luaArguments                            ## arguments
    luaAssignmentStatement                  ## assignment_statement
    luaAttribute                            ## attribute
    luaBinaryExpression                     ## binary_expression
    luaBlock                                ## block
    luaBracketIndexExpression               ## bracket_index_expression
    luaChunk                                ## chunk
    luaComment                              ## comment
    luaDoStatement                          ## do_statement
    luaDotIndexExpression                   ## dot_index_expression
    luaElseStatement                        ## else_statement
    luaElseifStatement                      ## elseif_statement
    luaEmptyStatement                       ## empty_statement
    luaExpressionList                       ## expression_list
    luaField                                ## field
    luaForGenericClause                     ## for_generic_clause
    luaForNumericClause                     ## for_numeric_clause
    luaForStatement                         ## for_statement
    luaFunctionCall                         ## function_call
    luaFunctionDeclaration                  ## function_declaration
    luaFunctionDefinition                   ## function_definition
    luaGotoStatement                        ## goto_statement
    luaIfStatement                          ## if_statement
    luaLabelStatement                       ## label_statement
    luaMethodIndexExpression                ## method_index_expression
    luaParameters                           ## parameters
    luaParenthesizedExpression              ## parenthesized_expression
    luaRepeatStatement                      ## repeat_statement
    luaReturnStatement                      ## return_statement
    luaString                               ## string
    luaStringContent                        ## string_content
    luaTableConstructor                     ## table_constructor
    luaUnaryExpression                      ## unary_expression
    luaVariableDeclaration                  ## variable_declaration
    luaVariableList                         ## variable_list
    luaWhileStatement                       ## while_statement
    luaQuoteTok                             ## "
    luaHashTok                              ## #
    luaPercentTok                           ## %
    luaAmpersandTok                         ## &
    luaApostropheTok                        ## '
    luaLParTok                              ## (
    luaRParTok                              ## )
    luaAsteriskTok                          ## *
    luaPlusTok                              ## +
    luaCommaTok                             ## ,
    luaMinusTok                             ## -
    luaDoubleMinusTok                       ## --
    luaDotTok                               ## .
    luaDoubleDotTok                         ## ..
    luaSlashTok                             ## /
    luaDoubleSlashTok                       ## //
    luaColonTok                             ## :
    luaDoubleColonTok                       ## ::
    luaSemicolonTok                         ## ;
    luaLessThanTok                          ## <
    luaDoubleLessThanTok                    ## <<
    luaLessThanEqualTok                     ## <=
    luaEqualTok                             ## =
    luaDoubleEqualTok                       ## ==
    luaGreaterThanTok                       ## >
    luaGreaterThanEqualTok                  ## >=
    luaDoubleGreaterThanTok                 ## >>
    luaLBrackTok                            ## [
    luaDoubleLBrackTok                      ## [[
    luaRBrackTok                            ## ]
    luaDoubleRBrackTok                      ## ]]
    luaAccentTok                            ## ^
    luaAndTok                               ## and
    luaBreakStatement                       ## break_statement
    luaCommentContent                       ## comment_content
    luaDoTok                                ## do
    luaElseTok                              ## else
    luaElseifTok                            ## elseif
    luaEndTok                               ## end
    luaEscapeSequence                       ## escape_sequence
    luaFalse                                ## false
    luaForTok                               ## for
    luaFunctionTok                          ## function
    luaGotoTok                              ## goto
    luaHashBangLine                         ## hash_bang_line
    luaIdentifier                           ## identifier
    luaIfTok                                ## if
    luaInTok                                ## in
    luaLocalTok                             ## local
    luaNil                                  ## nil
    luaNotTok                               ## not
    luaNumber                               ## number
    luaOrTok                                ## or
    luaRepeatTok                            ## repeat
    luaReturnTok                            ## return
    luaThenTok                              ## then
    luaTrue                                 ## true
    luaUntilTok                             ## until
    luaVarargExpression                     ## vararg_expression
    luaWhileTok                             ## while
    luaLCurlyTok                            ## {
    luaPipeTok                              ## |
    luaRCurlyTok                            ## }
    luaTildeTok                             ## ~
    luaTildeEqualTok                        ## ~=
    luaHidBlockCommentEnd                   ## _block_comment_end
    luaHidDoublequoteStringContent          ## _doublequote_string_content
    luaHidExpressionList                    ## _expression_list
    luaHidLocalFunctionDeclaration          ## _local_function_declaration
    luaHidParameterList                     ## _parameter_list
    luaHidBlockStringStart                  ## _block_string_start
    luaHidNameList                          ## _name_list
    luaHidPrefixExpression                  ## _prefix_expression
    luaHidFieldSep                          ## _field_sep
    luaHidFunctionName                      ## _function_name
    luaHidBlockCommentStart                 ## _block_comment_start
    luaHidFunctionNameMethodIndexExpression ## _function_name_method_index_expression
    luaHidAttrib                            ## _attrib
    luaHidVariableAssignmentVarlist         ## _variable_assignment_varlist
    luaHidAttNameList                       ## _att_name_list
    luaHidFunctionNamePrefixExpression      ## _function_name_prefix_expression
    luaHidQuoteString                       ## _quote_string
    luaHidBlock                             ## _block
    luaHidBlockString                       ## _block_string
    luaHidFunctionNameDotIndexExpression    ## _function_name_dot_index_expression
    luaHidFieldList                         ## _field_list
    luaHidBlockStringEnd                    ## _block_string_end
    luaHidBlockCommentContent               ## _block_comment_content
    luaHidSinglequoteStringContent          ## _singlequote_string_content
    luaHidLocalVariableAssignment           ## _local_variable_assignment
    luaHidFunctionBody                      ## _function_body
    luaHidVariableAssignmentExplist         ## _variable_assignment_explist
    luaHidBlockStringContent                ## _block_string_content
    luaSyntaxError                          ## Tree-sitter parser syntax error

proc strRepr*(kind: LuaNodeKind): string =
  case kind:
    of luaDeclaration:                          "declaration"
    of luaExpression:                           "expression"
    of luaStatement:                            "statement"
    of luaVariable:                             "variable"
    of luaArguments:                            "arguments"
    of luaAssignmentStatement:                  "assignment_statement"
    of luaAttribute:                            "attribute"
    of luaBinaryExpression:                     "binary_expression"
    of luaBlock:                                "block"
    of luaBracketIndexExpression:               "bracket_index_expression"
    of luaChunk:                                "chunk"
    of luaComment:                              "comment"
    of luaDoStatement:                          "do_statement"
    of luaDotIndexExpression:                   "dot_index_expression"
    of luaElseStatement:                        "else_statement"
    of luaElseifStatement:                      "elseif_statement"
    of luaEmptyStatement:                       "empty_statement"
    of luaExpressionList:                       "expression_list"
    of luaField:                                "field"
    of luaForGenericClause:                     "for_generic_clause"
    of luaForNumericClause:                     "for_numeric_clause"
    of luaForStatement:                         "for_statement"
    of luaFunctionCall:                         "function_call"
    of luaFunctionDeclaration:                  "function_declaration"
    of luaFunctionDefinition:                   "function_definition"
    of luaGotoStatement:                        "goto_statement"
    of luaIfStatement:                          "if_statement"
    of luaLabelStatement:                       "label_statement"
    of luaMethodIndexExpression:                "method_index_expression"
    of luaParameters:                           "parameters"
    of luaParenthesizedExpression:              "parenthesized_expression"
    of luaRepeatStatement:                      "repeat_statement"
    of luaReturnStatement:                      "return_statement"
    of luaString:                               "string"
    of luaStringContent:                        "string_content"
    of luaTableConstructor:                     "table_constructor"
    of luaUnaryExpression:                      "unary_expression"
    of luaVariableDeclaration:                  "variable_declaration"
    of luaVariableList:                         "variable_list"
    of luaWhileStatement:                       "while_statement"
    of luaQuoteTok:                             "\""
    of luaHashTok:                              "#"
    of luaPercentTok:                           "%"
    of luaAmpersandTok:                         "&"
    of luaApostropheTok:                        "\'"
    of luaLParTok:                              "("
    of luaRParTok:                              ")"
    of luaAsteriskTok:                          "*"
    of luaPlusTok:                              "+"
    of luaCommaTok:                             ","
    of luaMinusTok:                             "-"
    of luaDoubleMinusTok:                       "--"
    of luaDotTok:                               "."
    of luaDoubleDotTok:                         ".."
    of luaSlashTok:                             "/"
    of luaDoubleSlashTok:                       "//"
    of luaColonTok:                             ":"
    of luaDoubleColonTok:                       "::"
    of luaSemicolonTok:                         ";"
    of luaLessThanTok:                          "<"
    of luaDoubleLessThanTok:                    "<<"
    of luaLessThanEqualTok:                     "<="
    of luaEqualTok:                             "="
    of luaDoubleEqualTok:                       "=="
    of luaGreaterThanTok:                       ">"
    of luaGreaterThanEqualTok:                  ">="
    of luaDoubleGreaterThanTok:                 ">>"
    of luaLBrackTok:                            "["
    of luaDoubleLBrackTok:                      "[["
    of luaRBrackTok:                            "]"
    of luaDoubleRBrackTok:                      "]]"
    of luaAccentTok:                            "^"
    of luaAndTok:                               "and"
    of luaBreakStatement:                       "break_statement"
    of luaCommentContent:                       "comment_content"
    of luaDoTok:                                "do"
    of luaElseTok:                              "else"
    of luaElseifTok:                            "elseif"
    of luaEndTok:                               "end"
    of luaEscapeSequence:                       "escape_sequence"
    of luaFalse:                                "false"
    of luaForTok:                               "for"
    of luaFunctionTok:                          "function"
    of luaGotoTok:                              "goto"
    of luaHashBangLine:                         "hash_bang_line"
    of luaIdentifier:                           "identifier"
    of luaIfTok:                                "if"
    of luaInTok:                                "in"
    of luaLocalTok:                             "local"
    of luaNil:                                  "nil"
    of luaNotTok:                               "not"
    of luaNumber:                               "number"
    of luaOrTok:                                "or"
    of luaRepeatTok:                            "repeat"
    of luaReturnTok:                            "return"
    of luaThenTok:                              "then"
    of luaTrue:                                 "true"
    of luaUntilTok:                             "until"
    of luaVarargExpression:                     "vararg_expression"
    of luaWhileTok:                             "while"
    of luaLCurlyTok:                            "{"
    of luaPipeTok:                              "|"
    of luaRCurlyTok:                            "}"
    of luaTildeTok:                             "~"
    of luaTildeEqualTok:                        "~="
    of luaHidBlockCommentEnd:                   "_block_comment_end"
    of luaHidDoublequoteStringContent:          "_doublequote_string_content"
    of luaHidExpressionList:                    "_expression_list"
    of luaHidLocalFunctionDeclaration:          "_local_function_declaration"
    of luaHidParameterList:                     "_parameter_list"
    of luaHidBlockStringStart:                  "_block_string_start"
    of luaHidNameList:                          "_name_list"
    of luaHidPrefixExpression:                  "_prefix_expression"
    of luaHidFieldSep:                          "_field_sep"
    of luaHidFunctionName:                      "_function_name"
    of luaHidBlockCommentStart:                 "_block_comment_start"
    of luaHidFunctionNameMethodIndexExpression: "_function_name_method_index_expression"
    of luaHidAttrib:                            "_attrib"
    of luaHidVariableAssignmentVarlist:         "_variable_assignment_varlist"
    of luaHidAttNameList:                       "_att_name_list"
    of luaHidFunctionNamePrefixExpression:      "_function_name_prefix_expression"
    of luaHidQuoteString:                       "_quote_string"
    of luaHidBlock:                             "_block"
    of luaHidBlockString:                       "_block_string"
    of luaHidFunctionNameDotIndexExpression:    "_function_name_dot_index_expression"
    of luaHidFieldList:                         "_field_list"
    of luaHidBlockStringEnd:                    "_block_string_end"
    of luaHidBlockCommentContent:               "_block_comment_content"
    of luaHidSinglequoteStringContent:          "_singlequote_string_content"
    of luaHidLocalVariableAssignment:           "_local_variable_assignment"
    of luaHidFunctionBody:                      "_function_body"
    of luaHidVariableAssignmentExplist:         "_variable_assignment_explist"
    of luaHidBlockStringContent:                "_block_string_content"
    of luaSyntaxError:                          "ERROR"

type
  LuaExternalTok* = enum
    luaExtern_block_comment_start   ## _block_comment_start
    luaExtern_block_comment_content ## _block_comment_content
    luaExtern_block_comment_end     ## _block_comment_end
    luaExtern_block_string_start    ## _block_string_start
    luaExtern_block_string_content  ## _block_string_content
    luaExtern_block_string_end      ## _block_string_end

type
  TsLuaNode* = distinct TSNode

type
  LuaParser* = distinct PtsParser

const luaAllowedSubnodes*: array[LuaNodeKind, set[LuaNodeKind]] = block:
                                                                    var tmp: array[LuaNodeKind, set[LuaNodeKind]]
                                                                    tmp[luaArguments] = {luaExpression}
                                                                    tmp[luaAssignmentStatement] = {luaExpressionList, luaVariableList}
                                                                    tmp[luaAttribute] = {luaIdentifier}
                                                                    tmp[luaBlock] = {luaReturnStatement, luaStatement}
                                                                    tmp[luaChunk] = {luaHashBangLine, luaReturnStatement, luaStatement}
                                                                    tmp[luaExpressionList] = {luaExpression}
                                                                    tmp[luaForGenericClause] = {luaExpressionList, luaVariableList}
                                                                    tmp[luaGotoStatement] = {luaIdentifier}
                                                                    tmp[luaLabelStatement] = {luaIdentifier}
                                                                    tmp[luaParameters] = {luaVarargExpression}
                                                                    tmp[luaParenthesizedExpression] = {luaExpression}
                                                                    tmp[luaReturnStatement] = {luaExpressionList}
                                                                    tmp[luaStringContent] = {luaEscapeSequence}
                                                                    tmp[luaTableConstructor] = {luaField}
                                                                    tmp[luaVariableDeclaration] = {luaAssignmentStatement, luaVariableList}
                                                                    tmp
const luaTokenKinds*: set[LuaNodeKind] = {
                                           luaQuoteTok,
                                           luaHashTok,
                                           luaPercentTok,
                                           luaAmpersandTok,
                                           luaApostropheTok,
                                           luaLParTok,
                                           luaRParTok,
                                           luaAsteriskTok,
                                           luaPlusTok,
                                           luaCommaTok,
                                           luaMinusTok,
                                           luaDoubleMinusTok,
                                           luaDotTok,
                                           luaDoubleDotTok,
                                           luaSlashTok,
                                           luaDoubleSlashTok,
                                           luaColonTok,
                                           luaDoubleColonTok,
                                           luaSemicolonTok,
                                           luaLessThanTok,
                                           luaDoubleLessThanTok,
                                           luaLessThanEqualTok,
                                           luaEqualTok,
                                           luaDoubleEqualTok,
                                           luaGreaterThanTok,
                                           luaGreaterThanEqualTok,
                                           luaDoubleGreaterThanTok,
                                           luaLBrackTok,
                                           luaDoubleLBrackTok,
                                           luaRBrackTok,
                                           luaDoubleRBrackTok,
                                           luaAccentTok,
                                           luaAndTok,
                                           luaDoTok,
                                           luaElseTok,
                                           luaElseifTok,
                                           luaEndTok,
                                           luaForTok,
                                           luaFunctionTok,
                                           luaGotoTok,
                                           luaIfTok,
                                           luaInTok,
                                           luaLocalTok,
                                           luaNotTok,
                                           luaOrTok,
                                           luaRepeatTok,
                                           luaReturnTok,
                                           luaThenTok,
                                           luaUntilTok,
                                           luaWhileTok,
                                           luaLCurlyTok,
                                           luaPipeTok,
                                           luaRCurlyTok,
                                           luaTildeTok,
                                           luaTildeEqualTok
                                         }
const luaHiddenKinds*: set[LuaNodeKind] = {
                                            luaHidBlockCommentEnd,
                                            luaHidDoublequoteStringContent,
                                            luaHidExpressionList,
                                            luaHidLocalFunctionDeclaration,
                                            luaHidParameterList,
                                            luaHidBlockStringStart,
                                            luaHidNameList,
                                            luaHidPrefixExpression,
                                            luaHidFieldSep,
                                            luaHidFunctionName,
                                            luaHidBlockCommentStart,
                                            luaHidFunctionNameMethodIndexExpression,
                                            luaHidAttrib,
                                            luaHidVariableAssignmentVarlist,
                                            luaHidAttNameList,
                                            luaHidFunctionNamePrefixExpression,
                                            luaHidQuoteString,
                                            luaHidBlock,
                                            luaHidBlockString,
                                            luaHidFunctionNameDotIndexExpression,
                                            luaHidFieldList,
                                            luaHidBlockStringEnd,
                                            luaHidBlockCommentContent,
                                            luaHidSinglequoteStringContent,
                                            luaHidLocalVariableAssignment,
                                            luaHidFunctionBody,
                                            luaHidVariableAssignmentExplist,
                                            luaHidBlockStringContent
                                          }
proc tsNodeType*(node: TsLuaNode): string


proc kind*(node: TsLuaNode): LuaNodeKind {.noSideEffect.} =
  {.cast(noSideEffect).}:
    case node.tsNodeType:
      of "declaration":              luaDeclaration
      of "expression":               luaExpression
      of "statement":                luaStatement
      of "variable":                 luaVariable
      of "arguments":                luaArguments
      of "assignment_statement":     luaAssignmentStatement
      of "attribute":                luaAttribute
      of "binary_expression":        luaBinaryExpression
      of "block":                    luaBlock
      of "bracket_index_expression": luaBracketIndexExpression
      of "chunk":                    luaChunk
      of "comment":                  luaComment
      of "do_statement":             luaDoStatement
      of "dot_index_expression":     luaDotIndexExpression
      of "else_statement":           luaElseStatement
      of "elseif_statement":         luaElseifStatement
      of "empty_statement":          luaEmptyStatement
      of "expression_list":          luaExpressionList
      of "field":                    luaField
      of "for_generic_clause":       luaForGenericClause
      of "for_numeric_clause":       luaForNumericClause
      of "for_statement":            luaForStatement
      of "function_call":            luaFunctionCall
      of "function_declaration":     luaFunctionDeclaration
      of "function_definition":      luaFunctionDefinition
      of "goto_statement":           luaGotoStatement
      of "if_statement":             luaIfStatement
      of "label_statement":          luaLabelStatement
      of "method_index_expression":  luaMethodIndexExpression
      of "parameters":               luaParameters
      of "parenthesized_expression": luaParenthesizedExpression
      of "repeat_statement":         luaRepeatStatement
      of "return_statement":         luaReturnStatement
      of "string":                   luaString
      of "string_content":           luaStringContent
      of "table_constructor":        luaTableConstructor
      of "unary_expression":         luaUnaryExpression
      of "variable_declaration":     luaVariableDeclaration
      of "variable_list":            luaVariableList
      of "while_statement":          luaWhileStatement
      of "\"":                       luaQuoteTok
      of "#":                        luaHashTok
      of "%":                        luaPercentTok
      of "&":                        luaAmpersandTok
      of "\'":                       luaApostropheTok
      of "(":                        luaLParTok
      of ")":                        luaRParTok
      of "*":                        luaAsteriskTok
      of "+":                        luaPlusTok
      of ",":                        luaCommaTok
      of "-":                        luaMinusTok
      of "--":                       luaDoubleMinusTok
      of ".":                        luaDotTok
      of "..":                       luaDoubleDotTok
      of "/":                        luaSlashTok
      of "//":                       luaDoubleSlashTok
      of ":":                        luaColonTok
      of "::":                       luaDoubleColonTok
      of ";":                        luaSemicolonTok
      of "<":                        luaLessThanTok
      of "<<":                       luaDoubleLessThanTok
      of "<=":                       luaLessThanEqualTok
      of "=":                        luaEqualTok
      of "==":                       luaDoubleEqualTok
      of ">":                        luaGreaterThanTok
      of ">=":                       luaGreaterThanEqualTok
      of ">>":                       luaDoubleGreaterThanTok
      of "[":                        luaLBrackTok
      of "[[":                       luaDoubleLBrackTok
      of "]":                        luaRBrackTok
      of "]]":                       luaDoubleRBrackTok
      of "^":                        luaAccentTok
      of "and":                      luaAndTok
      of "break_statement":          luaBreakStatement
      of "comment_content":          luaCommentContent
      of "do":                       luaDoTok
      of "else":                     luaElseTok
      of "elseif":                   luaElseifTok
      of "end":                      luaEndTok
      of "escape_sequence":          luaEscapeSequence
      of "false":                    luaFalse
      of "for":                      luaForTok
      of "function":                 luaFunctionTok
      of "goto":                     luaGotoTok
      of "hash_bang_line":           luaHashBangLine
      of "identifier":               luaIdentifier
      of "if":                       luaIfTok
      of "in":                       luaInTok
      of "local":                    luaLocalTok
      of "nil":                      luaNil
      of "not":                      luaNotTok
      of "number":                   luaNumber
      of "or":                       luaOrTok
      of "repeat":                   luaRepeatTok
      of "return":                   luaReturnTok
      of "then":                     luaThenTok
      of "true":                     luaTrue
      of "until":                    luaUntilTok
      of "vararg_expression":        luaVarargExpression
      of "while":                    luaWhileTok
      of "{":                        luaLCurlyTok
      of "|":                        luaPipeTok
      of "}":                        luaRCurlyTok
      of "~":                        luaTildeTok
      of "~=":                       luaTildeEqualTok
      of "ERROR":                    luaSyntaxError
      else:
        raiseAssert("Invalid element name \'" & node.tsNodeType & "\'")

func isNil*(node: TsLuaNode): bool =
  ts_node_is_null(TSNode(node))

func len*(node: TsLuaNode, unnamed: bool = false): int =
  if unnamed:
    int(ts_node_child_count(TSNode(node)))

  else:
    int(ts_node_named_child_count(TSNode(node)))


func has*(node: TsLuaNode, idx: int, unnamed: bool = false): bool =
  0 <= idx and idx < node.len(unnamed)

proc tree_sitter_lua(): PtsLanguage {.importc, cdecl.}


proc tsNodeType*(node: TsLuaNode): string =
  $ts_node_type(TSNode(node))

proc newTsLuaParser*(): LuaParser =
  result = LuaParser(ts_parser_new())
  discard ts_parser_set_language(PtsParser(result), tree_sitter_lua())

proc parseString*(parser: LuaParser, str: string): TsLuaNode =
  TsLuaNode(ts_tree_root_node(ts_parser_parse_string(PtsParser(parser), nil, str.cstring, uint32(len(str)))))

proc parseTsLuaString*(str: string): TsLuaNode =
  let parser = newTsLuaParser()
  return parseString(parser, str)

func `$`*(node: TsLuaNode): string =
  if isNil(node):
    "<nil tree>"

  else:
    $node.kind


func `[]`*(
    node: TsLuaNode,
    idx:  int,
    kind: LuaNodeKind | set[LuaNodeKind]
  ): TsLuaNode =
  assert 0 <= idx and idx < node.len
  result = TsLuaNode(ts_node_named_child(TSNode(node), uint32(idx)))
  assertKind(result, kind, "Child node at index " & $idx & " for node kind " & $node.kind)

type
  LuaNode* = HtsNode[TsLuaNode, LuaNodeKind]


proc treeReprTsLua*(str: string, unnamed: bool = false): ColoredText =
  treeRepr[TsLuaNode, LuaNodeKind](parseTsLuaString(str), str, 3, unnamed = unnamed)

proc toHtsNode*(
    node: TsLuaNode,
    str:  ptr string
  ): HtsNode[TsLuaNode, LuaNodeKind] =
  toHtsNode[TsLuaNode, LuaNodeKind](node, str)

proc toHtsTree*(node: TsLuaNode, str: ptr string): LuaNode =
  toHtsNode[TsLuaNode, LuaNodeKind](node, str)

proc parseLuaString*(str: ptr string, unnamed: bool = false): LuaNode =
  let parser = newTsLuaParser()
  return toHtsTree[TsLuaNode, LuaNodeKind](parseString(parser, str[]), str)

proc parseLuaString*(str: string, unnamed: bool = false): LuaNode =
  let parser = newTsLuaParser()
  return toHtsTree[TsLuaNode, LuaNodeKind](parseString(parser, str), unsafeAddr str, storePtr = false)


import
  hmisc / wrappers/treesitter_core
let luaGrammar*: array[LuaNodeKind, HtsRule[LuaNodeKind]] = block:
                                                              var rules: array[LuaNodeKind, HtsRule[LuaNodeKind]]
                                                              type
                                                                K = LuaNodeKind


                                                              rules[luaHidDoublequoteStringContent] = tsRepeat1[K](tsChoice[K](tsRegex[K]("[^\"\\\\]+"), tsSymbol[K](luaEscapeSequence)))
                                                              rules[luaHidExpressionList] = tsSeq[K](tsSymbol[K](luaExpression), tsRepeat[K](tsSeq[K](tsString[K](","), tsSymbol[K](luaExpression))))
                                                              rules[luaLabelStatement] = tsSeq[K](tsString[K]("::"), tsSymbol[K](luaIdentifier), tsString[K]("::"))
                                                              rules[luaHidLocalFunctionDeclaration] = tsSeq[K](tsString[K]("local"), tsString[K]("function"), tsSymbol[K](luaIdentifier), tsSymbol[K](luaHidFunctionBody))
                                                              rules[luaFunctionDeclaration] = tsSeq[K](tsString[K]("function"), tsSymbol[K](luaHidFunctionName), tsSymbol[K](luaHidFunctionBody))
                                                              rules[luaWhileStatement] = tsSeq[K](tsString[K]("while"), tsSymbol[K](luaExpression), tsString[K]("do"), tsChoice[K](tsSymbol[K](luaHidBlock), tsBlank[K]()), tsString[K]("end"))
                                                              rules[luaHidNameList] = tsSeq[K](tsSymbol[K](luaIdentifier), tsRepeat[K](tsSeq[K](tsString[K](","), tsSymbol[K](luaIdentifier))))
                                                              rules[luaHidParameterList] = tsChoice[K](tsSeq[K](tsSeq[K](tsSymbol[K](luaIdentifier), tsRepeat[K](tsSeq[K](tsString[K](","), tsSymbol[K](luaIdentifier)))), tsChoice[K](tsSeq[K](tsString[K](","), tsSymbol[K](luaVarargExpression)), tsBlank[K]())), tsSymbol[K](luaVarargExpression))
                                                              rules[luaHidPrefixExpression] = tsChoice[K](tsSymbol[K](luaVariable), tsSymbol[K](luaFunctionCall), tsSymbol[K](luaParenthesizedExpression))
                                                              rules[luaBracketIndexExpression] = tsSeq[K](tsSymbol[K](luaHidPrefixExpression), tsString[K]("["), tsSymbol[K](luaExpression), tsString[K]("]"))
                                                              rules[luaHidFieldSep] = tsChoice[K](tsString[K](","), tsString[K](";"))
                                                              rules[luaDotIndexExpression] = tsSeq[K](tsSymbol[K](luaHidPrefixExpression), tsString[K]("."), tsSymbol[K](luaIdentifier))
                                                              rules[luaReturnStatement] = tsSeq[K](tsString[K]("return"), tsChoice[K](tsSymbol[K](luaHidExpressionList), tsBlank[K]()), tsChoice[K](tsString[K](";"), tsBlank[K]()))
                                                              rules[luaElseifStatement] = tsSeq[K](tsString[K]("elseif"), tsSymbol[K](luaExpression), tsString[K]("then"), tsChoice[K](tsSymbol[K](luaHidBlock), tsBlank[K]()))
                                                              rules[luaGotoStatement] = tsSeq[K](tsString[K]("goto"), tsSymbol[K](luaIdentifier))
                                                              rules[luaFalse] = tsString[K]("false")
                                                              rules[luaElseStatement] = tsSeq[K](tsString[K]("else"), tsChoice[K](tsSymbol[K](luaHidBlock), tsBlank[K]()))
                                                              rules[luaForStatement] = tsSeq[K](tsString[K]("for"), tsChoice[K](tsSymbol[K](luaForGenericClause), tsSymbol[K](luaForNumericClause)), tsString[K]("do"), tsChoice[K](tsSymbol[K](luaHidBlock), tsBlank[K]()), tsString[K]("end"))
                                                              rules[luaHidFunctionName] = tsChoice[K](tsSymbol[K](luaHidFunctionNamePrefixExpression), tsSymbol[K](luaHidFunctionNameMethodIndexExpression))
                                                              rules[luaBinaryExpression] = tsChoice[K](tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("or"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("and"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("<"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("<="), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("=="), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("~="), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K](">="), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K](">"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("|"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("~"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("&"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("<<"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K](">>"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("+"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("-"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("*"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("/"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("//"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("%"), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K](".."), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaExpression), tsString[K]("^"), tsSymbol[K](luaExpression)))
                                                              rules[luaString] = tsChoice[K](tsSymbol[K](luaHidQuoteString), tsSymbol[K](luaHidBlockString))
                                                              rules[luaDeclaration] = tsChoice[K](tsSymbol[K](luaFunctionDeclaration), tsSymbol[K](luaHidLocalFunctionDeclaration), tsSymbol[K](luaVariableDeclaration))
                                                              rules[luaHidFunctionNameMethodIndexExpression] = tsSeq[K](tsSymbol[K](luaHidFunctionNamePrefixExpression), tsString[K](":"), tsSymbol[K](luaIdentifier))
                                                              rules[luaHidAttrib] = tsSeq[K](tsString[K]("<"), tsSymbol[K](luaIdentifier), tsString[K](">"))
                                                              rules[luaHidVariableAssignmentVarlist] = tsSeq[K](tsSymbol[K](luaVariable), tsRepeat[K](tsSeq[K](tsString[K](","), tsSymbol[K](luaVariable))))
                                                              rules[luaBreakStatement] = tsString[K]("break")
                                                              rules[luaHidAttNameList] = tsSeq[K](tsSeq[K](tsSymbol[K](luaIdentifier), tsChoice[K](tsSymbol[K](luaHidAttrib), tsBlank[K]())), tsRepeat[K](tsSeq[K](tsString[K](","), tsSeq[K](tsSymbol[K](luaIdentifier), tsChoice[K](tsSymbol[K](luaHidAttrib), tsBlank[K]())))))
                                                              rules[luaMethodIndexExpression] = tsSeq[K](tsSymbol[K](luaHidPrefixExpression), tsString[K](":"), tsSymbol[K](luaIdentifier))
                                                              rules[luaTableConstructor] = tsSeq[K](tsString[K]("{"), tsChoice[K](tsSymbol[K](luaHidFieldList), tsBlank[K]()), tsString[K]("}"))
                                                              rules[luaEscapeSequence] = tsSeq[K](tsString[K]("\\"), tsChoice[K](tsRegex[K]("[\\nabfnrtv\\\\\'\"]"), tsRegex[K]("z\\s*"), tsRegex[K]("[0-9]{1,3}"), tsRegex[K]("x[0-9a-fA-F]{2}"), tsRegex[K]("u\\{[0-9a-fA-F]+\\}")))
                                                              rules[luaRepeatStatement] = tsSeq[K](tsString[K]("repeat"), tsChoice[K](tsSymbol[K](luaHidBlock), tsBlank[K]()), tsString[K]("until"), tsSymbol[K](luaExpression))
                                                              rules[luaHidFunctionNamePrefixExpression] = tsChoice[K](tsSymbol[K](luaIdentifier), tsSymbol[K](luaHidFunctionNameDotIndexExpression))
                                                              rules[luaIfStatement] = tsSeq[K](tsString[K]("if"), tsSymbol[K](luaExpression), tsString[K]("then"), tsChoice[K](tsSymbol[K](luaHidBlock), tsBlank[K]()), tsRepeat[K](tsSymbol[K](luaElseifStatement)), tsChoice[K](tsSymbol[K](luaElseStatement), tsBlank[K]()), tsString[K]("end"))
                                                              rules[luaFunctionDefinition] = tsSeq[K](tsString[K]("function"), tsSymbol[K](luaHidFunctionBody))
                                                              rules[luaHidQuoteString] = tsChoice[K](tsSeq[K](tsString[K]("\""), tsChoice[K](tsSymbol[K](luaHidDoublequoteStringContent), tsBlank[K]()), tsString[K]("\"")), tsSeq[K](tsString[K]("\'"), tsChoice[K](tsSymbol[K](luaHidSinglequoteStringContent), tsBlank[K]()), tsString[K]("\'")))
                                                              rules[luaUnaryExpression] = tsSeq[K](tsChoice[K](tsString[K]("not"), tsString[K]("#"), tsString[K]("-"), tsString[K]("~")), tsSymbol[K](luaExpression))
                                                              rules[luaHidBlock] = tsChoice[K](tsSeq[K](tsRepeat1[K](tsSymbol[K](luaStatement)), tsChoice[K](tsSymbol[K](luaReturnStatement), tsBlank[K]())), tsSeq[K](tsRepeat[K](tsSymbol[K](luaStatement)), tsSymbol[K](luaReturnStatement)))
                                                              rules[luaHidFunctionNameDotIndexExpression] = tsSeq[K](tsSymbol[K](luaHidFunctionNamePrefixExpression), tsString[K]("."), tsSymbol[K](luaIdentifier))
                                                              rules[luaExpression] = tsChoice[K](tsSymbol[K](luaNil), tsSymbol[K](luaFalse), tsSymbol[K](luaTrue), tsSymbol[K](luaNumber), tsSymbol[K](luaString), tsSymbol[K](luaVarargExpression), tsSymbol[K](luaFunctionDefinition), tsSymbol[K](luaVariable), tsSymbol[K](luaFunctionCall), tsSymbol[K](luaParenthesizedExpression), tsSymbol[K](luaTableConstructor), tsSymbol[K](luaBinaryExpression), tsSymbol[K](luaUnaryExpression))
                                                              rules[luaNil] = tsString[K]("nil")
                                                              rules[luaHidBlockString] = tsSeq[K](tsSymbol[K](luaHidBlockStringStart), tsSymbol[K](luaHidBlockStringContent), tsSymbol[K](luaHidBlockStringEnd))
                                                              rules[luaHidFieldList] = tsSeq[K](tsSymbol[K](luaField), tsRepeat[K](tsSeq[K](tsSymbol[K](luaHidFieldSep), tsSymbol[K](luaField))), tsChoice[K](tsSymbol[K](luaHidFieldSep), tsBlank[K]()))
                                                              rules[luaField] = tsChoice[K](tsSeq[K](tsString[K]("["), tsSymbol[K](luaExpression), tsString[K]("]"), tsString[K]("="), tsSymbol[K](luaExpression)), tsSeq[K](tsSymbol[K](luaIdentifier), tsString[K]("="), tsSymbol[K](luaExpression)), tsSymbol[K](luaExpression))
                                                              rules[luaComment] = tsChoice[K](tsSeq[K](tsString[K]("--"), tsRegex[K]("[^\\r\\n]*")), tsSeq[K](tsSymbol[K](luaHidBlockCommentStart), tsSymbol[K](luaHidBlockCommentContent), tsSymbol[K](luaHidBlockCommentEnd)))
                                                              rules[luaAssignmentStatement] = tsSeq[K](tsSymbol[K](luaHidVariableAssignmentVarlist), tsString[K]("="), tsSymbol[K](luaHidVariableAssignmentExplist))
                                                              rules[luaNumber] = tsChoice[K](tsChoice[K](tsSeq[K](tsRegex[K]("[0-9]+"), tsRegex[K]("U?LL")), tsSeq[K](tsChoice[K](tsSeq[K](tsChoice[K](tsRegex[K]("[0-9]+"), tsBlank[K]()), tsChoice[K](tsString[K]("."), tsBlank[K]()), tsRegex[K]("[0-9]+")), tsSeq[K](tsRegex[K]("[0-9]+"), tsChoice[K](tsString[K]("."), tsBlank[K]()), tsChoice[K](tsRegex[K]("[0-9]+"), tsBlank[K]()))), tsChoice[K](tsSeq[K](tsChoice[K](tsString[K]("e"), tsString[K]("E")), tsSeq[K](tsChoice[K](tsChoice[K](tsString[K]("-"), tsString[K]("+")), tsBlank[K]()), tsRegex[K]("[0-9]+"))), tsBlank[K]()), tsChoice[K](tsChoice[K](tsString[K]("i"), tsString[K]("I")), tsBlank[K]()))), tsSeq[K](tsChoice[K](tsString[K]("0x"), tsString[K]("0X")), tsChoice[K](tsSeq[K](tsRegex[K]("[a-fA-F0-9]+"), tsRegex[K]("U?LL")), tsSeq[K](tsChoice[K](tsSeq[K](tsChoice[K](tsRegex[K]("[a-fA-F0-9]+"), tsBlank[K]()), tsChoice[K](tsString[K]("."), tsBlank[K]()), tsRegex[K]("[a-fA-F0-9]+")), tsSeq[K](tsRegex[K]("[a-fA-F0-9]+"), tsChoice[K](tsString[K]("."), tsBlank[K]()), tsChoice[K](tsRegex[K]("[a-fA-F0-9]+"), tsBlank[K]()))), tsChoice[K](tsSeq[K](tsChoice[K](tsString[K]("p"), tsString[K]("P")), tsSeq[K](tsChoice[K](tsChoice[K](tsString[K]("-"), tsString[K]("+")), tsBlank[K]()), tsRegex[K]("[0-9]+"))), tsBlank[K]()), tsChoice[K](tsChoice[K](tsString[K]("i"), tsString[K]("I")), tsBlank[K]())))))
                                                              rules[luaParameters] = tsSeq[K](tsString[K]("("), tsChoice[K](tsSymbol[K](luaHidParameterList), tsBlank[K]()), tsString[K](")"))
                                                              rules[luaIdentifier] = tsSeq[K](tsRegex[K]("[^\\p{Control}\\s+\\-*/%^#&~|<>=(){}\\[\\];:,.\\\\\'\"\\d]"), tsRegex[K]("[^\\p{Control}\\s+\\-*/%^#&~|<>=(){}\\[\\];:,.\\\\\'\"]*"))
                                                              rules[luaArguments] = tsChoice[K](tsSeq[K](tsString[K]("("), tsChoice[K](tsSeq[K](tsSymbol[K](luaExpression), tsRepeat[K](tsSeq[K](tsString[K](","), tsSymbol[K](luaExpression)))), tsBlank[K]()), tsString[K](")")), tsSymbol[K](luaTableConstructor), tsSymbol[K](luaString))
                                                              rules[luaHidSinglequoteStringContent] = tsRepeat1[K](tsChoice[K](tsRegex[K]("[^\'\\\\]+"), tsSymbol[K](luaEscapeSequence)))
                                                              rules[luaFunctionCall] = tsSeq[K](tsChoice[K](tsSymbol[K](luaHidPrefixExpression), tsSymbol[K](luaMethodIndexExpression)), tsSymbol[K](luaArguments))
                                                              rules[luaHashBangLine] = tsRegex[K]("#.*")
                                                              rules[luaVarargExpression] = tsString[K]("...")
                                                              rules[luaHidLocalVariableAssignment] = tsSeq[K](tsSymbol[K](luaHidAttNameList), tsString[K]("="), tsSymbol[K](luaHidVariableAssignmentExplist))
                                                              rules[luaEmptyStatement] = tsString[K](";")
                                                              rules[luaHidFunctionBody] = tsSeq[K](tsSymbol[K](luaParameters), tsChoice[K](tsSymbol[K](luaHidBlock), tsBlank[K]()), tsString[K]("end"))
                                                              rules[luaHidVariableAssignmentExplist] = tsSeq[K](tsSymbol[K](luaExpression), tsRepeat[K](tsSeq[K](tsString[K](","), tsSymbol[K](luaExpression))))
                                                              rules[luaStatement] = tsChoice[K](tsSymbol[K](luaEmptyStatement), tsSymbol[K](luaAssignmentStatement), tsSymbol[K](luaFunctionCall), tsSymbol[K](luaLabelStatement), tsSymbol[K](luaBreakStatement), tsSymbol[K](luaGotoStatement), tsSymbol[K](luaDoStatement), tsSymbol[K](luaWhileStatement), tsSymbol[K](luaRepeatStatement), tsSymbol[K](luaIfStatement), tsSymbol[K](luaForStatement), tsSymbol[K](luaDeclaration))
                                                              rules[luaDoStatement] = tsSeq[K](tsString[K]("do"), tsChoice[K](tsSymbol[K](luaHidBlock), tsBlank[K]()), tsString[K]("end"))
                                                              rules[luaForGenericClause] = tsSeq[K](tsSymbol[K](luaHidNameList), tsString[K]("in"), tsSymbol[K](luaHidExpressionList))
                                                              rules[luaForNumericClause] = tsSeq[K](tsSymbol[K](luaIdentifier), tsString[K]("="), tsSymbol[K](luaExpression), tsString[K](","), tsSymbol[K](luaExpression), tsChoice[K](tsSeq[K](tsString[K](","), tsSymbol[K](luaExpression)), tsBlank[K]()))
                                                              rules[luaTrue] = tsString[K]("true")
                                                              rules[luaChunk] = tsSeq[K](tsChoice[K](tsSymbol[K](luaHashBangLine), tsBlank[K]()), tsRepeat[K](tsSymbol[K](luaStatement)), tsChoice[K](tsSymbol[K](luaReturnStatement), tsBlank[K]()))
                                                              rules[luaVariable] = tsChoice[K](tsSymbol[K](luaIdentifier), tsSymbol[K](luaBracketIndexExpression), tsSymbol[K](luaDotIndexExpression))
                                                              rules[luaVariableDeclaration] = tsSeq[K](tsString[K]("local"), tsChoice[K](tsSymbol[K](luaHidAttNameList), tsSymbol[K](luaHidLocalVariableAssignment)))
                                                              rules[luaParenthesizedExpression] = tsSeq[K](tsString[K]("("), tsSymbol[K](luaExpression), tsString[K](")"))
                                                              rules

