import
  hmisc / wrappers/treesitter,
  hmisc / core/all,
  hmisc / types/colorstring,
  std/strutils
export treesitter
type
  JanetNodeKind* = enum
    janetHidExpr               ## _expr
    janetHidIdentifier         ## _identifier
    janetHidLiterals           ## _literals
    janetHidShorthand          ## _shorthand
    janetHidSpecialForms       ## _special_forms
    janetHidSpecials           ## _specials
    janetArray                 ## array
    janetBody                  ## body
    janetBoolLiteral           ## bool_literal
    janetBreak                 ## break
    janetDef                   ## def
    janetDo                    ## do
    janetExtraDefs             ## extra_defs
    janetFn                    ## fn
    janetIf                    ## if
    janetLongBufferLiteral     ## long_buffer_literal
    janetLongStrLiteral        ## long_str_literal
    janetMetadata              ## metadata
    janetNumberLiteral         ## number_literal
    janetParameters            ## parameters
    janetPegSet                ## peg_set
    janetQuasiquote            ## quasiquote
    janetQuote                 ## quote
    janetSet                   ## set
    janetShortFn               ## short_fn
    janetShortQuasiquote       ## short_quasiquote
    janetShortQuote            ## short_quote
    janetShortSplice           ## short_splice
    janetShortUnquote          ## short_unquote
    janetSourceFile            ## source_file
    janetSplice                ## splice
    janetSqrArray              ## sqr_array
    janetSqrTuple              ## sqr_tuple
    janetStruct                ## struct
    janetTable                 ## table
    janetTuple                 ## tuple
    janetTupleParameters       ## tuple_parameters
    janetUnquote               ## unquote
    janetUpscope               ## upscope
    janetVar                   ## var
    janetWhile                 ## while
    janetApostropheTok         ## '
    janetLParTok               ## (
    janetRParTok               ## )
    janetCommaTok              ## ,
    janetSemicolonTok          ## ;
    janetAtLParTok             ## @(
    janetAtLBrackTok           ## @[
    janetAtLCurlyTok           ## @{
    janetLBrackTok             ## [
    janetRBrackTok             ## ]
    janetBreakTok              ## break
    janetBufferLiteral         ## buffer_literal
    janetDefTok                ## def
    janetDefmacroTok           ## defmacro
    janetDefmacroMinusTok      ## defmacro-
    janetDefnTok               ## defn
    janetDefnMinusTok          ## defn-
    janetDoTok                 ## do
    janetFalseTok              ## false
    janetFnTok                 ## fn
    janetIfTok                 ## if
    janetKeysMarker            ## keys_marker
    janetKeyword               ## keyword
    janetLineComment           ## line_comment
    janetNilLiteral            ## nil_literal
    janetOptionalMarker        ## optional_marker
    janetQuasiquoteTok         ## quasiquote
    janetQuoteTok              ## quote
    janetSetTok                ## set
    janetSpliceTok             ## splice
    janetStrLiteral            ## str_literal
    janetSymbol                ## symbol
    janetTrueTok               ## true
    janetUnquoteTok            ## unquote
    janetUpscopeTok            ## upscope
    janetVarTok                ## var
    janetVarfnTok              ## varfn
    janetVarfnMinusTok         ## varfn-
    janetVariadicMarker        ## variadic_marker
    janetWhileTok              ## while
    janetLCurlyTok             ## {
    janetPipeTok               ## |
    janetRCurlyTok             ## }
    janetTildeTok              ## ~
    janetHidStructTablesCommom ## _struct_tables_commom
    janetHidParameters         ## _parameters
    janetDocStr                ## doc_str
    janetHidLongStr            ## _long_str
    janetEscapeSequence        ## escape_sequence
    janetHidLongBuffer         ## _long_buffer
    janetHidName               ## _name
    janetSyntaxError           ## Tree-sitter parser syntax error

proc strRepr*(kind: JanetNodeKind): string =
  case kind:
    of janetHidExpr:               "_expr"
    of janetHidIdentifier:         "_identifier"
    of janetHidLiterals:           "_literals"
    of janetHidShorthand:          "_shorthand"
    of janetHidSpecialForms:       "_special_forms"
    of janetHidSpecials:           "_specials"
    of janetArray:                 "array"
    of janetBody:                  "body"
    of janetBoolLiteral:           "bool_literal"
    of janetBreak:                 "break"
    of janetDef:                   "def"
    of janetDo:                    "do"
    of janetExtraDefs:             "extra_defs"
    of janetFn:                    "fn"
    of janetIf:                    "if"
    of janetLongBufferLiteral:     "long_buffer_literal"
    of janetLongStrLiteral:        "long_str_literal"
    of janetMetadata:              "metadata"
    of janetNumberLiteral:         "number_literal"
    of janetParameters:            "parameters"
    of janetPegSet:                "peg_set"
    of janetQuasiquote:            "quasiquote"
    of janetQuote:                 "quote"
    of janetSet:                   "set"
    of janetShortFn:               "short_fn"
    of janetShortQuasiquote:       "short_quasiquote"
    of janetShortQuote:            "short_quote"
    of janetShortSplice:           "short_splice"
    of janetShortUnquote:          "short_unquote"
    of janetSourceFile:            "source_file"
    of janetSplice:                "splice"
    of janetSqrArray:              "sqr_array"
    of janetSqrTuple:              "sqr_tuple"
    of janetStruct:                "struct"
    of janetTable:                 "table"
    of janetTuple:                 "tuple"
    of janetTupleParameters:       "tuple_parameters"
    of janetUnquote:               "unquote"
    of janetUpscope:               "upscope"
    of janetVar:                   "var"
    of janetWhile:                 "while"
    of janetApostropheTok:         "\'"
    of janetLParTok:               "("
    of janetRParTok:               ")"
    of janetCommaTok:              ","
    of janetSemicolonTok:          ";"
    of janetAtLParTok:             "@("
    of janetAtLBrackTok:           "@["
    of janetAtLCurlyTok:           "@{"
    of janetLBrackTok:             "["
    of janetRBrackTok:             "]"
    of janetBreakTok:              "break"
    of janetBufferLiteral:         "buffer_literal"
    of janetDefTok:                "def"
    of janetDefmacroTok:           "defmacro"
    of janetDefmacroMinusTok:      "defmacro-"
    of janetDefnTok:               "defn"
    of janetDefnMinusTok:          "defn-"
    of janetDoTok:                 "do"
    of janetFalseTok:              "false"
    of janetFnTok:                 "fn"
    of janetIfTok:                 "if"
    of janetKeysMarker:            "keys_marker"
    of janetKeyword:               "keyword"
    of janetLineComment:           "line_comment"
    of janetNilLiteral:            "nil_literal"
    of janetOptionalMarker:        "optional_marker"
    of janetQuasiquoteTok:         "quasiquote"
    of janetQuoteTok:              "quote"
    of janetSetTok:                "set"
    of janetSpliceTok:             "splice"
    of janetStrLiteral:            "str_literal"
    of janetSymbol:                "symbol"
    of janetTrueTok:               "true"
    of janetUnquoteTok:            "unquote"
    of janetUpscopeTok:            "upscope"
    of janetVarTok:                "var"
    of janetVarfnTok:              "varfn"
    of janetVarfnMinusTok:         "varfn-"
    of janetVariadicMarker:        "variadic_marker"
    of janetWhileTok:              "while"
    of janetLCurlyTok:             "{"
    of janetPipeTok:               "|"
    of janetRCurlyTok:             "}"
    of janetTildeTok:              "~"
    of janetHidStructTablesCommom: "_struct_tables_commom"
    of janetHidParameters:         "_parameters"
    of janetDocStr:                "doc_str"
    of janetHidLongStr:            "_long_str"
    of janetEscapeSequence:        "escape_sequence"
    of janetHidLongBuffer:         "_long_buffer"
    of janetHidName:               "_name"
    of janetSyntaxError:           "ERROR"

type
  JanetExternalTok* = enum
    janetExtern_long_str    ## _long_str
    janetExtern_long_buffer ## _long_buffer

type
  TsJanetNode* = distinct TSNode

type
  JanetParser* = distinct PtsParser

const janetAllowedSubnodes*: array[JanetNodeKind, set[JanetNodeKind]] = block:
                                                                          var tmp: array[JanetNodeKind, set[JanetNodeKind]]
                                                                          tmp[janetBreak] = {janetHidExpr}
                                                                          tmp[janetMetadata] = {
                                                                                                 janetKeyword,
                                                                                                 janetLongStrLiteral,
                                                                                                 janetQuasiquote,
                                                                                                 janetQuote,
                                                                                                 janetShortQuasiquote,
                                                                                                 janetShortQuote,
                                                                                                 janetShortSplice,
                                                                                                 janetShortUnquote,
                                                                                                 janetSplice,
                                                                                                 janetStrLiteral,
                                                                                                 janetStruct,
                                                                                                 janetUnquote
                                                                                               }
                                                                          tmp[janetParameters] = {janetKeysMarker, janetOptionalMarker, janetVariadicMarker}
                                                                          tmp[janetPegSet] = {janetLongStrLiteral, janetQuote, janetShortQuote, janetStrLiteral}
                                                                          tmp[janetQuasiquote] = {janetHidExpr}
                                                                          tmp[janetQuote] = {janetHidExpr}
                                                                          tmp[janetShortQuasiquote] = {janetHidExpr}
                                                                          tmp[janetShortQuote] = {janetHidExpr}
                                                                          tmp[janetShortSplice] = {janetHidExpr}
                                                                          tmp[janetShortUnquote] = {janetHidExpr}
                                                                          tmp[janetSourceFile] = {janetHidExpr}
                                                                          tmp[janetSplice] = {janetHidExpr}
                                                                          tmp[janetTupleParameters] = {janetKeysMarker, janetOptionalMarker, janetVariadicMarker}
                                                                          tmp[janetUnquote] = {janetHidExpr}
                                                                          tmp
const janetTokenKinds*: set[JanetNodeKind] = {
                                               janetApostropheTok,
                                               janetLParTok,
                                               janetRParTok,
                                               janetCommaTok,
                                               janetSemicolonTok,
                                               janetAtLParTok,
                                               janetAtLBrackTok,
                                               janetAtLCurlyTok,
                                               janetLBrackTok,
                                               janetRBrackTok,
                                               janetBreakTok,
                                               janetDefTok,
                                               janetDefmacroTok,
                                               janetDefmacroMinusTok,
                                               janetDefnTok,
                                               janetDefnMinusTok,
                                               janetDoTok,
                                               janetFalseTok,
                                               janetFnTok,
                                               janetIfTok,
                                               janetQuasiquoteTok,
                                               janetQuoteTok,
                                               janetSetTok,
                                               janetSpliceTok,
                                               janetTrueTok,
                                               janetUnquoteTok,
                                               janetUpscopeTok,
                                               janetVarTok,
                                               janetVarfnTok,
                                               janetVarfnMinusTok,
                                               janetWhileTok,
                                               janetLCurlyTok,
                                               janetPipeTok,
                                               janetRCurlyTok,
                                               janetTildeTok
                                             }
const janetHiddenKinds*: set[JanetNodeKind] = {
                                                janetHidStructTablesCommom,
                                                janetHidLiterals,
                                                janetHidShorthand,
                                                janetHidParameters,
                                                janetHidExpr,
                                                janetHidSpecialForms,
                                                janetHidSpecials,
                                                janetDocStr,
                                                janetHidLongStr,
                                                janetEscapeSequence,
                                                janetHidIdentifier,
                                                janetHidLongBuffer,
                                                janetHidName
                                              }
proc tsNodeType*(node: TsJanetNode): string


proc kind*(node: TsJanetNode): JanetNodeKind {.noSideEffect.} =
  {.cast(noSideEffect).}:
    case node.tsNodeType:
      of "_expr":               janetHidExpr
      of "_identifier":         janetHidIdentifier
      of "_literals":           janetHidLiterals
      of "_shorthand":          janetHidShorthand
      of "_special_forms":      janetHidSpecialForms
      of "_specials":           janetHidSpecials
      of "array":               janetArray
      of "body":                janetBody
      of "bool_literal":        janetBoolLiteral
      of "break":               janetBreak
      of "def":                 janetDef
      of "do":                  janetDo
      of "extra_defs":          janetExtraDefs
      of "fn":                  janetFn
      of "if":                  janetIf
      of "long_buffer_literal": janetLongBufferLiteral
      of "long_str_literal":    janetLongStrLiteral
      of "metadata":            janetMetadata
      of "number_literal":      janetNumberLiteral
      of "parameters":          janetParameters
      of "peg_set":             janetPegSet
      of "quasiquote":          janetQuasiquote
      of "quote":               janetQuote
      of "set":                 janetSet
      of "short_fn":            janetShortFn
      of "short_quasiquote":    janetShortQuasiquote
      of "short_quote":         janetShortQuote
      of "short_splice":        janetShortSplice
      of "short_unquote":       janetShortUnquote
      of "source_file":         janetSourceFile
      of "splice":              janetSplice
      of "sqr_array":           janetSqrArray
      of "sqr_tuple":           janetSqrTuple
      of "struct":              janetStruct
      of "table":               janetTable
      of "tuple":               janetTuple
      of "tuple_parameters":    janetTupleParameters
      of "unquote":             janetUnquote
      of "upscope":             janetUpscope
      of "var":                 janetVar
      of "while":               janetWhile
      of "\'":                  janetApostropheTok
      of "(":                   janetLParTok
      of ")":                   janetRParTok
      of ",":                   janetCommaTok
      of ";":                   janetSemicolonTok
      of "@(":                  janetAtLParTok
      of "@[":                  janetAtLBrackTok
      of "@{":                  janetAtLCurlyTok
      of "[":                   janetLBrackTok
      of "]":                   janetRBrackTok
      of "buffer_literal":      janetBufferLiteral
      of "defmacro":            janetDefmacroTok
      of "defmacro-":           janetDefmacroMinusTok
      of "defn":                janetDefnTok
      of "defn-":               janetDefnMinusTok
      of "false":               janetFalseTok
      of "keys_marker":         janetKeysMarker
      of "keyword":             janetKeyword
      of "line_comment":        janetLineComment
      of "nil_literal":         janetNilLiteral
      of "optional_marker":     janetOptionalMarker
      of "str_literal":         janetStrLiteral
      of "symbol":              janetSymbol
      of "true":                janetTrueTok
      of "varfn":               janetVarfnTok
      of "varfn-":              janetVarfnMinusTok
      of "variadic_marker":     janetVariadicMarker
      of "{":                   janetLCurlyTok
      of "|":                   janetPipeTok
      of "}":                   janetRCurlyTok
      of "~":                   janetTildeTok
      of "ERROR":               janetSyntaxError
      else:
        raiseAssert("Invalid element name \'" & node.tsNodeType & "\'")

func isNil*(node: TsJanetNode): bool =
  ts_node_is_null(TSNode(node))

func len*(node: TsJanetNode, unnamed: bool = false): int =
  if unnamed:
    int(ts_node_child_count(TSNode(node)))

  else:
    int(ts_node_named_child_count(TSNode(node)))


func has*(node: TsJanetNode, idx: int, unnamed: bool = false): bool =
  0 <= idx and idx < node.len(unnamed)

proc tree_sitter_janet(): PtsLanguage {.importc, cdecl.}


proc tsNodeType*(node: TsJanetNode): string =
  $ts_node_type(TSNode(node))

proc newTsJanetParser*(): JanetParser =
  result = JanetParser(ts_parser_new())
  discard ts_parser_set_language(PtsParser(result), tree_sitter_janet())

proc parseString*(parser: JanetParser, str: string): TsJanetNode =
  TsJanetNode(ts_tree_root_node(ts_parser_parse_string(PtsParser(parser), nil, str.cstring, uint32(len(str)))))

proc parseTsJanetString*(str: string): TsJanetNode =
  let parser = newTsJanetParser()
  return parseString(parser, str)

func `$`*(node: TsJanetNode): string =
  if isNil(node):
    "<nil tree>"

  else:
    $node.kind


func `[]`*(
    node: TsJanetNode,
    idx:  int,
    kind: JanetNodeKind | set[JanetNodeKind]
  ): TsJanetNode =
  assert 0 <= idx and idx < node.len
  result = TsJanetNode(ts_node_named_child(TSNode(node), uint32(idx)))
  assertKind(result, kind, "Child node at index " & $idx & " for node kind " & $node.kind)

type
  JanetNode* = HtsNode[TsJanetNode, JanetNodeKind]


proc treeReprTsJanet*(str: string, unnamed: bool = false): ColoredText =
  treeRepr[TsJanetNode, JanetNodeKind](parseTsJanetString(str), str, 5, unnamed = unnamed)

proc toHtsNode*(
    node: TsJanetNode,
    str:  ptr string
  ): HtsNode[TsJanetNode, JanetNodeKind] =
  toHtsNode[TsJanetNode, JanetNodeKind](node, str)

proc toHtsTree*(node: TsJanetNode, str: ptr string): JanetNode =
  toHtsNode[TsJanetNode, JanetNodeKind](node, str)

proc parseJanetString*(str: ptr string, unnamed: bool = false): JanetNode =
  let parser = newTsJanetParser()
  return toHtsTree[TsJanetNode, JanetNodeKind](parseString(parser, str[]), str)

proc parseJanetString*(str: string, unnamed: bool = false): JanetNode =
  let parser = newTsJanetParser()
  return toHtsTree[TsJanetNode, JanetNodeKind](parseString(parser, str), unsafeAddr str, storePtr = false)


import
  hmisc / wrappers/treesitter_core
let janetGrammar*: array[JanetNodeKind, HtsRule[JanetNodeKind]] = block:
                                                                    var rules: array[JanetNodeKind, HtsRule[JanetNodeKind]]
                                                                    type
                                                                      K = JanetNodeKind


                                                                    rules[janetShortQuote] = tsSeq[K](tsString[K]("\'"), tsSymbol[K](janetHidExpr))
                                                                    rules[janetVariadicMarker] = tsString[K]("&")
                                                                    rules[janetHidStructTablesCommom] = tsSeq[K](tsSymbol[K](janetHidExpr), tsSymbol[K](janetHidExpr))
                                                                    rules[janetQuasiquote] = tsSeq[K](tsString[K]("("), tsString[K]("quasiquote"), tsChoice[K](tsSymbol[K](janetHidExpr), tsBlank[K]()), tsString[K](")"))
                                                                    rules[janetShortSplice] = tsSeq[K](tsString[K](";"), tsSymbol[K](janetHidExpr))
                                                                    rules[janetFn] = tsSeq[K](tsString[K]("("), tsString[K]("fn"), tsChoice[K](tsSymbol[K](janetHidName), tsBlank[K]()), tsSymbol[K](janetHidParameters), tsChoice[K](tsSymbol[K](janetBody), tsBlank[K]()), tsString[K](")"))
                                                                    rules[janetShortQuasiquote] = tsSeq[K](tsString[K]("~"), tsSymbol[K](janetHidExpr))
                                                                    rules[janetHidLiterals] = tsChoice[K](tsSymbol[K](janetBoolLiteral), tsSymbol[K](janetNilLiteral), tsSymbol[K](janetNumberLiteral), tsSymbol[K](janetStrLiteral), tsSymbol[K](janetLongStrLiteral), tsSymbol[K](janetBufferLiteral), tsSymbol[K](janetLongBufferLiteral))
                                                                    rules[janetNilLiteral] = tsString[K]("nil")
                                                                    rules[janetHidShorthand] = tsChoice[K](tsSymbol[K](janetShortQuote), tsSymbol[K](janetShortSplice), tsSymbol[K](janetShortQuasiquote), tsSymbol[K](janetShortUnquote), tsSymbol[K](janetShortFn))
                                                                    rules[janetSymbol] = tsRegex[K]("[^({\\[\"\'|`;,~\\]}\\s):][^({\\[\"\'|`;,~\\]}\\s)]*")
                                                                    rules[janetHidParameters] = tsChoice[K](tsSymbol[K](janetHidIdentifier), tsSymbol[K](janetQuote), tsSymbol[K](janetSplice), tsSymbol[K](janetQuasiquote), tsSymbol[K](janetUnquote), tsSymbol[K](janetShortQuote), tsSymbol[K](janetShortSplice), tsSymbol[K](janetShortQuasiquote), tsSymbol[K](janetShortUnquote), tsSymbol[K](janetParameters), tsSymbol[K](janetTupleParameters))
                                                                    rules[janetVar] = tsSeq[K](tsString[K]("("), tsString[K]("var"), tsSymbol[K](janetHidExpr), tsRepeat[K](tsSymbol[K](janetMetadata)), tsSymbol[K](janetHidExpr), tsString[K](")"))
                                                                    rules[janetHidExpr] = tsChoice[K](tsSymbol[K](janetHidLiterals), tsSymbol[K](janetHidIdentifier), tsSymbol[K](janetHidSpecialForms), tsSymbol[K](janetHidShorthand), tsSymbol[K](janetHidSpecials), tsSymbol[K](janetArray), tsSymbol[K](janetSqrArray), tsSymbol[K](janetTuple), tsSymbol[K](janetSqrTuple), tsSymbol[K](janetStruct), tsSymbol[K](janetTable), tsSymbol[K](janetPegSet))
                                                                    rules[janetHidSpecialForms] = tsChoice[K](tsSymbol[K](janetDef), tsSymbol[K](janetVar), tsSymbol[K](janetQuote), tsSymbol[K](janetSplice), tsSymbol[K](janetQuasiquote), tsSymbol[K](janetUnquote), tsSymbol[K](janetBreak), tsSymbol[K](janetSet), tsSymbol[K](janetIf), tsSymbol[K](janetDo), tsSymbol[K](janetWhile), tsSymbol[K](janetFn), tsSymbol[K](janetUpscope))
                                                                    rules[janetLongBufferLiteral] = tsSymbol[K](janetHidLongBuffer)
                                                                    rules[janetHidSpecials] = tsChoice[K](tsSymbol[K](janetExtraDefs))
                                                                    rules[janetIf] = tsSeq[K](tsString[K]("("), tsString[K]("if"), tsSymbol[K](janetHidExpr), tsSymbol[K](janetHidExpr), tsChoice[K](tsSymbol[K](janetHidExpr), tsBlank[K]()), tsString[K](")"))
                                                                    rules[janetDo] = tsSeq[K](tsString[K]("("), tsString[K]("do"), tsChoice[K](tsSymbol[K](janetBody), tsBlank[K]()), tsString[K](")"))
                                                                    rules[janetDocStr] = tsChoice[K](tsSymbol[K](janetStrLiteral), tsSymbol[K](janetLongStrLiteral))
                                                                    rules[janetTuple] = tsSeq[K](tsString[K]("("), tsRepeat[K](tsSymbol[K](janetHidExpr)), tsString[K](")"))
                                                                    rules[janetUnquote] = tsSeq[K](tsString[K]("("), tsString[K]("unquote"), tsChoice[K](tsSymbol[K](janetHidExpr), tsBlank[K]()), tsString[K](")"))
                                                                    rules[janetExtraDefs] = tsSeq[K](tsString[K]("("), tsChoice[K](tsString[K]("defn"), tsString[K]("defn-"), tsString[K]("varfn"), tsString[K]("varfn-"), tsString[K]("defmacro"), tsString[K]("defmacro-")), tsSymbol[K](janetHidName), tsRepeat[K](tsSymbol[K](janetMetadata)), tsSymbol[K](janetHidParameters), tsChoice[K](tsSymbol[K](janetBody), tsBlank[K]()), tsString[K](")"))
                                                                    rules[janetBody] = tsRepeat1[K](tsSymbol[K](janetHidExpr))
                                                                    rules[janetKeyword] = tsRegex[K](":[^({\\[\"\'|`;,~\\]}\\s)]*")
                                                                    rules[janetStrLiteral] = tsSeq[K](tsString[K]("\""), tsRepeat[K](tsChoice[K](tsRegex[K]("[^\"\\\\]+"), tsRegex[K]("\\\\[^xu]"), tsRegex[K]("\\\\u[0-9a-fA-F]{4}"), tsRegex[K]("\\\\u{[0-9a-fA-F]+}"), tsRegex[K]("\\\\x[0-9a-fA-F]{2}"))), tsString[K]("\""))
                                                                    rules[janetNumberLiteral] = tsChoice[K](tsRegex[K]("[-+]?(\\d[_\\d]*|\\d[_\\d]*\\.[_\\d]*|\\.[_\\d]+)([eE&][+-]?[\\d]+)?"), tsRegex[K]("[-+]?0x([_\\da-fA-F]+|[_\\da-fA-F]+\\.[_\\da-fA-F]*|\\.[_\\da-fA-F]+)(&[+-]?[\\da-fA-F]+)?"), tsRegex[K]("[-+]?\\d\\d?r([_\\w]+|[_\\w]+\\.[_\\w]*|\\.[_\\w]+)(&[+-]?[\\w]+)?"))
                                                                    rules[janetSqrArray] = tsSeq[K](tsString[K]("@["), tsRepeat[K](tsSymbol[K](janetHidExpr)), tsString[K]("]"))
                                                                    rules[janetLongStrLiteral] = tsSymbol[K](janetHidLongStr)
                                                                    rules[janetEscapeSequence] = tsChoice[K](tsRegex[K]("\\\\[^xu]"), tsRegex[K]("\\\\u[0-9a-fA-F]{4}"), tsRegex[K]("\\\\u{[0-9a-fA-F]+}"), tsRegex[K]("\\\\x[0-9a-fA-F]{2}"))
                                                                    rules[janetBoolLiteral] = tsChoice[K](tsString[K]("true"), tsString[K]("false"))
                                                                    rules[janetLineComment] = tsSeq[K](tsString[K]("#"), tsRegex[K](".*"))
                                                                    rules[janetWhile] = tsSeq[K](tsString[K]("("), tsString[K]("while"), tsSymbol[K](janetHidExpr), tsRepeat[K](tsSymbol[K](janetHidExpr)), tsString[K](")"))
                                                                    rules[janetSourceFile] = tsRepeat[K](tsSymbol[K](janetHidExpr))
                                                                    rules[janetHidIdentifier] = tsChoice[K](tsSymbol[K](janetSymbol), tsSymbol[K](janetKeyword))
                                                                    rules[janetSqrTuple] = tsSeq[K](tsString[K]("["), tsRepeat[K](tsSymbol[K](janetHidExpr)), tsString[K]("]"))
                                                                    rules[janetSplice] = tsSeq[K](tsString[K]("("), tsString[K]("splice"), tsChoice[K](tsSymbol[K](janetHidExpr), tsBlank[K]()), tsString[K](")"))
                                                                    rules[janetParameters] = tsSeq[K](tsString[K]("["), tsRepeat[K](tsSymbol[K](janetHidExpr)), tsChoice[K](tsSeq[K](tsChoice[K](tsSymbol[K](janetVariadicMarker), tsSymbol[K](janetOptionalMarker), tsSymbol[K](janetKeysMarker)), tsRepeat[K](tsSymbol[K](janetHidExpr))), tsBlank[K]()), tsString[K]("]"))
                                                                    rules[janetHidName] = tsChoice[K](tsSymbol[K](janetHidIdentifier), tsSymbol[K](janetQuote), tsSymbol[K](janetSplice), tsSymbol[K](janetQuasiquote), tsSymbol[K](janetUnquote), tsSymbol[K](janetShortQuote), tsSymbol[K](janetShortSplice), tsSymbol[K](janetShortQuasiquote), tsSymbol[K](janetShortUnquote))
                                                                    rules[janetBufferLiteral] = tsSeq[K](tsString[K]("@\""), tsRepeat[K](tsChoice[K](tsRegex[K]("[^\"\\\\]+"), tsRegex[K]("\\\\[^xu]"), tsRegex[K]("\\\\u[0-9a-fA-F]{4}"), tsRegex[K]("\\\\u{[0-9a-fA-F]+}"), tsRegex[K]("\\\\x[0-9a-fA-F]{2}"))), tsString[K]("\""))
                                                                    rules[janetTable] = tsSeq[K](tsString[K]("@{"), tsRepeat[K](tsSymbol[K](janetHidStructTablesCommom)), tsString[K]("}"))
                                                                    rules[janetDef] = tsSeq[K](tsString[K]("("), tsString[K]("def"), tsSymbol[K](janetHidExpr), tsRepeat[K](tsSymbol[K](janetMetadata)), tsSymbol[K](janetHidExpr), tsString[K](")"))
                                                                    rules[janetQuote] = tsSeq[K](tsString[K]("("), tsString[K]("quote"), tsChoice[K](tsSymbol[K](janetHidExpr), tsBlank[K]()), tsString[K](")"))
                                                                    rules[janetOptionalMarker] = tsString[K]("&opt")
                                                                    rules[janetBreak] = tsSeq[K](tsString[K]("("), tsString[K]("break"), tsChoice[K](tsSymbol[K](janetHidExpr), tsBlank[K]()), tsString[K](")"))
                                                                    rules[janetSet] = tsSeq[K](tsString[K]("("), tsString[K]("set"), tsSymbol[K](janetHidExpr), tsSymbol[K](janetHidExpr), tsString[K](")"))
                                                                    rules[janetUpscope] = tsSeq[K](tsString[K]("("), tsString[K]("upscope"), tsChoice[K](tsSymbol[K](janetBody), tsBlank[K]()), tsString[K](")"))
                                                                    rules[janetShortUnquote] = tsSeq[K](tsString[K](","), tsSymbol[K](janetHidExpr))
                                                                    rules[janetTupleParameters] = tsSeq[K](tsString[K]("("), tsRepeat[K](tsSymbol[K](janetHidExpr)), tsChoice[K](tsSeq[K](tsChoice[K](tsSymbol[K](janetVariadicMarker), tsSymbol[K](janetOptionalMarker), tsSymbol[K](janetKeysMarker)), tsRepeat[K](tsSymbol[K](janetHidExpr))), tsBlank[K]()), tsString[K](")"))
                                                                    rules[janetShortFn] = tsSeq[K](tsString[K]("|"), tsSymbol[K](janetHidExpr))
                                                                    rules[janetArray] = tsSeq[K](tsString[K]("@("), tsRepeat[K](tsSymbol[K](janetHidExpr)), tsString[K](")"))
                                                                    rules[janetKeysMarker] = tsString[K]("&keys")
                                                                    rules[janetStruct] = tsSeq[K](tsString[K]("{"), tsRepeat[K](tsSymbol[K](janetHidStructTablesCommom)), tsString[K]("}"))
                                                                    rules[janetPegSet] = tsSeq[K](tsString[K]("("), tsString[K]("set"), tsChoice[K](tsSymbol[K](janetStrLiteral), tsSymbol[K](janetLongStrLiteral), tsSymbol[K](janetQuote), tsSymbol[K](janetShortQuote)), tsString[K](")"))
                                                                    rules[janetMetadata] = tsChoice[K](tsSymbol[K](janetKeyword), tsSymbol[K](janetStrLiteral), tsSymbol[K](janetLongStrLiteral), tsSymbol[K](janetStruct), tsSymbol[K](janetQuote), tsSymbol[K](janetSplice), tsSymbol[K](janetQuasiquote), tsSymbol[K](janetUnquote), tsSymbol[K](janetShortQuote), tsSymbol[K](janetShortSplice), tsSymbol[K](janetShortQuasiquote), tsSymbol[K](janetShortUnquote))
                                                                    rules

