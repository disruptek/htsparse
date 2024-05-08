import
  hmisc / wrappers/treesitter,
  hmisc / core/all,
  hmisc / types/colorstring,
  std/strutils
export treesitter
type
  UngrammarNodeKind* = enum
    ungrammarAlternation     ## alternation
    ungrammarGrammar         ## grammar
    ungrammarLabel           ## label
    ungrammarNode            ## node
    ungrammarNodeRule        ## node_rule
    ungrammarOptional        ## optional
    ungrammarRepetition      ## repetition
    ungrammarRule            ## rule
    ungrammarSequence        ## sequence
    ungrammarToken           ## token
    ungrammarApostropheTok   ## '
    ungrammarLParTok         ## (
    ungrammarRParTok         ## )
    ungrammarAsteriskTok     ## *
    ungrammarColonTok        ## :
    ungrammarEqualTok        ## =
    ungrammarQuestionTok     ## ?
    ungrammarComment         ## comment
    ungrammarDefinition      ## definition
    ungrammarIdentifier      ## identifier
    ungrammarLabelName       ## label_name
    ungrammarTokenIdentifier ## token_identifier
    ungrammarPipeTok         ## |
    ungrammarHidAtomRule     ## _atom_rule
    ungrammarSyntaxError     ## Tree-sitter parser syntax error

proc strRepr*(kind: UngrammarNodeKind): string =
  case kind:
    of ungrammarAlternation:     "alternation"
    of ungrammarGrammar:         "grammar"
    of ungrammarLabel:           "label"
    of ungrammarNode:            "node"
    of ungrammarNodeRule:        "node_rule"
    of ungrammarOptional:        "optional"
    of ungrammarRepetition:      "repetition"
    of ungrammarRule:            "rule"
    of ungrammarSequence:        "sequence"
    of ungrammarToken:           "token"
    of ungrammarApostropheTok:   "\'"
    of ungrammarLParTok:         "("
    of ungrammarRParTok:         ")"
    of ungrammarAsteriskTok:     "*"
    of ungrammarColonTok:        ":"
    of ungrammarEqualTok:        "="
    of ungrammarQuestionTok:     "?"
    of ungrammarComment:         "comment"
    of ungrammarDefinition:      "definition"
    of ungrammarIdentifier:      "identifier"
    of ungrammarLabelName:       "label_name"
    of ungrammarTokenIdentifier: "token_identifier"
    of ungrammarPipeTok:         "|"
    of ungrammarHidAtomRule:     "_atom_rule"
    of ungrammarSyntaxError:     "ERROR"

type
  TsUngrammarNode* = distinct TSNode

type
  UngrammarParser* = distinct PtsParser

const ungrammarAllowedSubnodes*: array[UngrammarNodeKind, set[UngrammarNodeKind]] = block:
                                                                                      var tmp: array[UngrammarNodeKind, set[UngrammarNodeKind]]
                                                                                      tmp[ungrammarAlternation] = {ungrammarSequence}
                                                                                      tmp[ungrammarGrammar] = {ungrammarNode}
                                                                                      tmp[ungrammarLabel] = {
                                                                                                              ungrammarLabel,
                                                                                                              ungrammarLabelName,
                                                                                                              ungrammarNodeRule,
                                                                                                              ungrammarOptional,
                                                                                                              ungrammarRepetition,
                                                                                                              ungrammarRule,
                                                                                                              ungrammarToken
                                                                                                            }
                                                                                      tmp[ungrammarNode] = {ungrammarDefinition, ungrammarRule}
                                                                                      tmp[ungrammarNodeRule] = {ungrammarIdentifier}
                                                                                      tmp[ungrammarOptional] = {ungrammarLabel, ungrammarNodeRule, ungrammarOptional, ungrammarRepetition, ungrammarRule, ungrammarToken}
                                                                                      tmp[ungrammarRepetition] = {ungrammarLabel, ungrammarNodeRule, ungrammarOptional, ungrammarRepetition, ungrammarRule, ungrammarToken}
                                                                                      tmp[ungrammarRule] = {ungrammarAlternation}
                                                                                      tmp[ungrammarSequence] = {ungrammarLabel, ungrammarNodeRule, ungrammarOptional, ungrammarRepetition, ungrammarRule, ungrammarToken}
                                                                                      tmp[ungrammarToken] = {ungrammarTokenIdentifier}
                                                                                      tmp
const ungrammarTokenKinds*: set[UngrammarNodeKind] = {
                                                       ungrammarApostropheTok,
                                                       ungrammarLParTok,
                                                       ungrammarRParTok,
                                                       ungrammarAsteriskTok,
                                                       ungrammarColonTok,
                                                       ungrammarEqualTok,
                                                       ungrammarQuestionTok,
                                                       ungrammarPipeTok
                                                     }
const ungrammarHiddenKinds*: set[UngrammarNodeKind] = {ungrammarHidAtomRule}
proc tsNodeType*(node: TsUngrammarNode): string


proc kind*(node: TsUngrammarNode): UngrammarNodeKind {.noSideEffect.} =
  {.cast(noSideEffect).}:
    case node.tsNodeType:
      of "alternation":      ungrammarAlternation
      of "grammar":          ungrammarGrammar
      of "label":            ungrammarLabel
      of "node":             ungrammarNode
      of "node_rule":        ungrammarNodeRule
      of "optional":         ungrammarOptional
      of "repetition":       ungrammarRepetition
      of "rule":             ungrammarRule
      of "sequence":         ungrammarSequence
      of "token":            ungrammarToken
      of "\'":               ungrammarApostropheTok
      of "(":                ungrammarLParTok
      of ")":                ungrammarRParTok
      of "*":                ungrammarAsteriskTok
      of ":":                ungrammarColonTok
      of "=":                ungrammarEqualTok
      of "?":                ungrammarQuestionTok
      of "comment":          ungrammarComment
      of "definition":       ungrammarDefinition
      of "identifier":       ungrammarIdentifier
      of "label_name":       ungrammarLabelName
      of "token_identifier": ungrammarTokenIdentifier
      of "|":                ungrammarPipeTok
      of "ERROR":            ungrammarSyntaxError
      else:
        raiseAssert("Invalid element name \'" & node.tsNodeType & "\'")

func isNil*(node: TsUngrammarNode): bool =
  ts_node_is_null(TSNode(node))

func len*(node: TsUngrammarNode, unnamed: bool = false): int =
  if unnamed:
    int(ts_node_child_count(TSNode(node)))

  else:
    int(ts_node_named_child_count(TSNode(node)))


func has*(node: TsUngrammarNode, idx: int, unnamed: bool = false): bool =
  0 <= idx and idx < node.len(unnamed)

proc tree_sitter_ungrammar(): PtsLanguage {.importc, cdecl.}


proc tsNodeType*(node: TsUngrammarNode): string =
  $ts_node_type(TSNode(node))

proc newTsUngrammarParser*(): UngrammarParser =
  result = UngrammarParser(ts_parser_new())
  discard ts_parser_set_language(PtsParser(result), tree_sitter_ungrammar())

proc parseString*(parser: UngrammarParser, str: string): TsUngrammarNode =
  TsUngrammarNode(ts_tree_root_node(ts_parser_parse_string(PtsParser(parser), nil, str.cstring, uint32(len(str)))))

proc parseTsUngrammarString*(str: string): TsUngrammarNode =
  let parser = newTsUngrammarParser()
  return parseString(parser, str)

func `$`*(node: TsUngrammarNode): string =
  if isNil(node):
    "<nil tree>"

  else:
    $node.kind


func `[]`*(
    node: TsUngrammarNode,
    idx:  int,
    kind: UngrammarNodeKind | set[UngrammarNodeKind]
  ): TsUngrammarNode =
  assert 0 <= idx and idx < node.len
  result = TsUngrammarNode(ts_node_named_child(TSNode(node), uint32(idx)))
  assertKind(result, kind, "Child node at index " & $idx & " for node kind " & $node.kind)

type
  UngrammarNode* = HtsNode[TsUngrammarNode, UngrammarNodeKind]


proc treeReprTsUngrammar*(str: string, unnamed: bool = false): ColoredText =
  treeRepr[TsUngrammarNode, UngrammarNodeKind](parseTsUngrammarString(str), str, 9, unnamed = unnamed)

proc toHtsNode*(
    node: TsUngrammarNode,
    str:  ptr string
  ): HtsNode[TsUngrammarNode, UngrammarNodeKind] =
  toHtsNode[TsUngrammarNode, UngrammarNodeKind](node, str)

proc toHtsTree*(node: TsUngrammarNode, str: ptr string): UngrammarNode =
  toHtsNode[TsUngrammarNode, UngrammarNodeKind](node, str)

proc parseUngrammarString*(
    str:     ptr string,
    unnamed: bool = false
  ): UngrammarNode =
  let parser = newTsUngrammarParser()
  return toHtsTree[TsUngrammarNode, UngrammarNodeKind](parseString(parser, str[]), str)

proc parseUngrammarString*(str: string, unnamed: bool = false): UngrammarNode =
  let parser = newTsUngrammarParser()
  return toHtsTree[TsUngrammarNode, UngrammarNodeKind](parseString(parser, str), unsafeAddr str, storePtr = false)


import
  hmisc / wrappers/treesitter_core
let ungrammarGrammarRules*: array[UngrammarNodeKind, HtsRule[UngrammarNodeKind]] = block:
                                                                                var rules: array[UngrammarNodeKind, HtsRule[UngrammarNodeKind]]
                                                                                type
                                                                                  K = UngrammarNodeKind


                                                                                rules[ungrammarRepetition] = tsSeq[K](tsSymbol[K](ungrammarHidAtomRule), tsString[K]("*"))
                                                                                rules[ungrammarNodeRule] = tsSymbol[K](ungrammarIdentifier)
                                                                                rules[ungrammarToken] = tsSeq[K](tsString[K]("\'"), tsSymbol[K](ungrammarTokenIdentifier), tsString[K]("\'"))
                                                                                rules[ungrammarLabel] = tsSeq[K](tsSymbol[K](ungrammarIdentifier), tsString[K](":"), tsSymbol[K](ungrammarHidAtomRule))
                                                                                rules[ungrammarSequence] = tsRepeat1[K](tsSymbol[K](ungrammarHidAtomRule))
                                                                                rules[ungrammarComment] = tsSeq[K](tsString[K]("//"), tsRegex[K](".*"))
                                                                                rules[ungrammarIdentifier] = tsRegex[K]("[a-zA-Z_]+")
                                                                                rules[ungrammarAlternation] = tsSeq[K](tsSymbol[K](ungrammarSequence), tsRepeat[K](tsSeq[K](tsString[K]("|"), tsSymbol[K](ungrammarSequence))))
                                                                                rules[ungrammarRule] = tsSymbol[K](ungrammarAlternation)
                                                                                rules[ungrammarTokenIdentifier] = tsRegex[K]("(?:[^\'\\\\]|\\\\\\\\|\\\\\')+")
                                                                                rules[ungrammarNode] = tsSeq[K](tsSymbol[K](ungrammarIdentifier), tsString[K]("="), tsSymbol[K](ungrammarRule))
                                                                                rules[ungrammarGrammar] = tsRepeat[K](tsSymbol[K](ungrammarNode))
                                                                                rules[ungrammarOptional] = tsSeq[K](tsSymbol[K](ungrammarHidAtomRule), tsString[K]("?"))
                                                                                rules[ungrammarHidAtomRule] = tsChoice[K](tsSymbol[K](ungrammarNodeRule), tsSymbol[K](ungrammarToken), tsSymbol[K](ungrammarLabel), tsSeq[K](tsString[K]("("), tsSymbol[K](ungrammarRule), tsString[K](")")), tsSymbol[K](ungrammarRepetition), tsSymbol[K](ungrammarOptional))
                                                                                rules

