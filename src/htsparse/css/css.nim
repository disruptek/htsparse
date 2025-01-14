{.compile: "css_parser.c".}
{.compile: "css_scanner.c".}
{.passl: "-ltree-sitter".}

import css_wrapper
export css_wrapper

import ../common

const cssNodeKindMap* = toMapArray {
  cssComment: tskComment
}

initTreeRepr("Css", 3, cssNodeKindMap)
