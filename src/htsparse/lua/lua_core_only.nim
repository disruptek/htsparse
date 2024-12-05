{.compile: "lua_parser.c".}
{.compile: "lua_scanner.c".}
{.passl: "-ltree-sitter".}

import lua_wrapper_core
export lua_wrapper_core
