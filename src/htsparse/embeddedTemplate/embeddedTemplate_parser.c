#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 14
#define LARGE_STATE_COUNT 4
#define SYMBOL_COUNT 16
#define ALIAS_COUNT 1
#define TOKEN_COUNT 11
#define EXTERNAL_TOKEN_COUNT 2
#define FIELD_COUNT 0
#define MAX_ALIAS_SEQUENCE_LENGTH 3

enum {
  anon_sym_LT_PERCENT = 1,
  anon_sym_LT_PERCENT_ = 2,
  anon_sym_PERCENT_GT = 3,
  anon_sym_DASH_PERCENT_GT = 4,
  anon_sym__PERCENT_GT = 5,
  anon_sym_LT_PERCENT_EQ = 6,
  anon_sym_LT_PERCENT_DASH = 7,
  anon_sym_LT_PERCENT_POUND = 8,
  sym_code = 9,
  sym_content = 10,
  sym_template = 11,
  sym_directive = 12,
  sym_output_directive = 13,
  sym_comment_directive = 14,
  aux_sym_template_repeat1 = 15,
  alias_sym_comment = 16,
};

static const char *ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_LT_PERCENT] = "<%",
  [anon_sym_LT_PERCENT_] = "<%_",
  [anon_sym_PERCENT_GT] = "%>",
  [anon_sym_DASH_PERCENT_GT] = "-%>",
  [anon_sym__PERCENT_GT] = "_%>",
  [anon_sym_LT_PERCENT_EQ] = "<%=",
  [anon_sym_LT_PERCENT_DASH] = "<%-",
  [anon_sym_LT_PERCENT_POUND] = "<%#",
  [sym_code] = "code",
  [sym_content] = "content",
  [sym_template] = "template",
  [sym_directive] = "directive",
  [sym_output_directive] = "output_directive",
  [sym_comment_directive] = "comment_directive",
  [aux_sym_template_repeat1] = "template_repeat1",
  [alias_sym_comment] = "comment",
};

static TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_LT_PERCENT] = anon_sym_LT_PERCENT,
  [anon_sym_LT_PERCENT_] = anon_sym_LT_PERCENT_,
  [anon_sym_PERCENT_GT] = anon_sym_PERCENT_GT,
  [anon_sym_DASH_PERCENT_GT] = anon_sym_DASH_PERCENT_GT,
  [anon_sym__PERCENT_GT] = anon_sym__PERCENT_GT,
  [anon_sym_LT_PERCENT_EQ] = anon_sym_LT_PERCENT_EQ,
  [anon_sym_LT_PERCENT_DASH] = anon_sym_LT_PERCENT_DASH,
  [anon_sym_LT_PERCENT_POUND] = anon_sym_LT_PERCENT_POUND,
  [sym_code] = sym_code,
  [sym_content] = sym_content,
  [sym_template] = sym_template,
  [sym_directive] = sym_directive,
  [sym_output_directive] = sym_output_directive,
  [sym_comment_directive] = sym_comment_directive,
  [aux_sym_template_repeat1] = aux_sym_template_repeat1,
  [alias_sym_comment] = alias_sym_comment,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_LT_PERCENT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT_PERCENT_] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PERCENT_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH_PERCENT_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym__PERCENT_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT_PERCENT_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT_PERCENT_DASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT_PERCENT_POUND] = {
    .visible = true,
    .named = false,
  },
  [sym_code] = {
    .visible = true,
    .named = true,
  },
  [sym_content] = {
    .visible = true,
    .named = true,
  },
  [sym_template] = {
    .visible = true,
    .named = true,
  },
  [sym_directive] = {
    .visible = true,
    .named = true,
  },
  [sym_output_directive] = {
    .visible = true,
    .named = true,
  },
  [sym_comment_directive] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_template_repeat1] = {
    .visible = false,
    .named = false,
  },
  [alias_sym_comment] = {
    .visible = true,
    .named = true,
  },
};

static TSSymbol ts_alias_sequences[2][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
  [1] = {
    [1] = alias_sym_comment,
  },
};

static uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(7);
      if (lookahead == '%') ADVANCE(4);
      if (lookahead == '-') ADVANCE(2);
      if (lookahead == '<') ADVANCE(1);
      if (lookahead == '_') ADVANCE(3);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      END_STATE();
    case 1:
      if (lookahead == '%') ADVANCE(8);
      END_STATE();
    case 2:
      if (lookahead == '%') ADVANCE(5);
      END_STATE();
    case 3:
      if (lookahead == '%') ADVANCE(6);
      END_STATE();
    case 4:
      if (lookahead == '>') ADVANCE(10);
      END_STATE();
    case 5:
      if (lookahead == '>') ADVANCE(11);
      END_STATE();
    case 6:
      if (lookahead == '>') ADVANCE(12);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(anon_sym_LT_PERCENT);
      if (lookahead == '#') ADVANCE(15);
      if (lookahead == '-') ADVANCE(14);
      if (lookahead == '=') ADVANCE(13);
      if (lookahead == '_') ADVANCE(9);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(anon_sym_LT_PERCENT_);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(anon_sym_PERCENT_GT);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(anon_sym_DASH_PERCENT_GT);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(anon_sym__PERCENT_GT);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(anon_sym_LT_PERCENT_EQ);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(anon_sym_LT_PERCENT_DASH);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(anon_sym_LT_PERCENT_POUND);
      END_STATE();
    default:
      return false;
  }
}

static TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0, .external_lex_state = 1},
  [1] = {.lex_state = 0, .external_lex_state = 2},
  [2] = {.lex_state = 0, .external_lex_state = 2},
  [3] = {.lex_state = 0, .external_lex_state = 2},
  [4] = {.lex_state = 0, .external_lex_state = 2},
  [5] = {.lex_state = 0, .external_lex_state = 2},
  [6] = {.lex_state = 0, .external_lex_state = 2},
  [7] = {.lex_state = 0},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0, .external_lex_state = 3},
  [10] = {.lex_state = 0, .external_lex_state = 3},
  [11] = {.lex_state = 0, .external_lex_state = 3},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
};

enum {
  ts_external_token_code = 0,
  ts_external_token_content = 1,
};

static TSSymbol ts_external_scanner_symbol_map[EXTERNAL_TOKEN_COUNT] = {
  [ts_external_token_code] = sym_code,
  [ts_external_token_content] = sym_content,
};

static bool ts_external_scanner_states[4][EXTERNAL_TOKEN_COUNT] = {
  [1] = {
    [ts_external_token_code] = true,
    [ts_external_token_content] = true,
  },
  [2] = {
    [ts_external_token_content] = true,
  },
  [3] = {
    [ts_external_token_code] = true,
  },
};

static uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_LT_PERCENT] = ACTIONS(1),
    [anon_sym_LT_PERCENT_] = ACTIONS(1),
    [anon_sym_PERCENT_GT] = ACTIONS(1),
    [anon_sym_DASH_PERCENT_GT] = ACTIONS(1),
    [anon_sym__PERCENT_GT] = ACTIONS(1),
    [anon_sym_LT_PERCENT_EQ] = ACTIONS(1),
    [anon_sym_LT_PERCENT_DASH] = ACTIONS(1),
    [anon_sym_LT_PERCENT_POUND] = ACTIONS(1),
    [sym_code] = ACTIONS(1),
    [sym_content] = ACTIONS(1),
  },
  [1] = {
    [sym_template] = STATE(12),
    [sym_directive] = STATE(2),
    [sym_output_directive] = STATE(2),
    [sym_comment_directive] = STATE(2),
    [aux_sym_template_repeat1] = STATE(2),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_LT_PERCENT] = ACTIONS(5),
    [anon_sym_LT_PERCENT_] = ACTIONS(7),
    [anon_sym_LT_PERCENT_EQ] = ACTIONS(9),
    [anon_sym_LT_PERCENT_DASH] = ACTIONS(9),
    [anon_sym_LT_PERCENT_POUND] = ACTIONS(11),
    [sym_content] = ACTIONS(13),
  },
  [2] = {
    [sym_directive] = STATE(3),
    [sym_output_directive] = STATE(3),
    [sym_comment_directive] = STATE(3),
    [aux_sym_template_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(15),
    [anon_sym_LT_PERCENT] = ACTIONS(5),
    [anon_sym_LT_PERCENT_] = ACTIONS(7),
    [anon_sym_LT_PERCENT_EQ] = ACTIONS(9),
    [anon_sym_LT_PERCENT_DASH] = ACTIONS(9),
    [anon_sym_LT_PERCENT_POUND] = ACTIONS(11),
    [sym_content] = ACTIONS(17),
  },
  [3] = {
    [sym_directive] = STATE(3),
    [sym_output_directive] = STATE(3),
    [sym_comment_directive] = STATE(3),
    [aux_sym_template_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(19),
    [anon_sym_LT_PERCENT] = ACTIONS(21),
    [anon_sym_LT_PERCENT_] = ACTIONS(24),
    [anon_sym_LT_PERCENT_EQ] = ACTIONS(27),
    [anon_sym_LT_PERCENT_DASH] = ACTIONS(27),
    [anon_sym_LT_PERCENT_POUND] = ACTIONS(30),
    [sym_content] = ACTIONS(33),
  },
};

static uint16_t ts_small_parse_table[] = {
  [0] = 2,
    ACTIONS(38), 1,
      anon_sym_LT_PERCENT,
    ACTIONS(36), 6,
      sym_content,
      ts_builtin_sym_end,
      anon_sym_LT_PERCENT_,
      anon_sym_LT_PERCENT_EQ,
      anon_sym_LT_PERCENT_DASH,
      anon_sym_LT_PERCENT_POUND,
  [12] = 2,
    ACTIONS(42), 1,
      anon_sym_LT_PERCENT,
    ACTIONS(40), 6,
      sym_content,
      ts_builtin_sym_end,
      anon_sym_LT_PERCENT_,
      anon_sym_LT_PERCENT_EQ,
      anon_sym_LT_PERCENT_DASH,
      anon_sym_LT_PERCENT_POUND,
  [24] = 2,
    ACTIONS(46), 1,
      anon_sym_LT_PERCENT,
    ACTIONS(44), 6,
      sym_content,
      ts_builtin_sym_end,
      anon_sym_LT_PERCENT_,
      anon_sym_LT_PERCENT_EQ,
      anon_sym_LT_PERCENT_DASH,
      anon_sym_LT_PERCENT_POUND,
  [36] = 1,
    ACTIONS(48), 3,
      anon_sym_PERCENT_GT,
      anon_sym_DASH_PERCENT_GT,
      anon_sym__PERCENT_GT,
  [42] = 1,
    ACTIONS(50), 2,
      anon_sym_PERCENT_GT,
      anon_sym_DASH_PERCENT_GT,
  [47] = 1,
    ACTIONS(52), 1,
      sym_code,
  [51] = 1,
    ACTIONS(54), 1,
      sym_code,
  [55] = 1,
    ACTIONS(56), 1,
      sym_code,
  [59] = 1,
    ACTIONS(58), 1,
      ts_builtin_sym_end,
  [63] = 1,
    ACTIONS(60), 1,
      anon_sym_PERCENT_GT,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(4)] = 0,
  [SMALL_STATE(5)] = 12,
  [SMALL_STATE(6)] = 24,
  [SMALL_STATE(7)] = 36,
  [SMALL_STATE(8)] = 42,
  [SMALL_STATE(9)] = 47,
  [SMALL_STATE(10)] = 51,
  [SMALL_STATE(11)] = 55,
  [SMALL_STATE(12)] = 59,
  [SMALL_STATE(13)] = 63,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_template, 0),
  [5] = {.entry = {.count = 1, .reusable = false}}, SHIFT(9),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [15] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_template, 1),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [19] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_template_repeat1, 2),
  [21] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_template_repeat1, 2), SHIFT_REPEAT(9),
  [24] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_template_repeat1, 2), SHIFT_REPEAT(9),
  [27] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_template_repeat1, 2), SHIFT_REPEAT(10),
  [30] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_template_repeat1, 2), SHIFT_REPEAT(11),
  [33] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_template_repeat1, 2), SHIFT_REPEAT(3),
  [36] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_directive, 3),
  [38] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_directive, 3),
  [40] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_output_directive, 3),
  [42] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_output_directive, 3),
  [44] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_comment_directive, 3, .production_id = 1),
  [46] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_comment_directive, 3, .production_id = 1),
  [48] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [50] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [52] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [54] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [56] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [58] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [60] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
};

#ifdef __cplusplus
extern "C" {
#endif
void *tree_sitter_embedded_template_external_scanner_create(void);
void tree_sitter_embedded_template_external_scanner_destroy(void *);
bool tree_sitter_embedded_template_external_scanner_scan(void *, TSLexer *, const bool *);
unsigned tree_sitter_embedded_template_external_scanner_serialize(void *, char *);
void tree_sitter_embedded_template_external_scanner_deserialize(void *, const char *, unsigned);

#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_embedded_template(void) {
  static TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .symbol_names = ts_symbol_names,
    .symbol_metadata = ts_symbol_metadata,
    .parse_table = (const uint16_t *)ts_parse_table,
    .parse_actions = ts_parse_actions,
    .lex_modes = ts_lex_modes,
    .alias_sequences = (const TSSymbol *)ts_alias_sequences,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .lex_fn = ts_lex,
    .external_scanner = {
      (const bool *)ts_external_scanner_states,
      ts_external_scanner_symbol_map,
      tree_sitter_embedded_template_external_scanner_create,
      tree_sitter_embedded_template_external_scanner_destroy,
      tree_sitter_embedded_template_external_scanner_scan,
      tree_sitter_embedded_template_external_scanner_serialize,
      tree_sitter_embedded_template_external_scanner_deserialize,
    },
    .field_count = FIELD_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .small_parse_table = (const uint16_t *)ts_small_parse_table,
    .small_parse_table_map = (const uint32_t *)ts_small_parse_table_map,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .state_count = STATE_COUNT,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
