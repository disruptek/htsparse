import
  hmisc / wrappers/treesitter,
  hmisc / core/all,
  hmisc / types/colorstring,
  std/strutils
export treesitter

type
  C_sharpNodeKind* = enum
    c_sharpUsDeclaration                                 ## _declaration
    c_sharpUsExpression                                  ## _expression
    c_sharpUsStatement                                   ## _statement
    c_sharpUsType                                        ## _type
    c_sharpAccessorDeclaration                           ## accessor_declaration
    c_sharpAccessorList                                  ## accessor_list
    c_sharpAliasQualifiedName                            ## alias_qualified_name
    c_sharpAnonymousMethodExpression                     ## anonymous_method_expression
    c_sharpAnonymousObjectCreationExpression             ## anonymous_object_creation_expression
    c_sharpArgument                                      ## argument
    c_sharpArgumentList                                  ## argument_list
    c_sharpArrayCreationExpression                       ## array_creation_expression
    c_sharpArrayRankSpecifier                            ## array_rank_specifier
    c_sharpArrayType                                     ## array_type
    c_sharpArrowExpressionClause                         ## arrow_expression_clause
    c_sharpAsExpression                                  ## as_expression
    c_sharpAssignmentExpression                          ## assignment_expression
    c_sharpAssignmentOperator                            ## assignment_operator
    c_sharpAttribute                                     ## attribute
    c_sharpAttributeArgument                             ## attribute_argument
    c_sharpAttributeArgumentList                         ## attribute_argument_list
    c_sharpAttributeList                                 ## attribute_list
    c_sharpAttributeTargetSpecifier                      ## attribute_target_specifier
    c_sharpAwaitExpression                               ## await_expression
    c_sharpBaseExpression                                ## base_expression
    c_sharpBaseList                                      ## base_list
    c_sharpBinaryExpression                              ## binary_expression
    c_sharpBinaryPattern                                 ## binary_pattern
    c_sharpBlock                                         ## block
    c_sharpBooleanLiteral                                ## boolean_literal
    c_sharpBracketedArgumentList                         ## bracketed_argument_list
    c_sharpBracketedParameterList                        ## bracketed_parameter_list
    c_sharpBreakStatement                                ## break_statement
    c_sharpCasePatternSwitchLabel                        ## case_pattern_switch_label
    c_sharpCaseSwitchLabel                               ## case_switch_label
    c_sharpCastExpression                                ## cast_expression
    c_sharpCatchClause                                   ## catch_clause
    c_sharpCatchDeclaration                              ## catch_declaration
    c_sharpCatchFilterClause                             ## catch_filter_clause
    c_sharpCharacterLiteral                              ## character_literal
    c_sharpCheckedExpression                             ## checked_expression
    c_sharpCheckedStatement                              ## checked_statement
    c_sharpClassDeclaration                              ## class_declaration
    c_sharpCompilationUnit                               ## compilation_unit
    c_sharpConditionalAccessExpression                   ## conditional_access_expression
    c_sharpConditionalExpression                         ## conditional_expression
    c_sharpConstantPattern                               ## constant_pattern
    c_sharpConstructorConstraint                         ## constructor_constraint
    c_sharpConstructorDeclaration                        ## constructor_declaration
    c_sharpConstructorInitializer                        ## constructor_initializer
    c_sharpContinueStatement                             ## continue_statement
    c_sharpConversionOperatorDeclaration                 ## conversion_operator_declaration
    c_sharpDeclarationExpression                         ## declaration_expression
    c_sharpDeclarationList                               ## declaration_list
    c_sharpDeclarationPattern                            ## declaration_pattern
    c_sharpDefaultExpression                             ## default_expression
    c_sharpDefaultSwitchLabel                            ## default_switch_label
    c_sharpDelegateDeclaration                           ## delegate_declaration
    c_sharpDestructorDeclaration                         ## destructor_declaration
    c_sharpDoStatement                                   ## do_statement
    c_sharpElementAccessExpression                       ## element_access_expression
    c_sharpElementBindingExpression                      ## element_binding_expression
    c_sharpEmptyStatement                                ## empty_statement
    c_sharpEnumDeclaration                               ## enum_declaration
    c_sharpEnumMemberDeclaration                         ## enum_member_declaration
    c_sharpEnumMemberDeclarationList                     ## enum_member_declaration_list
    c_sharpEqualsValueClause                             ## equals_value_clause
    c_sharpEventDeclaration                              ## event_declaration
    c_sharpEventFieldDeclaration                         ## event_field_declaration
    c_sharpExplicitInterfaceSpecifier                    ## explicit_interface_specifier
    c_sharpExpressionStatement                           ## expression_statement
    c_sharpExternAliasDirective                          ## extern_alias_directive
    c_sharpFieldDeclaration                              ## field_declaration
    c_sharpFinallyClause                                 ## finally_clause
    c_sharpFixedStatement                                ## fixed_statement
    c_sharpForEachStatement                              ## for_each_statement
    c_sharpForStatement                                  ## for_statement
    c_sharpFromClause                                    ## from_clause
    c_sharpFunctionPointerCallingConvention              ## function_pointer_calling_convention
    c_sharpFunctionPointerParameter                      ## function_pointer_parameter
    c_sharpFunctionPointerType                           ## function_pointer_type
    c_sharpFunctionPointerUnmanagedCallingConvention     ## function_pointer_unmanaged_calling_convention
    c_sharpFunctionPointerUnmanagedCallingConventionList ## function_pointer_unmanaged_calling_convention_list
    c_sharpGenericName                                   ## generic_name
    c_sharpGlobal                                        ## global
    c_sharpGlobalAttributeList                           ## global_attribute_list
    c_sharpGlobalStatement                               ## global_statement
    c_sharpGotoStatement                                 ## goto_statement
    c_sharpGroupClause                                   ## group_clause
    c_sharpIdentifier                                    ## identifier
    c_sharpIfStatement                                   ## if_statement
    c_sharpImplicitArrayCreationExpression               ## implicit_array_creation_expression
    c_sharpImplicitObjectCreationExpression              ## implicit_object_creation_expression
    c_sharpImplicitStackAllocArrayCreationExpression     ## implicit_stack_alloc_array_creation_expression
    c_sharpImplicitType                                  ## implicit_type
    c_sharpIndexerDeclaration                            ## indexer_declaration
    c_sharpInitializerExpression                         ## initializer_expression
    c_sharpInterfaceDeclaration                          ## interface_declaration
    c_sharpInterpolatedStringExpression                  ## interpolated_string_expression
    c_sharpInterpolatedStringText                        ## interpolated_string_text
    c_sharpInterpolatedVerbatimStringText                ## interpolated_verbatim_string_text
    c_sharpInterpolation                                 ## interpolation
    c_sharpInterpolationAlignmentClause                  ## interpolation_alignment_clause
    c_sharpInterpolationFormatClause                     ## interpolation_format_clause
    c_sharpInvocationExpression                          ## invocation_expression
    c_sharpIsExpression                                  ## is_expression
    c_sharpIsPatternExpression                           ## is_pattern_expression
    c_sharpJoinClause                                    ## join_clause
    c_sharpJoinIntoClause                                ## join_into_clause
    c_sharpLabelName                                     ## label_name
    c_sharpLabeledStatement                              ## labeled_statement
    c_sharpLambdaExpression                              ## lambda_expression
    c_sharpLetClause                                     ## let_clause
    c_sharpLocalDeclarationStatement                     ## local_declaration_statement
    c_sharpLocalFunctionStatement                        ## local_function_statement
    c_sharpLockStatement                                 ## lock_statement
    c_sharpMakeRefExpression                             ## make_ref_expression
    c_sharpMemberAccessExpression                        ## member_access_expression
    c_sharpMemberBindingExpression                       ## member_binding_expression
    c_sharpMethodDeclaration                             ## method_declaration
    c_sharpModifier                                      ## modifier
    c_sharpNameColon                                     ## name_colon
    c_sharpNameEquals                                    ## name_equals
    c_sharpNamespaceDeclaration                          ## namespace_declaration
    c_sharpNegatedPattern                                ## negated_pattern
    c_sharpNullableType                                  ## nullable_type
    c_sharpObjectCreationExpression                      ## object_creation_expression
    c_sharpOperatorDeclaration                           ## operator_declaration
    c_sharpOrderByClause                                 ## order_by_clause
    c_sharpParameter                                     ## parameter
    c_sharpParameterArray                                ## parameter_array
    c_sharpParameterList                                 ## parameter_list
    c_sharpParameterModifier                             ## parameter_modifier
    c_sharpParenthesizedExpression                       ## parenthesized_expression
    c_sharpParenthesizedPattern                          ## parenthesized_pattern
    c_sharpParenthesizedVariableDesignation              ## parenthesized_variable_designation
    c_sharpPointerType                                   ## pointer_type
    c_sharpPositionalPatternClause                       ## positional_pattern_clause
    c_sharpPostfixUnaryExpression                        ## postfix_unary_expression
    c_sharpPrefixUnaryExpression                         ## prefix_unary_expression
    c_sharpPreprocessorCall                              ## preprocessor_call
    c_sharpPrimaryConstructorBaseType                    ## primary_constructor_base_type
    c_sharpPropertyDeclaration                           ## property_declaration
    c_sharpPropertyPatternClause                         ## property_pattern_clause
    c_sharpQualifiedName                                 ## qualified_name
    c_sharpQueryContinuation                             ## query_continuation
    c_sharpQueryExpression                               ## query_expression
    c_sharpRangeExpression                               ## range_expression
    c_sharpRecordDeclaration                             ## record_declaration
    c_sharpRecursivePattern                              ## recursive_pattern
    c_sharpRefExpression                                 ## ref_expression
    c_sharpRefTypeExpression                             ## ref_type_expression
    c_sharpRefValueExpression                            ## ref_value_expression
    c_sharpRelationalPattern                             ## relational_pattern
    c_sharpReturnStatement                               ## return_statement
    c_sharpSelectClause                                  ## select_clause
    c_sharpSimpleAssignmentExpression                    ## simple_assignment_expression
    c_sharpSizeOfExpression                              ## size_of_expression
    c_sharpStackAllocArrayCreationExpression             ## stack_alloc_array_creation_expression
    c_sharpStringLiteral                                 ## string_literal
    c_sharpStructDeclaration                             ## struct_declaration
    c_sharpSubpattern                                    ## subpattern
    c_sharpSwitchBody                                    ## switch_body
    c_sharpSwitchExpression                              ## switch_expression
    c_sharpSwitchExpressionArm                           ## switch_expression_arm
    c_sharpSwitchSection                                 ## switch_section
    c_sharpSwitchStatement                               ## switch_statement
    c_sharpThisExpression                                ## this_expression
    c_sharpThrowExpression                               ## throw_expression
    c_sharpThrowStatement                                ## throw_statement
    c_sharpTryStatement                                  ## try_statement
    c_sharpTupleElement                                  ## tuple_element
    c_sharpTupleExpression                               ## tuple_expression
    c_sharpTuplePattern                                  ## tuple_pattern
    c_sharpTupleType                                     ## tuple_type
    c_sharpTypeArgumentList                              ## type_argument_list
    c_sharpTypeConstraint                                ## type_constraint
    c_sharpTypeOfExpression                              ## type_of_expression
    c_sharpTypeParameter                                 ## type_parameter
    c_sharpTypeParameterConstraint                       ## type_parameter_constraint
    c_sharpTypeParameterConstraintsClause                ## type_parameter_constraints_clause
    c_sharpTypeParameterList                             ## type_parameter_list
    c_sharpTypePattern                                   ## type_pattern
    c_sharpUnsafeStatement                               ## unsafe_statement
    c_sharpUsingDirective                                ## using_directive
    c_sharpUsingStatement                                ## using_statement
    c_sharpVarPattern                                    ## var_pattern
    c_sharpVariableDeclaration                           ## variable_declaration
    c_sharpVariableDeclarator                            ## variable_declarator
    c_sharpWhenClause                                    ## when_clause
    c_sharpWhereClause                                   ## where_clause
    c_sharpWhileStatement                                ## while_statement
    c_sharpWithExpression                                ## with_expression
    c_sharpWithInitializerExpression                     ## with_initializer_expression
    c_sharpYieldStatement                                ## yield_statement
    c_sharpExclamationTok                                ## !
    c_sharpExclamationEqualTok                           ## !=
    c_sharpQuoteTok                                      ## "
    c_sharpDoubleQuoteTok                                ## ""
    c_sharpDollarQuoteTok                                ## $"
    c_sharpDollarAtQuoteTok                              ## $@"
    c_sharpPercentTok                                    ## %
    c_sharpPercentEqualTok                               ## %=
    c_sharpAmpersandTok                                  ## &
    c_sharpDoubleAmpersandTok                            ## &&
    c_sharpAmpersandEqualTok                             ## &=
    c_sharpApostropheTok                                 ## '
    c_sharpLParTok                                       ## (
    c_sharpRParTok                                       ## )
    c_sharpAsteriskTok                                   ## *
    c_sharpAsteriskEqualTok                              ## *=
    c_sharpPlusTok                                       ## +
    c_sharpDoublePlusTok                                 ## ++
    c_sharpPlusEqualTok                                  ## +=
    c_sharpCommaTok                                      ## ,
    c_sharpMinusTok                                      ## -
    c_sharpDoubleMinusTok                                ## --
    c_sharpMinusEqualTok                                 ## -=
    c_sharpMinusGreaterThanTok                           ## ->
    c_sharpDotTok                                        ## .
    c_sharpDoubleDotTok                                  ## ..
    c_sharpSlashTok                                      ## /
    c_sharpSlashEqualTok                                 ## /=
    c_sharpColonTok                                      ## :
    c_sharpDoubleColonTok                                ## ::
    c_sharpSemicolonTok                                  ## ;
    c_sharpLessThanTok                                   ## <
    c_sharpDoubleLessThanTok                             ## <<
    c_sharpDoubleLessThanEqualTok                        ## <<=
    c_sharpLessThanEqualTok                              ## <=
    c_sharpEqualTok                                      ## =
    c_sharpDoubleEqualTok                                ## ==
    c_sharpEqualGreaterThanTok                           ## =>
    c_sharpGreaterThanTok                                ## >
    c_sharpGreaterThanEqualTok                           ## >=
    c_sharpDoubleGreaterThanTok                          ## >>
    c_sharpDoubleGreaterThanEqualTok                     ## >>=
    c_sharpQuestionTok                                   ## ?
    c_sharpDoubleQuestionTok                             ## ??
    c_sharpDoubleQuestionEqualTok                        ## ??=
    c_sharpAtDollarQuoteTok                              ## @$"
    c_sharpCdeclTok                                      ## Cdecl
    c_sharpFastcallTok                                   ## Fastcall
    c_sharpStdcallTok                                    ## Stdcall
    c_sharpThiscallTok                                   ## Thiscall
    c_sharpLBrackTok                                     ## [
    c_sharpRBrackTok                                     ## ]
    c_sharpAccentTok                                     ## ^
    c_sharpAccentEqualTok                                ## ^=
    c_sharpUsUsMakerefTok                                ## __makeref
    c_sharpUsUsReftypeTok                                ## __reftype
    c_sharpUsUsRefvalueTok                               ## __refvalue
    c_sharpAbstractTok                                   ## abstract
    c_sharpAddTok                                        ## add
    c_sharpAliasTok                                      ## alias
    c_sharpAndTok                                        ## and
    c_sharpAsTok                                         ## as
    c_sharpAscendingTok                                  ## ascending
    c_sharpAssemblyTok                                   ## assembly
    c_sharpAsyncTok                                      ## async
    c_sharpAwaitTok                                      ## await
    c_sharpBaseTok                                       ## base
    c_sharpBreakTok                                      ## break
    c_sharpByTok                                         ## by
    c_sharpCaseTok                                       ## case
    c_sharpCatchTok                                      ## catch
    c_sharpCheckedTok                                    ## checked
    c_sharpClassTok                                      ## class
    c_sharpComment                                       ## comment
    c_sharpConstTok                                      ## const
    c_sharpContinueTok                                   ## continue
    c_sharpDefaultTok                                    ## default
    c_sharpDelegateTok                                   ## delegate
    c_sharpDescendingTok                                 ## descending
    c_sharpDiscard                                       ## discard
    c_sharpDoTok                                         ## do
    c_sharpDynamicTok                                    ## dynamic
    c_sharpElseTok                                       ## else
    c_sharpEnumTok                                       ## enum
    c_sharpEqualsTok                                     ## equals
    c_sharpEscapeSequence                                ## escape_sequence
    c_sharpEventTok                                      ## event
    c_sharpExplicitTok                                   ## explicit
    c_sharpExternTok                                     ## extern
    c_sharpFalseTok                                      ## false
    c_sharpFieldTok                                      ## field
    c_sharpFinallyTok                                    ## finally
    c_sharpFixedTok                                      ## fixed
    c_sharpForTok                                        ## for
    c_sharpForeachTok                                    ## foreach
    c_sharpFromTok                                       ## from
    c_sharpGetTok                                        ## get
    c_sharpGlobalTok                                     ## global
    c_sharpGotoTok                                       ## goto
    c_sharpGroupTok                                      ## group
    c_sharpIfTok                                         ## if
    c_sharpImplicitTok                                   ## implicit
    c_sharpInTok                                         ## in
    c_sharpInitTok                                       ## init
    c_sharpIntegerLiteral                                ## integer_literal
    c_sharpInterfaceTok                                  ## interface
    c_sharpInternalTok                                   ## internal
    c_sharpIntoTok                                       ## into
    c_sharpIsTok                                         ## is
    c_sharpJoinTok                                       ## join
    c_sharpLetTok                                        ## let
    c_sharpLockTok                                       ## lock
    c_sharpManagedTok                                    ## managed
    c_sharpMethodTok                                     ## method
    c_sharpModuleTok                                     ## module
    c_sharpNameofTok                                     ## nameof
    c_sharpNamespaceTok                                  ## namespace
    c_sharpNewTok                                        ## new
    c_sharpNotTok                                        ## not
    c_sharpNotnullTok                                    ## notnull
    c_sharpNullLiteral                                   ## null_literal
    c_sharpOnTok                                         ## on
    c_sharpOperatorTok                                   ## operator
    c_sharpOrTok                                         ## or
    c_sharpOrderbyTok                                    ## orderby
    c_sharpOutTok                                        ## out
    c_sharpOverrideTok                                   ## override
    c_sharpParamTok                                      ## param
    c_sharpParamsTok                                     ## params
    c_sharpPartialTok                                    ## partial
    c_sharpPredefinedType                                ## predefined_type
    c_sharpPreprocessorDirective                         ## preprocessor_directive
    c_sharpPrivateTok                                    ## private
    c_sharpPropertyTok                                   ## property
    c_sharpProtectedTok                                  ## protected
    c_sharpPublicTok                                     ## public
    c_sharpReadonlyTok                                   ## readonly
    c_sharpRealLiteral                                   ## real_literal
    c_sharpRecordTok                                     ## record
    c_sharpRefTok                                        ## ref
    c_sharpRemoveTok                                     ## remove
    c_sharpReturnTok                                     ## return
    c_sharpSealedTok                                     ## sealed
    c_sharpSelectTok                                     ## select
    c_sharpSetTok                                        ## set
    c_sharpSizeofTok                                     ## sizeof
    c_sharpStackallocTok                                 ## stackalloc
    c_sharpStaticTok                                     ## static
    c_sharpStructTok                                     ## struct
    c_sharpSwitchTok                                     ## switch
    c_sharpThisTok                                       ## this
    c_sharpThrowTok                                      ## throw
    c_sharpTrueTok                                       ## true
    c_sharpTryTok                                        ## try
    c_sharpTypeTok                                       ## type
    c_sharpTypeofTok                                     ## typeof
    c_sharpUncheckedTok                                  ## unchecked
    c_sharpUnmanagedTok                                  ## unmanaged
    c_sharpUnsafeTok                                     ## unsafe
    c_sharpUsingTok                                      ## using
    c_sharpVarTok                                        ## var
    c_sharpVerbatimStringLiteral                         ## verbatim_string_literal
    c_sharpVirtualTok                                    ## virtual
    c_sharpVoidKeyword                                   ## void_keyword
    c_sharpVolatileTok                                   ## volatile
    c_sharpWhenTok                                       ## when
    c_sharpWhereTok                                      ## where
    c_sharpWhileTok                                      ## while
    c_sharpWithTok                                       ## with
    c_sharpYieldTok                                      ## yield
    c_sharpLCurlyTok                                     ## {
    c_sharpDoubleLCurlyTok                               ## {{
    c_sharpPipeTok                                       ## |
    c_sharpPipeEqualTok                                  ## |=
    c_sharpDoublePipeTok                                 ## ||
    c_sharpRCurlyTok                                     ## }
    c_sharpTildeTok                                      ## ~
    c_sharpSyntaxError                                   ## Tree-sitter parser syntax error


proc strRepr*(kind: C_sharpNodeKind): string =
  case kind:
    of c_sharpUsDeclaration:                                 "_declaration"
    of c_sharpUsExpression:                                  "_expression"
    of c_sharpUsStatement:                                   "_statement"
    of c_sharpUsType:                                        "_type"
    of c_sharpAccessorDeclaration:                           "accessor_declaration"
    of c_sharpAccessorList:                                  "accessor_list"
    of c_sharpAliasQualifiedName:                            "alias_qualified_name"
    of c_sharpAnonymousMethodExpression:                     "anonymous_method_expression"
    of c_sharpAnonymousObjectCreationExpression:             "anonymous_object_creation_expression"
    of c_sharpArgument:                                      "argument"
    of c_sharpArgumentList:                                  "argument_list"
    of c_sharpArrayCreationExpression:                       "array_creation_expression"
    of c_sharpArrayRankSpecifier:                            "array_rank_specifier"
    of c_sharpArrayType:                                     "array_type"
    of c_sharpArrowExpressionClause:                         "arrow_expression_clause"
    of c_sharpAsExpression:                                  "as_expression"
    of c_sharpAssignmentExpression:                          "assignment_expression"
    of c_sharpAssignmentOperator:                            "assignment_operator"
    of c_sharpAttribute:                                     "attribute"
    of c_sharpAttributeArgument:                             "attribute_argument"
    of c_sharpAttributeArgumentList:                         "attribute_argument_list"
    of c_sharpAttributeList:                                 "attribute_list"
    of c_sharpAttributeTargetSpecifier:                      "attribute_target_specifier"
    of c_sharpAwaitExpression:                               "await_expression"
    of c_sharpBaseExpression:                                "base_expression"
    of c_sharpBaseList:                                      "base_list"
    of c_sharpBinaryExpression:                              "binary_expression"
    of c_sharpBinaryPattern:                                 "binary_pattern"
    of c_sharpBlock:                                         "block"
    of c_sharpBooleanLiteral:                                "boolean_literal"
    of c_sharpBracketedArgumentList:                         "bracketed_argument_list"
    of c_sharpBracketedParameterList:                        "bracketed_parameter_list"
    of c_sharpBreakStatement:                                "break_statement"
    of c_sharpCasePatternSwitchLabel:                        "case_pattern_switch_label"
    of c_sharpCaseSwitchLabel:                               "case_switch_label"
    of c_sharpCastExpression:                                "cast_expression"
    of c_sharpCatchClause:                                   "catch_clause"
    of c_sharpCatchDeclaration:                              "catch_declaration"
    of c_sharpCatchFilterClause:                             "catch_filter_clause"
    of c_sharpCharacterLiteral:                              "character_literal"
    of c_sharpCheckedExpression:                             "checked_expression"
    of c_sharpCheckedStatement:                              "checked_statement"
    of c_sharpClassDeclaration:                              "class_declaration"
    of c_sharpCompilationUnit:                               "compilation_unit"
    of c_sharpConditionalAccessExpression:                   "conditional_access_expression"
    of c_sharpConditionalExpression:                         "conditional_expression"
    of c_sharpConstantPattern:                               "constant_pattern"
    of c_sharpConstructorConstraint:                         "constructor_constraint"
    of c_sharpConstructorDeclaration:                        "constructor_declaration"
    of c_sharpConstructorInitializer:                        "constructor_initializer"
    of c_sharpContinueStatement:                             "continue_statement"
    of c_sharpConversionOperatorDeclaration:                 "conversion_operator_declaration"
    of c_sharpDeclarationExpression:                         "declaration_expression"
    of c_sharpDeclarationList:                               "declaration_list"
    of c_sharpDeclarationPattern:                            "declaration_pattern"
    of c_sharpDefaultExpression:                             "default_expression"
    of c_sharpDefaultSwitchLabel:                            "default_switch_label"
    of c_sharpDelegateDeclaration:                           "delegate_declaration"
    of c_sharpDestructorDeclaration:                         "destructor_declaration"
    of c_sharpDoStatement:                                   "do_statement"
    of c_sharpElementAccessExpression:                       "element_access_expression"
    of c_sharpElementBindingExpression:                      "element_binding_expression"
    of c_sharpEmptyStatement:                                "empty_statement"
    of c_sharpEnumDeclaration:                               "enum_declaration"
    of c_sharpEnumMemberDeclaration:                         "enum_member_declaration"
    of c_sharpEnumMemberDeclarationList:                     "enum_member_declaration_list"
    of c_sharpEqualsValueClause:                             "equals_value_clause"
    of c_sharpEventDeclaration:                              "event_declaration"
    of c_sharpEventFieldDeclaration:                         "event_field_declaration"
    of c_sharpExplicitInterfaceSpecifier:                    "explicit_interface_specifier"
    of c_sharpExpressionStatement:                           "expression_statement"
    of c_sharpExternAliasDirective:                          "extern_alias_directive"
    of c_sharpFieldDeclaration:                              "field_declaration"
    of c_sharpFinallyClause:                                 "finally_clause"
    of c_sharpFixedStatement:                                "fixed_statement"
    of c_sharpForEachStatement:                              "for_each_statement"
    of c_sharpForStatement:                                  "for_statement"
    of c_sharpFromClause:                                    "from_clause"
    of c_sharpFunctionPointerCallingConvention:              "function_pointer_calling_convention"
    of c_sharpFunctionPointerParameter:                      "function_pointer_parameter"
    of c_sharpFunctionPointerType:                           "function_pointer_type"
    of c_sharpFunctionPointerUnmanagedCallingConvention:     "function_pointer_unmanaged_calling_convention"
    of c_sharpFunctionPointerUnmanagedCallingConventionList: "function_pointer_unmanaged_calling_convention_list"
    of c_sharpGenericName:                                   "generic_name"
    of c_sharpGlobal:                                        "global"
    of c_sharpGlobalAttributeList:                           "global_attribute_list"
    of c_sharpGlobalStatement:                               "global_statement"
    of c_sharpGotoStatement:                                 "goto_statement"
    of c_sharpGroupClause:                                   "group_clause"
    of c_sharpIdentifier:                                    "identifier"
    of c_sharpIfStatement:                                   "if_statement"
    of c_sharpImplicitArrayCreationExpression:               "implicit_array_creation_expression"
    of c_sharpImplicitObjectCreationExpression:              "implicit_object_creation_expression"
    of c_sharpImplicitStackAllocArrayCreationExpression:     "implicit_stack_alloc_array_creation_expression"
    of c_sharpImplicitType:                                  "implicit_type"
    of c_sharpIndexerDeclaration:                            "indexer_declaration"
    of c_sharpInitializerExpression:                         "initializer_expression"
    of c_sharpInterfaceDeclaration:                          "interface_declaration"
    of c_sharpInterpolatedStringExpression:                  "interpolated_string_expression"
    of c_sharpInterpolatedStringText:                        "interpolated_string_text"
    of c_sharpInterpolatedVerbatimStringText:                "interpolated_verbatim_string_text"
    of c_sharpInterpolation:                                 "interpolation"
    of c_sharpInterpolationAlignmentClause:                  "interpolation_alignment_clause"
    of c_sharpInterpolationFormatClause:                     "interpolation_format_clause"
    of c_sharpInvocationExpression:                          "invocation_expression"
    of c_sharpIsExpression:                                  "is_expression"
    of c_sharpIsPatternExpression:                           "is_pattern_expression"
    of c_sharpJoinClause:                                    "join_clause"
    of c_sharpJoinIntoClause:                                "join_into_clause"
    of c_sharpLabelName:                                     "label_name"
    of c_sharpLabeledStatement:                              "labeled_statement"
    of c_sharpLambdaExpression:                              "lambda_expression"
    of c_sharpLetClause:                                     "let_clause"
    of c_sharpLocalDeclarationStatement:                     "local_declaration_statement"
    of c_sharpLocalFunctionStatement:                        "local_function_statement"
    of c_sharpLockStatement:                                 "lock_statement"
    of c_sharpMakeRefExpression:                             "make_ref_expression"
    of c_sharpMemberAccessExpression:                        "member_access_expression"
    of c_sharpMemberBindingExpression:                       "member_binding_expression"
    of c_sharpMethodDeclaration:                             "method_declaration"
    of c_sharpModifier:                                      "modifier"
    of c_sharpNameColon:                                     "name_colon"
    of c_sharpNameEquals:                                    "name_equals"
    of c_sharpNamespaceDeclaration:                          "namespace_declaration"
    of c_sharpNegatedPattern:                                "negated_pattern"
    of c_sharpNullableType:                                  "nullable_type"
    of c_sharpObjectCreationExpression:                      "object_creation_expression"
    of c_sharpOperatorDeclaration:                           "operator_declaration"
    of c_sharpOrderByClause:                                 "order_by_clause"
    of c_sharpParameter:                                     "parameter"
    of c_sharpParameterArray:                                "parameter_array"
    of c_sharpParameterList:                                 "parameter_list"
    of c_sharpParameterModifier:                             "parameter_modifier"
    of c_sharpParenthesizedExpression:                       "parenthesized_expression"
    of c_sharpParenthesizedPattern:                          "parenthesized_pattern"
    of c_sharpParenthesizedVariableDesignation:              "parenthesized_variable_designation"
    of c_sharpPointerType:                                   "pointer_type"
    of c_sharpPositionalPatternClause:                       "positional_pattern_clause"
    of c_sharpPostfixUnaryExpression:                        "postfix_unary_expression"
    of c_sharpPrefixUnaryExpression:                         "prefix_unary_expression"
    of c_sharpPreprocessorCall:                              "preprocessor_call"
    of c_sharpPrimaryConstructorBaseType:                    "primary_constructor_base_type"
    of c_sharpPropertyDeclaration:                           "property_declaration"
    of c_sharpPropertyPatternClause:                         "property_pattern_clause"
    of c_sharpQualifiedName:                                 "qualified_name"
    of c_sharpQueryContinuation:                             "query_continuation"
    of c_sharpQueryExpression:                               "query_expression"
    of c_sharpRangeExpression:                               "range_expression"
    of c_sharpRecordDeclaration:                             "record_declaration"
    of c_sharpRecursivePattern:                              "recursive_pattern"
    of c_sharpRefExpression:                                 "ref_expression"
    of c_sharpRefTypeExpression:                             "ref_type_expression"
    of c_sharpRefValueExpression:                            "ref_value_expression"
    of c_sharpRelationalPattern:                             "relational_pattern"
    of c_sharpReturnStatement:                               "return_statement"
    of c_sharpSelectClause:                                  "select_clause"
    of c_sharpSimpleAssignmentExpression:                    "simple_assignment_expression"
    of c_sharpSizeOfExpression:                              "size_of_expression"
    of c_sharpStackAllocArrayCreationExpression:             "stack_alloc_array_creation_expression"
    of c_sharpStringLiteral:                                 "string_literal"
    of c_sharpStructDeclaration:                             "struct_declaration"
    of c_sharpSubpattern:                                    "subpattern"
    of c_sharpSwitchBody:                                    "switch_body"
    of c_sharpSwitchExpression:                              "switch_expression"
    of c_sharpSwitchExpressionArm:                           "switch_expression_arm"
    of c_sharpSwitchSection:                                 "switch_section"
    of c_sharpSwitchStatement:                               "switch_statement"
    of c_sharpThisExpression:                                "this_expression"
    of c_sharpThrowExpression:                               "throw_expression"
    of c_sharpThrowStatement:                                "throw_statement"
    of c_sharpTryStatement:                                  "try_statement"
    of c_sharpTupleElement:                                  "tuple_element"
    of c_sharpTupleExpression:                               "tuple_expression"
    of c_sharpTuplePattern:                                  "tuple_pattern"
    of c_sharpTupleType:                                     "tuple_type"
    of c_sharpTypeArgumentList:                              "type_argument_list"
    of c_sharpTypeConstraint:                                "type_constraint"
    of c_sharpTypeOfExpression:                              "type_of_expression"
    of c_sharpTypeParameter:                                 "type_parameter"
    of c_sharpTypeParameterConstraint:                       "type_parameter_constraint"
    of c_sharpTypeParameterConstraintsClause:                "type_parameter_constraints_clause"
    of c_sharpTypeParameterList:                             "type_parameter_list"
    of c_sharpTypePattern:                                   "type_pattern"
    of c_sharpUnsafeStatement:                               "unsafe_statement"
    of c_sharpUsingDirective:                                "using_directive"
    of c_sharpUsingStatement:                                "using_statement"
    of c_sharpVarPattern:                                    "var_pattern"
    of c_sharpVariableDeclaration:                           "variable_declaration"
    of c_sharpVariableDeclarator:                            "variable_declarator"
    of c_sharpWhenClause:                                    "when_clause"
    of c_sharpWhereClause:                                   "where_clause"
    of c_sharpWhileStatement:                                "while_statement"
    of c_sharpWithExpression:                                "with_expression"
    of c_sharpWithInitializerExpression:                     "with_initializer_expression"
    of c_sharpYieldStatement:                                "yield_statement"
    of c_sharpExclamationTok:                                "!"
    of c_sharpExclamationEqualTok:                           "!="
    of c_sharpQuoteTok:                                      "\""
    of c_sharpDoubleQuoteTok:                                "\"\""
    of c_sharpDollarQuoteTok:                                "$\""
    of c_sharpDollarAtQuoteTok:                              "$@\""
    of c_sharpPercentTok:                                    "%"
    of c_sharpPercentEqualTok:                               "%="
    of c_sharpAmpersandTok:                                  "&"
    of c_sharpDoubleAmpersandTok:                            "&&"
    of c_sharpAmpersandEqualTok:                             "&="
    of c_sharpApostropheTok:                                 "\'"
    of c_sharpLParTok:                                       "("
    of c_sharpRParTok:                                       ")"
    of c_sharpAsteriskTok:                                   "*"
    of c_sharpAsteriskEqualTok:                              "*="
    of c_sharpPlusTok:                                       "+"
    of c_sharpDoublePlusTok:                                 "++"
    of c_sharpPlusEqualTok:                                  "+="
    of c_sharpCommaTok:                                      ","
    of c_sharpMinusTok:                                      "-"
    of c_sharpDoubleMinusTok:                                "--"
    of c_sharpMinusEqualTok:                                 "-="
    of c_sharpMinusGreaterThanTok:                           "->"
    of c_sharpDotTok:                                        "."
    of c_sharpDoubleDotTok:                                  ".."
    of c_sharpSlashTok:                                      "/"
    of c_sharpSlashEqualTok:                                 "/="
    of c_sharpColonTok:                                      ":"
    of c_sharpDoubleColonTok:                                "::"
    of c_sharpSemicolonTok:                                  ";"
    of c_sharpLessThanTok:                                   "<"
    of c_sharpDoubleLessThanTok:                             "<<"
    of c_sharpDoubleLessThanEqualTok:                        "<<="
    of c_sharpLessThanEqualTok:                              "<="
    of c_sharpEqualTok:                                      "="
    of c_sharpDoubleEqualTok:                                "=="
    of c_sharpEqualGreaterThanTok:                           "=>"
    of c_sharpGreaterThanTok:                                ">"
    of c_sharpGreaterThanEqualTok:                           ">="
    of c_sharpDoubleGreaterThanTok:                          ">>"
    of c_sharpDoubleGreaterThanEqualTok:                     ">>="
    of c_sharpQuestionTok:                                   "?"
    of c_sharpDoubleQuestionTok:                             "??"
    of c_sharpDoubleQuestionEqualTok:                        "??="
    of c_sharpAtDollarQuoteTok:                              "@$\""
    of c_sharpCdeclTok:                                      "Cdecl"
    of c_sharpFastcallTok:                                   "Fastcall"
    of c_sharpStdcallTok:                                    "Stdcall"
    of c_sharpThiscallTok:                                   "Thiscall"
    of c_sharpLBrackTok:                                     "["
    of c_sharpRBrackTok:                                     "]"
    of c_sharpAccentTok:                                     "^"
    of c_sharpAccentEqualTok:                                "^="
    of c_sharpUsUsMakerefTok:                                "__makeref"
    of c_sharpUsUsReftypeTok:                                "__reftype"
    of c_sharpUsUsRefvalueTok:                               "__refvalue"
    of c_sharpAbstractTok:                                   "abstract"
    of c_sharpAddTok:                                        "add"
    of c_sharpAliasTok:                                      "alias"
    of c_sharpAndTok:                                        "and"
    of c_sharpAsTok:                                         "as"
    of c_sharpAscendingTok:                                  "ascending"
    of c_sharpAssemblyTok:                                   "assembly"
    of c_sharpAsyncTok:                                      "async"
    of c_sharpAwaitTok:                                      "await"
    of c_sharpBaseTok:                                       "base"
    of c_sharpBreakTok:                                      "break"
    of c_sharpByTok:                                         "by"
    of c_sharpCaseTok:                                       "case"
    of c_sharpCatchTok:                                      "catch"
    of c_sharpCheckedTok:                                    "checked"
    of c_sharpClassTok:                                      "class"
    of c_sharpComment:                                       "comment"
    of c_sharpConstTok:                                      "const"
    of c_sharpContinueTok:                                   "continue"
    of c_sharpDefaultTok:                                    "default"
    of c_sharpDelegateTok:                                   "delegate"
    of c_sharpDescendingTok:                                 "descending"
    of c_sharpDiscard:                                       "discard"
    of c_sharpDoTok:                                         "do"
    of c_sharpDynamicTok:                                    "dynamic"
    of c_sharpElseTok:                                       "else"
    of c_sharpEnumTok:                                       "enum"
    of c_sharpEqualsTok:                                     "equals"
    of c_sharpEscapeSequence:                                "escape_sequence"
    of c_sharpEventTok:                                      "event"
    of c_sharpExplicitTok:                                   "explicit"
    of c_sharpExternTok:                                     "extern"
    of c_sharpFalseTok:                                      "false"
    of c_sharpFieldTok:                                      "field"
    of c_sharpFinallyTok:                                    "finally"
    of c_sharpFixedTok:                                      "fixed"
    of c_sharpForTok:                                        "for"
    of c_sharpForeachTok:                                    "foreach"
    of c_sharpFromTok:                                       "from"
    of c_sharpGetTok:                                        "get"
    of c_sharpGlobalTok:                                     "global"
    of c_sharpGotoTok:                                       "goto"
    of c_sharpGroupTok:                                      "group"
    of c_sharpIfTok:                                         "if"
    of c_sharpImplicitTok:                                   "implicit"
    of c_sharpInTok:                                         "in"
    of c_sharpInitTok:                                       "init"
    of c_sharpIntegerLiteral:                                "integer_literal"
    of c_sharpInterfaceTok:                                  "interface"
    of c_sharpInternalTok:                                   "internal"
    of c_sharpIntoTok:                                       "into"
    of c_sharpIsTok:                                         "is"
    of c_sharpJoinTok:                                       "join"
    of c_sharpLetTok:                                        "let"
    of c_sharpLockTok:                                       "lock"
    of c_sharpManagedTok:                                    "managed"
    of c_sharpMethodTok:                                     "method"
    of c_sharpModuleTok:                                     "module"
    of c_sharpNameofTok:                                     "nameof"
    of c_sharpNamespaceTok:                                  "namespace"
    of c_sharpNewTok:                                        "new"
    of c_sharpNotTok:                                        "not"
    of c_sharpNotnullTok:                                    "notnull"
    of c_sharpNullLiteral:                                   "null_literal"
    of c_sharpOnTok:                                         "on"
    of c_sharpOperatorTok:                                   "operator"
    of c_sharpOrTok:                                         "or"
    of c_sharpOrderbyTok:                                    "orderby"
    of c_sharpOutTok:                                        "out"
    of c_sharpOverrideTok:                                   "override"
    of c_sharpParamTok:                                      "param"
    of c_sharpParamsTok:                                     "params"
    of c_sharpPartialTok:                                    "partial"
    of c_sharpPredefinedType:                                "predefined_type"
    of c_sharpPreprocessorDirective:                         "preprocessor_directive"
    of c_sharpPrivateTok:                                    "private"
    of c_sharpPropertyTok:                                   "property"
    of c_sharpProtectedTok:                                  "protected"
    of c_sharpPublicTok:                                     "public"
    of c_sharpReadonlyTok:                                   "readonly"
    of c_sharpRealLiteral:                                   "real_literal"
    of c_sharpRecordTok:                                     "record"
    of c_sharpRefTok:                                        "ref"
    of c_sharpRemoveTok:                                     "remove"
    of c_sharpReturnTok:                                     "return"
    of c_sharpSealedTok:                                     "sealed"
    of c_sharpSelectTok:                                     "select"
    of c_sharpSetTok:                                        "set"
    of c_sharpSizeofTok:                                     "sizeof"
    of c_sharpStackallocTok:                                 "stackalloc"
    of c_sharpStaticTok:                                     "static"
    of c_sharpStructTok:                                     "struct"
    of c_sharpSwitchTok:                                     "switch"
    of c_sharpThisTok:                                       "this"
    of c_sharpThrowTok:                                      "throw"
    of c_sharpTrueTok:                                       "true"
    of c_sharpTryTok:                                        "try"
    of c_sharpTypeTok:                                       "type"
    of c_sharpTypeofTok:                                     "typeof"
    of c_sharpUncheckedTok:                                  "unchecked"
    of c_sharpUnmanagedTok:                                  "unmanaged"
    of c_sharpUnsafeTok:                                     "unsafe"
    of c_sharpUsingTok:                                      "using"
    of c_sharpVarTok:                                        "var"
    of c_sharpVerbatimStringLiteral:                         "verbatim_string_literal"
    of c_sharpVirtualTok:                                    "virtual"
    of c_sharpVoidKeyword:                                   "void_keyword"
    of c_sharpVolatileTok:                                   "volatile"
    of c_sharpWhenTok:                                       "when"
    of c_sharpWhereTok:                                      "where"
    of c_sharpWhileTok:                                      "while"
    of c_sharpWithTok:                                       "with"
    of c_sharpYieldTok:                                      "yield"
    of c_sharpLCurlyTok:                                     "{"
    of c_sharpDoubleLCurlyTok:                               "{{"
    of c_sharpPipeTok:                                       "|"
    of c_sharpPipeEqualTok:                                  "|="
    of c_sharpDoublePipeTok:                                 "||"
    of c_sharpRCurlyTok:                                     "}"
    of c_sharpTildeTok:                                      "~"
    of c_sharpSyntaxError:                                   "ERROR"


type
  C_sharpExternalTok* = enum
    c_sharpExtern_preproc_directive_end ## _preproc_directive_end


type
  TsC_sharpNode* = distinct TSNode


type
  C_sharpParser* = distinct PtsParser


const c_sharpAllowedSubnodes*: array[C_sharpNodeKind, set[C_sharpNodeKind]] = block:
                                                                                var tmp: array[C_sharpNodeKind, set[C_sharpNodeKind]]
                                                                                tmp[c_sharpAccessorDeclaration] = {c_sharpAttributeList, c_sharpIdentifier, c_sharpModifier}
                                                                                tmp[c_sharpAccessorList] = {c_sharpAccessorDeclaration}
                                                                                tmp[c_sharpAliasQualifiedName] = {c_sharpGenericName, c_sharpGlobal, c_sharpIdentifier}
                                                                                tmp[c_sharpAnonymousMethodExpression] = {c_sharpBlock, c_sharpParameterList}
                                                                                tmp[c_sharpAnonymousObjectCreationExpression] = {c_sharpUsExpression, c_sharpNameEquals}
                                                                                tmp[c_sharpArgument] = {c_sharpUsExpression, c_sharpDeclarationExpression, c_sharpNameColon}
                                                                                tmp[c_sharpArgumentList] = {c_sharpArgument}
                                                                                tmp[c_sharpArrayCreationExpression] = {c_sharpArrayType, c_sharpInitializerExpression}
                                                                                tmp[c_sharpArrayRankSpecifier] = {c_sharpUsExpression}
                                                                                tmp[c_sharpArrowExpressionClause] = {c_sharpUsExpression}
                                                                                tmp[c_sharpAssignmentExpression] = {c_sharpAssignmentOperator}
                                                                                tmp[c_sharpAttribute] = {c_sharpAttributeArgumentList}
                                                                                tmp[c_sharpAttributeArgument] = {c_sharpUsExpression, c_sharpNameColon, c_sharpNameEquals}
                                                                                tmp[c_sharpAttributeArgumentList] = {c_sharpAttributeArgument}
                                                                                tmp[c_sharpAttributeList] = {c_sharpAttribute, c_sharpAttributeTargetSpecifier}
                                                                                tmp[c_sharpAwaitExpression] = {c_sharpUsExpression}
                                                                                tmp[c_sharpBaseList] = {c_sharpUsType, c_sharpPrimaryConstructorBaseType}
                                                                                tmp[c_sharpBlock] = {c_sharpUsStatement}
                                                                                tmp[c_sharpBracketedArgumentList] = {c_sharpArgument}
                                                                                tmp[c_sharpBracketedParameterList] = {c_sharpParameter}
                                                                                tmp[c_sharpCasePatternSwitchLabel] = {
                                                                                                                       c_sharpBinaryPattern,
                                                                                                                       c_sharpConstantPattern,
                                                                                                                       c_sharpDeclarationPattern,
                                                                                                                       c_sharpDiscard,
                                                                                                                       c_sharpNegatedPattern,
                                                                                                                       c_sharpParenthesizedPattern,
                                                                                                                       c_sharpRecursivePattern,
                                                                                                                       c_sharpRelationalPattern,
                                                                                                                       c_sharpTypePattern,
                                                                                                                       c_sharpVarPattern,
                                                                                                                       c_sharpWhenClause
                                                                                                                     }
                                                                                tmp[c_sharpCaseSwitchLabel] = {c_sharpUsExpression}
                                                                                tmp[c_sharpCatchClause] = {c_sharpCatchDeclaration, c_sharpCatchFilterClause}
                                                                                tmp[c_sharpCatchFilterClause] = {c_sharpUsExpression}
                                                                                tmp[c_sharpCharacterLiteral] = {c_sharpEscapeSequence}
                                                                                tmp[c_sharpCheckedExpression] = {c_sharpUsExpression}
                                                                                tmp[c_sharpCheckedStatement] = {c_sharpBlock}
                                                                                tmp[c_sharpClassDeclaration] = {c_sharpAttributeList, c_sharpModifier, c_sharpTypeParameterConstraintsClause}
                                                                                tmp[c_sharpCompilationUnit] = {
                                                                                                                c_sharpClassDeclaration,
                                                                                                                c_sharpDelegateDeclaration,
                                                                                                                c_sharpEnumDeclaration,
                                                                                                                c_sharpExternAliasDirective,
                                                                                                                c_sharpGlobalAttributeList,
                                                                                                                c_sharpGlobalStatement,
                                                                                                                c_sharpInterfaceDeclaration,
                                                                                                                c_sharpNamespaceDeclaration,
                                                                                                                c_sharpRecordDeclaration,
                                                                                                                c_sharpStructDeclaration,
                                                                                                                c_sharpUsingDirective
                                                                                                              }
                                                                                tmp[c_sharpConditionalAccessExpression] = {c_sharpElementBindingExpression, c_sharpMemberBindingExpression}
                                                                                tmp[c_sharpConstantPattern] = {c_sharpUsExpression}
                                                                                tmp[c_sharpConstructorDeclaration] = {c_sharpAttributeList, c_sharpConstructorInitializer, c_sharpModifier}
                                                                                tmp[c_sharpConstructorInitializer] = {c_sharpArgumentList}
                                                                                tmp[c_sharpConversionOperatorDeclaration] = {c_sharpAttributeList, c_sharpModifier}
                                                                                tmp[c_sharpDeclarationList] = {c_sharpUsDeclaration}
                                                                                tmp[c_sharpDeclarationPattern] = {c_sharpDiscard, c_sharpIdentifier, c_sharpParenthesizedVariableDesignation}
                                                                                tmp[c_sharpDelegateDeclaration] = {c_sharpAttributeList, c_sharpModifier, c_sharpTypeParameterConstraintsClause}
                                                                                tmp[c_sharpDestructorDeclaration] = {c_sharpAttributeList, c_sharpIdentifier, c_sharpParameterList}
                                                                                tmp[c_sharpDoStatement] = {c_sharpUsExpression, c_sharpUsStatement}
                                                                                tmp[c_sharpElementBindingExpression] = {c_sharpBracketedArgumentList}
                                                                                tmp[c_sharpEnumDeclaration] = {c_sharpAttributeList, c_sharpModifier}
                                                                                tmp[c_sharpEnumMemberDeclaration] = {c_sharpAttributeList}
                                                                                tmp[c_sharpEnumMemberDeclarationList] = {c_sharpEnumMemberDeclaration}
                                                                                tmp[c_sharpEqualsValueClause] = {c_sharpUsExpression}
                                                                                tmp[c_sharpEventDeclaration] = {c_sharpAttributeList, c_sharpExplicitInterfaceSpecifier, c_sharpModifier}
                                                                                tmp[c_sharpEventFieldDeclaration] = {c_sharpAttributeList, c_sharpModifier, c_sharpVariableDeclaration}
                                                                                tmp[c_sharpExplicitInterfaceSpecifier] = {c_sharpAliasQualifiedName, c_sharpGenericName, c_sharpGlobal, c_sharpIdentifier, c_sharpQualifiedName}
                                                                                tmp[c_sharpExpressionStatement] = {c_sharpUsExpression}
                                                                                tmp[c_sharpExternAliasDirective] = {c_sharpIdentifier}
                                                                                tmp[c_sharpFieldDeclaration] = {c_sharpAttributeList, c_sharpModifier, c_sharpVariableDeclaration}
                                                                                tmp[c_sharpFinallyClause] = {c_sharpBlock}
                                                                                tmp[c_sharpFixedStatement] = {c_sharpUsStatement, c_sharpVariableDeclaration}
                                                                                tmp[c_sharpFromClause] = {c_sharpUsExpression, c_sharpUsType}
                                                                                tmp[c_sharpFunctionPointerCallingConvention] = {c_sharpFunctionPointerUnmanagedCallingConventionList}
                                                                                tmp[c_sharpFunctionPointerParameter] = {c_sharpUsType, c_sharpVoidKeyword}
                                                                                tmp[c_sharpFunctionPointerType] = {c_sharpFunctionPointerCallingConvention, c_sharpFunctionPointerParameter}
                                                                                tmp[c_sharpFunctionPointerUnmanagedCallingConvention] = {c_sharpIdentifier}
                                                                                tmp[c_sharpFunctionPointerUnmanagedCallingConventionList] = {c_sharpFunctionPointerUnmanagedCallingConvention}
                                                                                tmp[c_sharpGenericName] = {c_sharpIdentifier, c_sharpTypeArgumentList}
                                                                                tmp[c_sharpGlobalAttributeList] = {c_sharpAttribute}
                                                                                tmp[c_sharpGlobalStatement] = {c_sharpUsStatement}
                                                                                tmp[c_sharpGotoStatement] = {c_sharpUsExpression, c_sharpLabelName}
                                                                                tmp[c_sharpGroupClause] = {c_sharpUsExpression}
                                                                                tmp[c_sharpImplicitArrayCreationExpression] = {c_sharpInitializerExpression}
                                                                                tmp[c_sharpImplicitObjectCreationExpression] = {c_sharpArgumentList, c_sharpInitializerExpression}
                                                                                tmp[c_sharpImplicitStackAllocArrayCreationExpression] = {c_sharpInitializerExpression}
                                                                                tmp[c_sharpIndexerDeclaration] = {c_sharpAttributeList, c_sharpExplicitInterfaceSpecifier, c_sharpModifier}
                                                                                tmp[c_sharpInitializerExpression] = {c_sharpUsExpression}
                                                                                tmp[c_sharpInterfaceDeclaration] = {c_sharpAttributeList, c_sharpModifier, c_sharpTypeParameterConstraintsClause}
                                                                                tmp[c_sharpInterpolatedStringExpression] = {c_sharpInterpolatedStringText, c_sharpInterpolatedVerbatimStringText, c_sharpInterpolation}
                                                                                tmp[c_sharpInterpolatedStringText] = {c_sharpEscapeSequence}
                                                                                tmp[c_sharpInterpolation] = {c_sharpUsExpression, c_sharpInterpolationAlignmentClause, c_sharpInterpolationFormatClause}
                                                                                tmp[c_sharpInterpolationAlignmentClause] = {c_sharpUsExpression}
                                                                                tmp[c_sharpJoinClause] = {c_sharpUsExpression, c_sharpUsType, c_sharpJoinIntoClause}
                                                                                tmp[c_sharpJoinIntoClause] = {c_sharpIdentifier}
                                                                                tmp[c_sharpLabeledStatement] = {c_sharpUsStatement, c_sharpLabelName}
                                                                                tmp[c_sharpLambdaExpression] = {c_sharpIdentifier, c_sharpParameterList}
                                                                                tmp[c_sharpLetClause] = {c_sharpUsExpression}
                                                                                tmp[c_sharpLocalDeclarationStatement] = {c_sharpModifier, c_sharpVariableDeclaration}
                                                                                tmp[c_sharpLocalFunctionStatement] = {c_sharpAttributeList, c_sharpModifier, c_sharpTypeParameterConstraintsClause}
                                                                                tmp[c_sharpLockStatement] = {c_sharpUsExpression, c_sharpUsStatement}
                                                                                tmp[c_sharpMakeRefExpression] = {c_sharpUsExpression}
                                                                                tmp[c_sharpMethodDeclaration] = {c_sharpAttributeList, c_sharpExplicitInterfaceSpecifier, c_sharpModifier, c_sharpTypeParameterConstraintsClause}
                                                                                tmp[c_sharpNameColon] = {c_sharpGlobal, c_sharpIdentifier}
                                                                                tmp[c_sharpNameEquals] = {c_sharpGlobal, c_sharpIdentifier}
                                                                                tmp[c_sharpNegatedPattern] = {
                                                                                                               c_sharpBinaryPattern,
                                                                                                               c_sharpConstantPattern,
                                                                                                               c_sharpDeclarationPattern,
                                                                                                               c_sharpDiscard,
                                                                                                               c_sharpNegatedPattern,
                                                                                                               c_sharpParenthesizedPattern,
                                                                                                               c_sharpRecursivePattern,
                                                                                                               c_sharpRelationalPattern,
                                                                                                               c_sharpTypePattern,
                                                                                                               c_sharpVarPattern
                                                                                                             }
                                                                                tmp[c_sharpNullableType] = {c_sharpUsType}
                                                                                tmp[c_sharpOperatorDeclaration] = {c_sharpAttributeList, c_sharpModifier}
                                                                                tmp[c_sharpOrderByClause] = {c_sharpUsExpression}
                                                                                tmp[c_sharpParameter] = {c_sharpAttributeList, c_sharpEqualsValueClause, c_sharpParameterModifier}
                                                                                tmp[c_sharpParameterArray] = {c_sharpArrayType, c_sharpAttributeList, c_sharpIdentifier, c_sharpNullableType}
                                                                                tmp[c_sharpParameterList] = {c_sharpParameter, c_sharpParameterArray}
                                                                                tmp[c_sharpParenthesizedExpression] = {c_sharpUsExpression}
                                                                                tmp[c_sharpParenthesizedPattern] = {
                                                                                                                     c_sharpBinaryPattern,
                                                                                                                     c_sharpConstantPattern,
                                                                                                                     c_sharpDeclarationPattern,
                                                                                                                     c_sharpDiscard,
                                                                                                                     c_sharpNegatedPattern,
                                                                                                                     c_sharpParenthesizedPattern,
                                                                                                                     c_sharpRecursivePattern,
                                                                                                                     c_sharpRelationalPattern,
                                                                                                                     c_sharpTypePattern,
                                                                                                                     c_sharpVarPattern
                                                                                                                   }
                                                                                tmp[c_sharpParenthesizedVariableDesignation] = {c_sharpDiscard, c_sharpIdentifier, c_sharpParenthesizedVariableDesignation}
                                                                                tmp[c_sharpPointerType] = {c_sharpUsType}
                                                                                tmp[c_sharpPositionalPatternClause] = {c_sharpSubpattern}
                                                                                tmp[c_sharpPostfixUnaryExpression] = {c_sharpUsExpression}
                                                                                tmp[c_sharpPrefixUnaryExpression] = {c_sharpUsExpression}
                                                                                tmp[c_sharpPreprocessorCall] = {
                                                                                                                 c_sharpBooleanLiteral,
                                                                                                                 c_sharpCharacterLiteral,
                                                                                                                 c_sharpIdentifier,
                                                                                                                 c_sharpIntegerLiteral,
                                                                                                                 c_sharpNullLiteral,
                                                                                                                 c_sharpPreprocessorDirective,
                                                                                                                 c_sharpRealLiteral,
                                                                                                                 c_sharpStringLiteral,
                                                                                                                 c_sharpVerbatimStringLiteral
                                                                                                               }
                                                                                tmp[c_sharpPrimaryConstructorBaseType] = {c_sharpArgumentList, c_sharpIdentifier}
                                                                                tmp[c_sharpPropertyDeclaration] = {c_sharpAttributeList, c_sharpExplicitInterfaceSpecifier, c_sharpModifier}
                                                                                tmp[c_sharpPropertyPatternClause] = {c_sharpSubpattern}
                                                                                tmp[c_sharpQualifiedName] = {c_sharpAliasQualifiedName, c_sharpGenericName, c_sharpGlobal, c_sharpIdentifier, c_sharpQualifiedName}
                                                                                tmp[c_sharpQueryContinuation] = {
                                                                                                                  c_sharpFromClause,
                                                                                                                  c_sharpGroupClause,
                                                                                                                  c_sharpIdentifier,
                                                                                                                  c_sharpJoinClause,
                                                                                                                  c_sharpLetClause,
                                                                                                                  c_sharpOrderByClause,
                                                                                                                  c_sharpQueryContinuation,
                                                                                                                  c_sharpSelectClause,
                                                                                                                  c_sharpWhereClause
                                                                                                                }
                                                                                tmp[c_sharpQueryExpression] = {
                                                                                                                c_sharpFromClause,
                                                                                                                c_sharpGroupClause,
                                                                                                                c_sharpJoinClause,
                                                                                                                c_sharpLetClause,
                                                                                                                c_sharpOrderByClause,
                                                                                                                c_sharpQueryContinuation,
                                                                                                                c_sharpSelectClause,
                                                                                                                c_sharpWhereClause
                                                                                                              }
                                                                                tmp[c_sharpRangeExpression] = {c_sharpUsExpression}
                                                                                tmp[c_sharpRecordDeclaration] = {c_sharpAttributeList, c_sharpModifier, c_sharpTypeParameterConstraintsClause}
                                                                                tmp[c_sharpRecursivePattern] = {c_sharpUsType, c_sharpDiscard, c_sharpParenthesizedVariableDesignation, c_sharpPositionalPatternClause, c_sharpPropertyPatternClause}
                                                                                tmp[c_sharpRefExpression] = {c_sharpUsExpression}
                                                                                tmp[c_sharpRefTypeExpression] = {c_sharpUsExpression}
                                                                                tmp[c_sharpRelationalPattern] = {c_sharpUsExpression}
                                                                                tmp[c_sharpReturnStatement] = {c_sharpUsExpression}
                                                                                tmp[c_sharpSelectClause] = {c_sharpUsExpression}
                                                                                tmp[c_sharpSimpleAssignmentExpression] = {c_sharpUsExpression}
                                                                                tmp[c_sharpSizeOfExpression] = {c_sharpUsType}
                                                                                tmp[c_sharpStackAllocArrayCreationExpression] = {c_sharpArrayType, c_sharpInitializerExpression}
                                                                                tmp[c_sharpStringLiteral] = {c_sharpEscapeSequence}
                                                                                tmp[c_sharpStructDeclaration] = {c_sharpAttributeList, c_sharpModifier, c_sharpTypeParameterConstraintsClause}
                                                                                tmp[c_sharpSubpattern] = {
                                                                                                           c_sharpBinaryPattern,
                                                                                                           c_sharpConstantPattern,
                                                                                                           c_sharpDeclarationPattern,
                                                                                                           c_sharpDiscard,
                                                                                                           c_sharpNameColon,
                                                                                                           c_sharpNegatedPattern,
                                                                                                           c_sharpParenthesizedPattern,
                                                                                                           c_sharpRecursivePattern,
                                                                                                           c_sharpRelationalPattern,
                                                                                                           c_sharpTypePattern,
                                                                                                           c_sharpVarPattern
                                                                                                         }
                                                                                tmp[c_sharpSwitchBody] = {c_sharpSwitchSection}
                                                                                tmp[c_sharpSwitchExpression] = {c_sharpUsExpression, c_sharpSwitchExpressionArm}
                                                                                tmp[c_sharpSwitchExpressionArm] = {
                                                                                                                    c_sharpUsExpression,
                                                                                                                    c_sharpBinaryPattern,
                                                                                                                    c_sharpConstantPattern,
                                                                                                                    c_sharpDeclarationPattern,
                                                                                                                    c_sharpDiscard,
                                                                                                                    c_sharpNegatedPattern,
                                                                                                                    c_sharpParenthesizedPattern,
                                                                                                                    c_sharpRecursivePattern,
                                                                                                                    c_sharpRelationalPattern,
                                                                                                                    c_sharpTypePattern,
                                                                                                                    c_sharpVarPattern,
                                                                                                                    c_sharpWhenClause
                                                                                                                  }
                                                                                tmp[c_sharpSwitchSection] = {c_sharpUsStatement, c_sharpCasePatternSwitchLabel, c_sharpCaseSwitchLabel, c_sharpDefaultSwitchLabel}
                                                                                tmp[c_sharpThrowExpression] = {c_sharpUsExpression}
                                                                                tmp[c_sharpThrowStatement] = {c_sharpUsExpression}
                                                                                tmp[c_sharpTryStatement] = {c_sharpCatchClause, c_sharpFinallyClause}
                                                                                tmp[c_sharpTupleExpression] = {c_sharpArgument}
                                                                                tmp[c_sharpTuplePattern] = {c_sharpDiscard, c_sharpIdentifier, c_sharpTuplePattern}
                                                                                tmp[c_sharpTupleType] = {c_sharpTupleElement}
                                                                                tmp[c_sharpTypeArgumentList] = {c_sharpUsType}
                                                                                tmp[c_sharpTypeOfExpression] = {c_sharpUsType}
                                                                                tmp[c_sharpTypeParameter] = {c_sharpAttributeList, c_sharpIdentifier}
                                                                                tmp[c_sharpTypeParameterConstraint] = {c_sharpConstructorConstraint, c_sharpTypeConstraint}
                                                                                tmp[c_sharpTypeParameterList] = {c_sharpTypeParameter}
                                                                                tmp[c_sharpTypePattern] = {c_sharpUsType}
                                                                                tmp[c_sharpUnsafeStatement] = {c_sharpBlock}
                                                                                tmp[c_sharpUsingDirective] = {c_sharpAliasQualifiedName, c_sharpGenericName, c_sharpGlobal, c_sharpIdentifier, c_sharpNameEquals, c_sharpQualifiedName}
                                                                                tmp[c_sharpUsingStatement] = {c_sharpUsExpression, c_sharpVariableDeclaration}
                                                                                tmp[c_sharpVarPattern] = {c_sharpDiscard, c_sharpIdentifier, c_sharpParenthesizedVariableDesignation}
                                                                                tmp[c_sharpVariableDeclaration] = {c_sharpVariableDeclarator}
                                                                                tmp[c_sharpVariableDeclarator] = {c_sharpBracketedArgumentList, c_sharpEqualsValueClause, c_sharpIdentifier, c_sharpTuplePattern}
                                                                                tmp[c_sharpWhenClause] = {c_sharpUsExpression}
                                                                                tmp[c_sharpWhereClause] = {c_sharpUsExpression}
                                                                                tmp[c_sharpWhileStatement] = {c_sharpUsExpression, c_sharpUsStatement}
                                                                                tmp[c_sharpWithExpression] = {c_sharpUsExpression, c_sharpWithInitializerExpression}
                                                                                tmp[c_sharpWithInitializerExpression] = {c_sharpSimpleAssignmentExpression}
                                                                                tmp[c_sharpYieldStatement] = {c_sharpUsExpression}
                                                                                tmp
const c_sharpTokenKinds*: set[C_sharpNodeKind] = {
                                                   c_sharpExclamationTok,
                                                   c_sharpExclamationEqualTok,
                                                   c_sharpQuoteTok,
                                                   c_sharpDoubleQuoteTok,
                                                   c_sharpDollarQuoteTok,
                                                   c_sharpDollarAtQuoteTok,
                                                   c_sharpPercentTok,
                                                   c_sharpPercentEqualTok,
                                                   c_sharpAmpersandTok,
                                                   c_sharpDoubleAmpersandTok,
                                                   c_sharpAmpersandEqualTok,
                                                   c_sharpApostropheTok,
                                                   c_sharpLParTok,
                                                   c_sharpRParTok,
                                                   c_sharpAsteriskTok,
                                                   c_sharpAsteriskEqualTok,
                                                   c_sharpPlusTok,
                                                   c_sharpDoublePlusTok,
                                                   c_sharpPlusEqualTok,
                                                   c_sharpCommaTok,
                                                   c_sharpMinusTok,
                                                   c_sharpDoubleMinusTok,
                                                   c_sharpMinusEqualTok,
                                                   c_sharpMinusGreaterThanTok,
                                                   c_sharpDotTok,
                                                   c_sharpDoubleDotTok,
                                                   c_sharpSlashTok,
                                                   c_sharpSlashEqualTok,
                                                   c_sharpColonTok,
                                                   c_sharpDoubleColonTok,
                                                   c_sharpSemicolonTok,
                                                   c_sharpLessThanTok,
                                                   c_sharpDoubleLessThanTok,
                                                   c_sharpDoubleLessThanEqualTok,
                                                   c_sharpLessThanEqualTok,
                                                   c_sharpEqualTok,
                                                   c_sharpDoubleEqualTok,
                                                   c_sharpEqualGreaterThanTok,
                                                   c_sharpGreaterThanTok,
                                                   c_sharpGreaterThanEqualTok,
                                                   c_sharpDoubleGreaterThanTok,
                                                   c_sharpDoubleGreaterThanEqualTok,
                                                   c_sharpQuestionTok,
                                                   c_sharpDoubleQuestionTok,
                                                   c_sharpDoubleQuestionEqualTok,
                                                   c_sharpAtDollarQuoteTok,
                                                   c_sharpCdeclTok,
                                                   c_sharpFastcallTok,
                                                   c_sharpStdcallTok,
                                                   c_sharpThiscallTok,
                                                   c_sharpLBrackTok,
                                                   c_sharpRBrackTok,
                                                   c_sharpAccentTok,
                                                   c_sharpAccentEqualTok,
                                                   c_sharpUsUsMakerefTok,
                                                   c_sharpUsUsReftypeTok,
                                                   c_sharpUsUsRefvalueTok,
                                                   c_sharpAbstractTok,
                                                   c_sharpAddTok,
                                                   c_sharpAliasTok,
                                                   c_sharpAndTok,
                                                   c_sharpAsTok,
                                                   c_sharpAscendingTok,
                                                   c_sharpAssemblyTok,
                                                   c_sharpAsyncTok,
                                                   c_sharpAwaitTok,
                                                   c_sharpBaseTok,
                                                   c_sharpBreakTok,
                                                   c_sharpByTok,
                                                   c_sharpCaseTok,
                                                   c_sharpCatchTok,
                                                   c_sharpCheckedTok,
                                                   c_sharpClassTok,
                                                   c_sharpConstTok,
                                                   c_sharpContinueTok,
                                                   c_sharpDefaultTok,
                                                   c_sharpDelegateTok,
                                                   c_sharpDescendingTok,
                                                   c_sharpDoTok,
                                                   c_sharpDynamicTok,
                                                   c_sharpElseTok,
                                                   c_sharpEnumTok,
                                                   c_sharpEqualsTok,
                                                   c_sharpEventTok,
                                                   c_sharpExplicitTok,
                                                   c_sharpExternTok,
                                                   c_sharpFalseTok,
                                                   c_sharpFieldTok,
                                                   c_sharpFinallyTok,
                                                   c_sharpFixedTok,
                                                   c_sharpForTok,
                                                   c_sharpForeachTok,
                                                   c_sharpFromTok,
                                                   c_sharpGetTok,
                                                   c_sharpGlobalTok,
                                                   c_sharpGotoTok,
                                                   c_sharpGroupTok,
                                                   c_sharpIfTok,
                                                   c_sharpImplicitTok,
                                                   c_sharpInTok,
                                                   c_sharpInitTok,
                                                   c_sharpInterfaceTok,
                                                   c_sharpInternalTok,
                                                   c_sharpIntoTok,
                                                   c_sharpIsTok,
                                                   c_sharpJoinTok,
                                                   c_sharpLetTok,
                                                   c_sharpLockTok,
                                                   c_sharpManagedTok,
                                                   c_sharpMethodTok,
                                                   c_sharpModuleTok,
                                                   c_sharpNameofTok,
                                                   c_sharpNamespaceTok,
                                                   c_sharpNewTok,
                                                   c_sharpNotTok,
                                                   c_sharpNotnullTok,
                                                   c_sharpOnTok,
                                                   c_sharpOperatorTok,
                                                   c_sharpOrTok,
                                                   c_sharpOrderbyTok,
                                                   c_sharpOutTok,
                                                   c_sharpOverrideTok,
                                                   c_sharpParamTok,
                                                   c_sharpParamsTok,
                                                   c_sharpPartialTok,
                                                   c_sharpPrivateTok,
                                                   c_sharpPropertyTok,
                                                   c_sharpProtectedTok,
                                                   c_sharpPublicTok,
                                                   c_sharpReadonlyTok,
                                                   c_sharpRecordTok,
                                                   c_sharpRefTok,
                                                   c_sharpRemoveTok,
                                                   c_sharpReturnTok,
                                                   c_sharpSealedTok,
                                                   c_sharpSelectTok,
                                                   c_sharpSetTok,
                                                   c_sharpSizeofTok,
                                                   c_sharpStackallocTok,
                                                   c_sharpStaticTok,
                                                   c_sharpStructTok,
                                                   c_sharpSwitchTok,
                                                   c_sharpThisTok,
                                                   c_sharpThrowTok,
                                                   c_sharpTrueTok,
                                                   c_sharpTryTok,
                                                   c_sharpTypeTok,
                                                   c_sharpTypeofTok,
                                                   c_sharpUncheckedTok,
                                                   c_sharpUnmanagedTok,
                                                   c_sharpUnsafeTok,
                                                   c_sharpUsingTok,
                                                   c_sharpVarTok,
                                                   c_sharpVirtualTok,
                                                   c_sharpVolatileTok,
                                                   c_sharpWhenTok,
                                                   c_sharpWhereTok,
                                                   c_sharpWhileTok,
                                                   c_sharpWithTok,
                                                   c_sharpYieldTok,
                                                   c_sharpLCurlyTok,
                                                   c_sharpDoubleLCurlyTok,
                                                   c_sharpPipeTok,
                                                   c_sharpPipeEqualTok,
                                                   c_sharpDoublePipeTok,
                                                   c_sharpRCurlyTok,
                                                   c_sharpTildeTok
                                                 }

proc tsNodeType*(node: TsC_sharpNode): string



proc kind*(node: TsC_sharpNode): C_sharpNodeKind {.noSideEffect.} =
  {.cast(noSideEffect).}:
    case node.tsNodeType:
      of "_declaration":                                       c_sharpUsDeclaration
      of "_expression":                                        c_sharpUsExpression
      of "_statement":                                         c_sharpUsStatement
      of "_type":                                              c_sharpUsType
      of "accessor_declaration":                               c_sharpAccessorDeclaration
      of "accessor_list":                                      c_sharpAccessorList
      of "alias_qualified_name":                               c_sharpAliasQualifiedName
      of "anonymous_method_expression":                        c_sharpAnonymousMethodExpression
      of "anonymous_object_creation_expression":               c_sharpAnonymousObjectCreationExpression
      of "argument":                                           c_sharpArgument
      of "argument_list":                                      c_sharpArgumentList
      of "array_creation_expression":                          c_sharpArrayCreationExpression
      of "array_rank_specifier":                               c_sharpArrayRankSpecifier
      of "array_type":                                         c_sharpArrayType
      of "arrow_expression_clause":                            c_sharpArrowExpressionClause
      of "as_expression":                                      c_sharpAsExpression
      of "assignment_expression":                              c_sharpAssignmentExpression
      of "assignment_operator":                                c_sharpAssignmentOperator
      of "attribute":                                          c_sharpAttribute
      of "attribute_argument":                                 c_sharpAttributeArgument
      of "attribute_argument_list":                            c_sharpAttributeArgumentList
      of "attribute_list":                                     c_sharpAttributeList
      of "attribute_target_specifier":                         c_sharpAttributeTargetSpecifier
      of "await_expression":                                   c_sharpAwaitExpression
      of "base_expression":                                    c_sharpBaseExpression
      of "base_list":                                          c_sharpBaseList
      of "binary_expression":                                  c_sharpBinaryExpression
      of "binary_pattern":                                     c_sharpBinaryPattern
      of "block":                                              c_sharpBlock
      of "boolean_literal":                                    c_sharpBooleanLiteral
      of "bracketed_argument_list":                            c_sharpBracketedArgumentList
      of "bracketed_parameter_list":                           c_sharpBracketedParameterList
      of "break_statement":                                    c_sharpBreakStatement
      of "case_pattern_switch_label":                          c_sharpCasePatternSwitchLabel
      of "case_switch_label":                                  c_sharpCaseSwitchLabel
      of "cast_expression":                                    c_sharpCastExpression
      of "catch_clause":                                       c_sharpCatchClause
      of "catch_declaration":                                  c_sharpCatchDeclaration
      of "catch_filter_clause":                                c_sharpCatchFilterClause
      of "character_literal":                                  c_sharpCharacterLiteral
      of "checked_expression":                                 c_sharpCheckedExpression
      of "checked_statement":                                  c_sharpCheckedStatement
      of "class_declaration":                                  c_sharpClassDeclaration
      of "compilation_unit":                                   c_sharpCompilationUnit
      of "conditional_access_expression":                      c_sharpConditionalAccessExpression
      of "conditional_expression":                             c_sharpConditionalExpression
      of "constant_pattern":                                   c_sharpConstantPattern
      of "constructor_constraint":                             c_sharpConstructorConstraint
      of "constructor_declaration":                            c_sharpConstructorDeclaration
      of "constructor_initializer":                            c_sharpConstructorInitializer
      of "continue_statement":                                 c_sharpContinueStatement
      of "conversion_operator_declaration":                    c_sharpConversionOperatorDeclaration
      of "declaration_expression":                             c_sharpDeclarationExpression
      of "declaration_list":                                   c_sharpDeclarationList
      of "declaration_pattern":                                c_sharpDeclarationPattern
      of "default_expression":                                 c_sharpDefaultExpression
      of "default_switch_label":                               c_sharpDefaultSwitchLabel
      of "delegate_declaration":                               c_sharpDelegateDeclaration
      of "destructor_declaration":                             c_sharpDestructorDeclaration
      of "do_statement":                                       c_sharpDoStatement
      of "element_access_expression":                          c_sharpElementAccessExpression
      of "element_binding_expression":                         c_sharpElementBindingExpression
      of "empty_statement":                                    c_sharpEmptyStatement
      of "enum_declaration":                                   c_sharpEnumDeclaration
      of "enum_member_declaration":                            c_sharpEnumMemberDeclaration
      of "enum_member_declaration_list":                       c_sharpEnumMemberDeclarationList
      of "equals_value_clause":                                c_sharpEqualsValueClause
      of "event_declaration":                                  c_sharpEventDeclaration
      of "event_field_declaration":                            c_sharpEventFieldDeclaration
      of "explicit_interface_specifier":                       c_sharpExplicitInterfaceSpecifier
      of "expression_statement":                               c_sharpExpressionStatement
      of "extern_alias_directive":                             c_sharpExternAliasDirective
      of "field_declaration":                                  c_sharpFieldDeclaration
      of "finally_clause":                                     c_sharpFinallyClause
      of "fixed_statement":                                    c_sharpFixedStatement
      of "for_each_statement":                                 c_sharpForEachStatement
      of "for_statement":                                      c_sharpForStatement
      of "from_clause":                                        c_sharpFromClause
      of "function_pointer_calling_convention":                c_sharpFunctionPointerCallingConvention
      of "function_pointer_parameter":                         c_sharpFunctionPointerParameter
      of "function_pointer_type":                              c_sharpFunctionPointerType
      of "function_pointer_unmanaged_calling_convention":      c_sharpFunctionPointerUnmanagedCallingConvention
      of "function_pointer_unmanaged_calling_convention_list": c_sharpFunctionPointerUnmanagedCallingConventionList
      of "generic_name":                                       c_sharpGenericName
      of "global":                                             c_sharpGlobal
      of "global_attribute_list":                              c_sharpGlobalAttributeList
      of "global_statement":                                   c_sharpGlobalStatement
      of "goto_statement":                                     c_sharpGotoStatement
      of "group_clause":                                       c_sharpGroupClause
      of "identifier":                                         c_sharpIdentifier
      of "if_statement":                                       c_sharpIfStatement
      of "implicit_array_creation_expression":                 c_sharpImplicitArrayCreationExpression
      of "implicit_object_creation_expression":                c_sharpImplicitObjectCreationExpression
      of "implicit_stack_alloc_array_creation_expression":     c_sharpImplicitStackAllocArrayCreationExpression
      of "implicit_type":                                      c_sharpImplicitType
      of "indexer_declaration":                                c_sharpIndexerDeclaration
      of "initializer_expression":                             c_sharpInitializerExpression
      of "interface_declaration":                              c_sharpInterfaceDeclaration
      of "interpolated_string_expression":                     c_sharpInterpolatedStringExpression
      of "interpolated_string_text":                           c_sharpInterpolatedStringText
      of "interpolated_verbatim_string_text":                  c_sharpInterpolatedVerbatimStringText
      of "interpolation":                                      c_sharpInterpolation
      of "interpolation_alignment_clause":                     c_sharpInterpolationAlignmentClause
      of "interpolation_format_clause":                        c_sharpInterpolationFormatClause
      of "invocation_expression":                              c_sharpInvocationExpression
      of "is_expression":                                      c_sharpIsExpression
      of "is_pattern_expression":                              c_sharpIsPatternExpression
      of "join_clause":                                        c_sharpJoinClause
      of "join_into_clause":                                   c_sharpJoinIntoClause
      of "label_name":                                         c_sharpLabelName
      of "labeled_statement":                                  c_sharpLabeledStatement
      of "lambda_expression":                                  c_sharpLambdaExpression
      of "let_clause":                                         c_sharpLetClause
      of "local_declaration_statement":                        c_sharpLocalDeclarationStatement
      of "local_function_statement":                           c_sharpLocalFunctionStatement
      of "lock_statement":                                     c_sharpLockStatement
      of "make_ref_expression":                                c_sharpMakeRefExpression
      of "member_access_expression":                           c_sharpMemberAccessExpression
      of "member_binding_expression":                          c_sharpMemberBindingExpression
      of "method_declaration":                                 c_sharpMethodDeclaration
      of "modifier":                                           c_sharpModifier
      of "name_colon":                                         c_sharpNameColon
      of "name_equals":                                        c_sharpNameEquals
      of "namespace_declaration":                              c_sharpNamespaceDeclaration
      of "negated_pattern":                                    c_sharpNegatedPattern
      of "nullable_type":                                      c_sharpNullableType
      of "object_creation_expression":                         c_sharpObjectCreationExpression
      of "operator_declaration":                               c_sharpOperatorDeclaration
      of "order_by_clause":                                    c_sharpOrderByClause
      of "parameter":                                          c_sharpParameter
      of "parameter_array":                                    c_sharpParameterArray
      of "parameter_list":                                     c_sharpParameterList
      of "parameter_modifier":                                 c_sharpParameterModifier
      of "parenthesized_expression":                           c_sharpParenthesizedExpression
      of "parenthesized_pattern":                              c_sharpParenthesizedPattern
      of "parenthesized_variable_designation":                 c_sharpParenthesizedVariableDesignation
      of "pointer_type":                                       c_sharpPointerType
      of "positional_pattern_clause":                          c_sharpPositionalPatternClause
      of "postfix_unary_expression":                           c_sharpPostfixUnaryExpression
      of "prefix_unary_expression":                            c_sharpPrefixUnaryExpression
      of "preprocessor_call":                                  c_sharpPreprocessorCall
      of "primary_constructor_base_type":                      c_sharpPrimaryConstructorBaseType
      of "property_declaration":                               c_sharpPropertyDeclaration
      of "property_pattern_clause":                            c_sharpPropertyPatternClause
      of "qualified_name":                                     c_sharpQualifiedName
      of "query_continuation":                                 c_sharpQueryContinuation
      of "query_expression":                                   c_sharpQueryExpression
      of "range_expression":                                   c_sharpRangeExpression
      of "record_declaration":                                 c_sharpRecordDeclaration
      of "recursive_pattern":                                  c_sharpRecursivePattern
      of "ref_expression":                                     c_sharpRefExpression
      of "ref_type_expression":                                c_sharpRefTypeExpression
      of "ref_value_expression":                               c_sharpRefValueExpression
      of "relational_pattern":                                 c_sharpRelationalPattern
      of "return_statement":                                   c_sharpReturnStatement
      of "select_clause":                                      c_sharpSelectClause
      of "simple_assignment_expression":                       c_sharpSimpleAssignmentExpression
      of "size_of_expression":                                 c_sharpSizeOfExpression
      of "stack_alloc_array_creation_expression":              c_sharpStackAllocArrayCreationExpression
      of "string_literal":                                     c_sharpStringLiteral
      of "struct_declaration":                                 c_sharpStructDeclaration
      of "subpattern":                                         c_sharpSubpattern
      of "switch_body":                                        c_sharpSwitchBody
      of "switch_expression":                                  c_sharpSwitchExpression
      of "switch_expression_arm":                              c_sharpSwitchExpressionArm
      of "switch_section":                                     c_sharpSwitchSection
      of "switch_statement":                                   c_sharpSwitchStatement
      of "this_expression":                                    c_sharpThisExpression
      of "throw_expression":                                   c_sharpThrowExpression
      of "throw_statement":                                    c_sharpThrowStatement
      of "try_statement":                                      c_sharpTryStatement
      of "tuple_element":                                      c_sharpTupleElement
      of "tuple_expression":                                   c_sharpTupleExpression
      of "tuple_pattern":                                      c_sharpTuplePattern
      of "tuple_type":                                         c_sharpTupleType
      of "type_argument_list":                                 c_sharpTypeArgumentList
      of "type_constraint":                                    c_sharpTypeConstraint
      of "type_of_expression":                                 c_sharpTypeOfExpression
      of "type_parameter":                                     c_sharpTypeParameter
      of "type_parameter_constraint":                          c_sharpTypeParameterConstraint
      of "type_parameter_constraints_clause":                  c_sharpTypeParameterConstraintsClause
      of "type_parameter_list":                                c_sharpTypeParameterList
      of "type_pattern":                                       c_sharpTypePattern
      of "unsafe_statement":                                   c_sharpUnsafeStatement
      of "using_directive":                                    c_sharpUsingDirective
      of "using_statement":                                    c_sharpUsingStatement
      of "var_pattern":                                        c_sharpVarPattern
      of "variable_declaration":                               c_sharpVariableDeclaration
      of "variable_declarator":                                c_sharpVariableDeclarator
      of "when_clause":                                        c_sharpWhenClause
      of "where_clause":                                       c_sharpWhereClause
      of "while_statement":                                    c_sharpWhileStatement
      of "with_expression":                                    c_sharpWithExpression
      of "with_initializer_expression":                        c_sharpWithInitializerExpression
      of "yield_statement":                                    c_sharpYieldStatement
      of "!":                                                  c_sharpExclamationTok
      of "!=":                                                 c_sharpExclamationEqualTok
      of "\"":                                                 c_sharpQuoteTok
      of "\"\"":                                               c_sharpDoubleQuoteTok
      of "$\"":                                                c_sharpDollarQuoteTok
      of "$@\"":                                               c_sharpDollarAtQuoteTok
      of "%":                                                  c_sharpPercentTok
      of "%=":                                                 c_sharpPercentEqualTok
      of "&":                                                  c_sharpAmpersandTok
      of "&&":                                                 c_sharpDoubleAmpersandTok
      of "&=":                                                 c_sharpAmpersandEqualTok
      of "\'":                                                 c_sharpApostropheTok
      of "(":                                                  c_sharpLParTok
      of ")":                                                  c_sharpRParTok
      of "*":                                                  c_sharpAsteriskTok
      of "*=":                                                 c_sharpAsteriskEqualTok
      of "+":                                                  c_sharpPlusTok
      of "++":                                                 c_sharpDoublePlusTok
      of "+=":                                                 c_sharpPlusEqualTok
      of ",":                                                  c_sharpCommaTok
      of "-":                                                  c_sharpMinusTok
      of "--":                                                 c_sharpDoubleMinusTok
      of "-=":                                                 c_sharpMinusEqualTok
      of "->":                                                 c_sharpMinusGreaterThanTok
      of ".":                                                  c_sharpDotTok
      of "..":                                                 c_sharpDoubleDotTok
      of "/":                                                  c_sharpSlashTok
      of "/=":                                                 c_sharpSlashEqualTok
      of ":":                                                  c_sharpColonTok
      of "::":                                                 c_sharpDoubleColonTok
      of ";":                                                  c_sharpSemicolonTok
      of "<":                                                  c_sharpLessThanTok
      of "<<":                                                 c_sharpDoubleLessThanTok
      of "<<=":                                                c_sharpDoubleLessThanEqualTok
      of "<=":                                                 c_sharpLessThanEqualTok
      of "=":                                                  c_sharpEqualTok
      of "==":                                                 c_sharpDoubleEqualTok
      of "=>":                                                 c_sharpEqualGreaterThanTok
      of ">":                                                  c_sharpGreaterThanTok
      of ">=":                                                 c_sharpGreaterThanEqualTok
      of ">>":                                                 c_sharpDoubleGreaterThanTok
      of ">>=":                                                c_sharpDoubleGreaterThanEqualTok
      of "?":                                                  c_sharpQuestionTok
      of "??":                                                 c_sharpDoubleQuestionTok
      of "??=":                                                c_sharpDoubleQuestionEqualTok
      of "@$\"":                                               c_sharpAtDollarQuoteTok
      of "Cdecl":                                              c_sharpCdeclTok
      of "Fastcall":                                           c_sharpFastcallTok
      of "Stdcall":                                            c_sharpStdcallTok
      of "Thiscall":                                           c_sharpThiscallTok
      of "[":                                                  c_sharpLBrackTok
      of "]":                                                  c_sharpRBrackTok
      of "^":                                                  c_sharpAccentTok
      of "^=":                                                 c_sharpAccentEqualTok
      of "__makeref":                                          c_sharpUsUsMakerefTok
      of "__reftype":                                          c_sharpUsUsReftypeTok
      of "__refvalue":                                         c_sharpUsUsRefvalueTok
      of "abstract":                                           c_sharpAbstractTok
      of "add":                                                c_sharpAddTok
      of "alias":                                              c_sharpAliasTok
      of "and":                                                c_sharpAndTok
      of "as":                                                 c_sharpAsTok
      of "ascending":                                          c_sharpAscendingTok
      of "assembly":                                           c_sharpAssemblyTok
      of "async":                                              c_sharpAsyncTok
      of "await":                                              c_sharpAwaitTok
      of "base":                                               c_sharpBaseTok
      of "break":                                              c_sharpBreakTok
      of "by":                                                 c_sharpByTok
      of "case":                                               c_sharpCaseTok
      of "catch":                                              c_sharpCatchTok
      of "checked":                                            c_sharpCheckedTok
      of "class":                                              c_sharpClassTok
      of "comment":                                            c_sharpComment
      of "const":                                              c_sharpConstTok
      of "continue":                                           c_sharpContinueTok
      of "default":                                            c_sharpDefaultTok
      of "delegate":                                           c_sharpDelegateTok
      of "descending":                                         c_sharpDescendingTok
      of "discard":                                            c_sharpDiscard
      of "do":                                                 c_sharpDoTok
      of "dynamic":                                            c_sharpDynamicTok
      of "else":                                               c_sharpElseTok
      of "enum":                                               c_sharpEnumTok
      of "equals":                                             c_sharpEqualsTok
      of "escape_sequence":                                    c_sharpEscapeSequence
      of "event":                                              c_sharpEventTok
      of "explicit":                                           c_sharpExplicitTok
      of "extern":                                             c_sharpExternTok
      of "false":                                              c_sharpFalseTok
      of "field":                                              c_sharpFieldTok
      of "finally":                                            c_sharpFinallyTok
      of "fixed":                                              c_sharpFixedTok
      of "for":                                                c_sharpForTok
      of "foreach":                                            c_sharpForeachTok
      of "from":                                               c_sharpFromTok
      of "get":                                                c_sharpGetTok
      of "goto":                                               c_sharpGotoTok
      of "group":                                              c_sharpGroupTok
      of "if":                                                 c_sharpIfTok
      of "implicit":                                           c_sharpImplicitTok
      of "in":                                                 c_sharpInTok
      of "init":                                               c_sharpInitTok
      of "integer_literal":                                    c_sharpIntegerLiteral
      of "interface":                                          c_sharpInterfaceTok
      of "internal":                                           c_sharpInternalTok
      of "into":                                               c_sharpIntoTok
      of "is":                                                 c_sharpIsTok
      of "join":                                               c_sharpJoinTok
      of "let":                                                c_sharpLetTok
      of "lock":                                               c_sharpLockTok
      of "managed":                                            c_sharpManagedTok
      of "method":                                             c_sharpMethodTok
      of "module":                                             c_sharpModuleTok
      of "nameof":                                             c_sharpNameofTok
      of "namespace":                                          c_sharpNamespaceTok
      of "new":                                                c_sharpNewTok
      of "not":                                                c_sharpNotTok
      of "notnull":                                            c_sharpNotnullTok
      of "null_literal":                                       c_sharpNullLiteral
      of "on":                                                 c_sharpOnTok
      of "operator":                                           c_sharpOperatorTok
      of "or":                                                 c_sharpOrTok
      of "orderby":                                            c_sharpOrderbyTok
      of "out":                                                c_sharpOutTok
      of "override":                                           c_sharpOverrideTok
      of "param":                                              c_sharpParamTok
      of "params":                                             c_sharpParamsTok
      of "partial":                                            c_sharpPartialTok
      of "predefined_type":                                    c_sharpPredefinedType
      of "preprocessor_directive":                             c_sharpPreprocessorDirective
      of "private":                                            c_sharpPrivateTok
      of "property":                                           c_sharpPropertyTok
      of "protected":                                          c_sharpProtectedTok
      of "public":                                             c_sharpPublicTok
      of "readonly":                                           c_sharpReadonlyTok
      of "real_literal":                                       c_sharpRealLiteral
      of "record":                                             c_sharpRecordTok
      of "ref":                                                c_sharpRefTok
      of "remove":                                             c_sharpRemoveTok
      of "return":                                             c_sharpReturnTok
      of "sealed":                                             c_sharpSealedTok
      of "select":                                             c_sharpSelectTok
      of "set":                                                c_sharpSetTok
      of "sizeof":                                             c_sharpSizeofTok
      of "stackalloc":                                         c_sharpStackallocTok
      of "static":                                             c_sharpStaticTok
      of "struct":                                             c_sharpStructTok
      of "switch":                                             c_sharpSwitchTok
      of "this":                                               c_sharpThisTok
      of "throw":                                              c_sharpThrowTok
      of "true":                                               c_sharpTrueTok
      of "try":                                                c_sharpTryTok
      of "type":                                               c_sharpTypeTok
      of "typeof":                                             c_sharpTypeofTok
      of "unchecked":                                          c_sharpUncheckedTok
      of "unmanaged":                                          c_sharpUnmanagedTok
      of "unsafe":                                             c_sharpUnsafeTok
      of "using":                                              c_sharpUsingTok
      of "var":                                                c_sharpVarTok
      of "verbatim_string_literal":                            c_sharpVerbatimStringLiteral
      of "virtual":                                            c_sharpVirtualTok
      of "void_keyword":                                       c_sharpVoidKeyword
      of "volatile":                                           c_sharpVolatileTok
      of "when":                                               c_sharpWhenTok
      of "where":                                              c_sharpWhereTok
      of "while":                                              c_sharpWhileTok
      of "with":                                               c_sharpWithTok
      of "yield":                                              c_sharpYieldTok
      of "{":                                                  c_sharpLCurlyTok
      of "{{":                                                 c_sharpDoubleLCurlyTok
      of "|":                                                  c_sharpPipeTok
      of "|=":                                                 c_sharpPipeEqualTok
      of "||":                                                 c_sharpDoublePipeTok
      of "}":                                                  c_sharpRCurlyTok
      of "~":                                                  c_sharpTildeTok
      of "ERROR":                                              c_sharpSyntaxError
      else:
        raiseAssert("Invalid element name \'" & node.tsNodeType & "\'")


func isNil*(node: TsC_sharpNode): bool =
  ts_node_is_null(TSNode(node))

func len*(node: TsC_sharpNode, unnamed: bool = false): int =
  if unnamed:
    int(ts_node_child_count(TSNode(node)))

  else:
    int(ts_node_named_child_count(TSNode(node)))


func has*(node: TsC_sharpNode, idx: int, unnamed: bool = false): bool =
  0 <= idx and idx < node.len(unnamed)

proc tree_sitter_c_sharp(): PtsLanguage {.importc, cdecl.}


proc tsNodeType*(node: TsC_sharpNode): string =
  $ts_node_type(TSNode(node))

proc newTsC_sharpParser*(): C_sharpParser =
  result = C_sharpParser(ts_parser_new())
  discard ts_parser_set_language(PtsParser(result), tree_sitter_c_sharp())

proc parseString*(parser: C_sharpParser, str: string): TsC_sharpNode =
  TsC_sharpNode(ts_tree_root_node(ts_parser_parse_string(PtsParser(parser), nil, str.cstring, uint32(len(str)))))

proc parseTsC_sharpString*(str: string): TsC_sharpNode =
  let parser = newTsC_sharpParser()
  return parseString(parser, str)

func `$`*(node: TsC_sharpNode): string =
  if isNil(node):
    "<nil tree>"

  else:
    $node.kind


func `[]`*(
    node: TsC_sharpNode,
    idx:  int,
    kind: C_sharpNodeKind | set[C_sharpNodeKind]
  ): TsC_sharpNode =
  assert 0 <= idx and idx < node.len
  result = TsC_sharpNode(ts_node_named_child(TSNode(node), uint32(idx)))
  assertKind(result, kind, "Child node at index " & $idx & " for node kind " & $node.kind)

type
  C_sharpNode* = HtsNode[TsC_sharpNode, C_sharpNodeKind]


proc treeReprTsC_sharp*(str: string, unnamed: bool = false): ColoredText =
  treeRepr[TsC_sharpNode, C_sharpNodeKind](parseTsC_sharpString(str), str, 7, unnamed = unnamed)

proc toHtsNode*(
    node: TsC_sharpNode,
    str:  ptr string
  ): HtsNode[TsC_sharpNode, C_sharpNodeKind] =
  toHtsNode[TsC_sharpNode, C_sharpNodeKind](node, str)

proc toHtsTree*(node: TsC_sharpNode, str: ptr string): C_sharpNode =
  toHtsNode[TsC_sharpNode, C_sharpNodeKind](node, str)

proc parseC_sharpString*(str: ptr string, unnamed: bool = false): C_sharpNode =
  let parser = newTsC_sharpParser()
  return toHtsTree[TsC_sharpNode, C_sharpNodeKind](parseString(parser, str[]), str)

proc parseC_sharpString*(str: string, unnamed: bool = false): C_sharpNode =
  let parser = newTsC_sharpParser()
  return toHtsTree[TsC_sharpNode, C_sharpNodeKind](parseString(parser, str), unsafeAddr str, storePtr = false)


