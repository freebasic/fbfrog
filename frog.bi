#include once "fbfrog.bi"

enum
	DECL_PARAM = 0
	DECL_TOP
	DECL_TYPEDEF
	DECL_TYPEDEFSTRUCTBLOCK
	DECL_VAR
	DECL_FIELD
	DECL_PROC
end enum

declare function skip(byval x as integer) as integer
declare function skiprev(byval x as integer) as integer
declare function skip_unless_eol(byval x as integer) as integer
declare function skiprev_unless_eol(byval x as integer) as integer
declare function is_whitespace_until_eol(byval x as integer) as integer
declare function insert_spaced_token _
	( _
		byval x as integer, _
		byval tk as integer, _
		byval text as zstring ptr _
	) as integer
declare function insert_statement_separator(byval x as integer) as integer
declare sub remove_this_and_space(byval x as integer)
declare function find_token(byval x as integer, byval tk as integer) as integer
declare function find_parentheses _
	( _
		byval x as integer, _
		byval backwards as integer _
	) as integer
declare function parse_unknown(byval x as integer) as integer
declare function parse_pp_directive _
	( _
		byval x as integer, _
		byval is_preparse as integer _
	) as integer

declare function parse_enumconst _
	( _
		byval x as integer, _
		byval is_unknown as integer _
	) as integer
declare function parse_multdecl _
	( _
		byval x as integer, _
		byval begin as integer, _
		byval decl as integer _
	) as integer
declare function parse_topdecl_or_typedef(byval x as integer) as integer
declare function translate_enumconst(byval x as integer) as integer
declare sub fixup_multdecl(byval x as integer, byval begin as integer)
declare function translate_decl _
	( _
		byval x as integer, _
		byval decl as integer _
	) as integer

declare function parse_struct(byval x as integer) as integer
declare function parse_extern_begin(byval x as integer) as integer
declare function parse_extern_end(byval x as integer) as integer
declare function translate_compound_end _
	( _
		byval x as integer, _
		byval compoundkw as integer _
	) as integer
declare function translate_struct(byval x as integer) as integer
