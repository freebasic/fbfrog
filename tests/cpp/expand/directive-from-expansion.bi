#pragma once

'' TODO: unrecognized construct:
'' #define foo
'' ---------------------------------------------------------------------------
'' expected a data type starting a declaration but found '#'
''    # define foo
''    ^
'' tests/cpp/expand/directive-from-expansion.h(4): token found here
''    2: 
''    3: // CPP doesn't support directives coming from macro expansions
''    4: #define m #define foo
''                 ^
''    5: m
''    6: 
