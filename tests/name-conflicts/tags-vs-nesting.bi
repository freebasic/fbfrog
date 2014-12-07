#pragma once

'' The following symbols have been renamed:
''     inside struct UDT1:
''         struct screen => screen_
''     inside struct UDT2:
''         struct screen_ => screen__
''     inside struct UDT3:
''         struct screen__ => screen___
''     struct screen___ => screen____
''     typedef SCREEN => SCREEN_

type SCREEN_ as screen____

type UDT1
	i as long
end type

type UDT2
	i as long
end type

type UDT3
	i as long
end type
