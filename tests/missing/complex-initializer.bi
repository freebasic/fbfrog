#pragma once

type Vector2A
	x as long
	y as long
end type

dim shared a(0 to 3) as Vector2A = {(0, 0), (1, 0), (0, 1), (1, 1)}

type Vector2B
	coord(0 to 1) as long
end type

dim shared b(0 to 3) as Vector2B = {(0, 0), (1, 0), (0, 1), (1, 1)}
