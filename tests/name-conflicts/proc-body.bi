#pragma once

'' The following symbols have been renamed:
''     procedure screen => screen_

extern "C"

sub screen_ alias "screen"(byval color_ as long)
	screen_(color_)
end sub

end extern
