dim shared i as long
version "1"."0" or "1"."99"
	dim shared major1 as long
end version
version "2"."0" or "2"."99"
	dim shared major2 as long
end version
version "3"."0" or "3"."99"
	dim shared major3 as long
end version
version "4"."0" or "4"."99"
	dim shared major4 as long
end version
version "5"."0" or "5"."99"
	dim shared major5 as long
end version
version "6"."0" or "6"."99"
	dim shared major6 as long
end version
version (((("1"."0" or "2"."0") or "3"."0") or "4"."0") or "5"."0") or "6"."0"
	dim shared minor0 as long
end version
version (((("1"."99" or "2"."99") or "3"."99") or "4"."99") or "5"."99") or "6"."99"
	dim shared minor99 as long
end version
version "3"."1"
	dim shared major3 as long
end version
version "4"."1"
	dim shared major4 as long
end version
version "5"."1" or "5"."2"
	dim shared major5 as long
end version
version ("6"."1" or "6"."2") or "6"."3"
	dim shared major6 as long
end version
version (("3"."1" or "4"."1") or "5"."1") or "6"."1"
	dim shared minor1 as long
end version
version "5"."2" or "6"."2"
	dim shared minor2 as long
end version
version "6"."3"
	dim shared minor3 as long
end version
