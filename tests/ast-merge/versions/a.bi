dim shared i as long
version major(1).minor(0) or major(1).minor(99)
	dim shared major1 as long
end version
version major(2).minor(0) or major(2).minor(99)
	dim shared major2 as long
end version
version major(3).minor(0) or major(3).minor(99)
	dim shared major3 as long
end version
version major(4).minor(0) or major(4).minor(99)
	dim shared major4 as long
end version
version major(5).minor(0) or major(5).minor(99)
	dim shared major5 as long
end version
version major(6).minor(0) or major(6).minor(99)
	dim shared major6 as long
end version
version ((((major(1).minor(0) or major(2).minor(0)) or major(3).minor(0)) or major(4).minor(0)) or major(5).minor(0)) or major(6).minor(0)
	dim shared minor0 as long
end version
version ((((major(1).minor(99) or major(2).minor(99)) or major(3).minor(99)) or major(4).minor(99)) or major(5).minor(99)) or major(6).minor(99)
	dim shared minor99 as long
end version
version major(3).minor(1)
	dim shared major3 as long
end version
version major(4).minor(1)
	dim shared major4 as long
end version
version major(5).minor(1) or major(5).minor(2)
	dim shared major5 as long
end version
version (major(6).minor(1) or major(6).minor(2)) or major(6).minor(3)
	dim shared major6 as long
end version
version ((major(3).minor(1) or major(4).minor(1)) or major(5).minor(1)) or major(6).minor(1)
	dim shared minor1 as long
end version
version major(5).minor(2) or major(6).minor(2)
	dim shared minor2 as long
end version
version major(6).minor(3)
	dim shared minor3 as long
end version
type UDT
	version major(1).minor(0) or major(1).minor(99)
		fieldmajor1 as long
	end version
	version major(2).minor(0) or major(2).minor(99)
		fieldmajor2 as long
	end version
	version (((((((((((((major(3).minor(1) or major(3).minor(0)) or major(3).minor(99)) or major(4).minor(1)) or major(4).minor(0)) or major(4).minor(99)) or major(5).minor(1)) or major(5).minor(2)) or major(5).minor(0)) or major(5).minor(99)) or major(6).minor(1)) or major(6).minor(2)) or major(6).minor(3)) or major(6).minor(0)) or major(6).minor(99)
		fieldmajor3456 as long
	end version
end type
