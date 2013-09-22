dim shared i as long
version major(1) and (minor(0) or minor(99))
	dim shared major1 as long
end version
version major(2) and (minor(0) or minor(99))
	dim shared major2 as long
end version
version major(3) and (minor(0) or minor(99))
	dim shared major3 as long
end version
version major(4) and (minor(0) or minor(99))
	dim shared major4 as long
end version
version major(5) and (minor(0) or minor(99))
	dim shared major5 as long
end version
version major(6) and (minor(0) or minor(99))
	dim shared major6 as long
end version
version ((((major(1) or major(2)) or major(3)) or major(4)) or major(5)) or major(6)
	version minor(0)
		dim shared minor0 as long
	end version
	version minor(99)
		dim shared minor99 as long
	end version
end version
version major(3) and minor(1)
	dim shared major3 as long
end version
version major(4) and minor(1)
	dim shared major4 as long
end version
version major(5) and (minor(1) or minor(2))
	dim shared major5 as long
end version
version major(6) and ((minor(1) or minor(2)) or minor(3))
	dim shared major6 as long
end version
version (((major(3) or major(4)) or major(5)) or major(6)) and minor(1)
	dim shared minor1 as long
end version
version (major(5) or major(6)) and minor(2)
	dim shared minor2 as long
end version
version major(6) and minor(3)
	dim shared minor3 as long
end version
type UDT
	version major(1) and (minor(0) or minor(99))
		fieldmajor1 as long
	end version
	version major(2) and (minor(0) or minor(99))
		fieldmajor2 as long
	end version
	version ((major(3) or major(4)) and ((minor(1) or minor(0)) or minor(99))) or ((major(5) and (((minor(1) or minor(2)) or minor(0)) or minor(99))) or (major(6) and ((((minor(1) or minor(2)) or minor(3)) or minor(0)) or minor(99))))
		fieldmajor3456 as long
	end version
end type
