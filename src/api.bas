#include once "api.bi"

dim shared osinfo(0 to OS__COUNT-1) as OsInfo => { _
	_ ''                           is_unix has_64bit has_arm
	(@"linux"  , @"__FB_LINUX__"  ,  TRUE,  TRUE,  TRUE), _
	(@"freebsd", @"__FB_FREEBSD__",  TRUE,  TRUE,  TRUE), _
	(@"openbsd", @"__FB_OPENBSD__",  TRUE,  TRUE,  TRUE), _
	(@"netbsd" , @"__FB_NETBSD__" ,  TRUE,  TRUE,  TRUE), _
	(@"darwin" , @"__FB_DARWIN__" ,  TRUE,  TRUE, FALSE), _
	(@"windows", @"__FB_WIN32__"  , FALSE,  TRUE, FALSE), _
	(@"cygwin" , @"__FB_CYGWIN__" ,  TRUE,  TRUE, FALSE), _
	(@"dos"    , @"__FB_DOS__"    , FALSE, FALSE, FALSE)  _
}

dim shared archinfo(0 to ARCH__COUNT-1) as ArchInfo => { _
	_ ''       is_64bit is_arm
	(@"x86"    , FALSE, FALSE), _
	(@"x86_64" ,  TRUE, FALSE), _
	(@"arm"    , FALSE,  TRUE), _
	(@"aarch64",  TRUE,  TRUE)  _
}

function osParse(byref s as string) as integer
	for i as integer = 0 to OS__COUNT - 1
		if *osinfo(i).id = s then
			return i
		end if
	next
	function = -1
end function

function archParse(byref s as string) as integer
	for i as integer = 0 to ARCH__COUNT - 1
                if *archinfo(i).id = s then
			return i
		end if
	next
	function = -1
end function

function TargetInfo.id() as string
	if (os = OS_DOS) and (arch = ARCH_X86) then
		return "dos"
	end if

	if os = OS_WINDOWS then
		select case arch
		case ARCH_X86 : return "win32"
		case ARCH_X86_64 : return "win64"
		end select
	end if

	return *osinfo(os).id + "-" + *archinfo(arch).id
end function

function ApiBits.calcAccess(byval api as integer, byref element as integer) as ulongint
	assert(element = 0)
	while api >= 64
		element += 1
		api -= 64
	wend
	assert((api >= 0) and (api < 64))
	assert((element >= 0) and (element < BitsArrayElements))
	function = 1ull shl api
end function

'' Set the bit identified by the given index
sub ApiBits.set(byval api as integer)
	dim element as integer
	var bitmask = calcAccess(api, element)
	bits(element) or= bitmask
end sub

sub ApiBits.set(byref rhs as ApiBits)
	for i as integer = 0 to BitsArrayElements - 1
		bits(i) or= rhs.bits(i)
	next
end sub

function ApiBits.isSet(byval api as integer) as integer
	dim element as integer
	var bitmask = calcAccess(api, element)
	function = ((bits(element) and bitmask) <> 0)
end function

function ApiBits.equals(byref rhs as ApiBits) as integer
	for i as integer = 0 to BitsArrayElements - 1
		if bits(i) <> rhs.bits(i) then exit function
	next
	function = TRUE
end function

function ApiBits.coversAtLeast(byref rhs as ApiBits) as integer
	for i as integer = 0 to BitsArrayElements - 1
		var rhsbits = rhs.bits(i)
		if (bits(i) and rhsbits) <> rhsbits then exit function
	next
	function = TRUE
end function

function ApiBits.hasAtLeast1Set() as integer
	for i as integer = 0 to BitsArrayElements - 1
		if bits(i) then return TRUE
	next
end function

function ApiBits.containsNoneOf(byref rhs as ApiBits) as integer
	for i as integer = 0 to BitsArrayElements - 1
		var rhsbits = rhs.bits(i)
		if (bits(i) and rhsbits) <> 0 then exit function
	next
	function = TRUE
end function

function ApiBits.dump() as string
	dim s as string
	for i as integer = BitsArrayElements - 1 to 0 step -1
		s += bin(bits(i))
		if i > 0 then
			s += "|"
		end if
	next
	function = s
end function
