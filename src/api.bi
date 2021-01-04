enum
	OS_LINUX
	OS_FREEBSD
	OS_OPENBSD
	OS_NETBSD
	OS_DARWIN
	OS_WINDOWS
	OS_CYGWIN
	OS_DOS
	OS__COUNT
end enum

enum
	ARCH_X86
	ARCH_X86_64
	ARCH_ARM
	ARCH_AARCH64
	ARCH__COUNT
end enum

type OsInfo
	as zstring ptr id, fbdefine
	as byte is_unix, has_64bit, has_arm
end type

type ArchInfo
	id as zstring ptr
	as byte is_64bit, is_arm
end type

extern osinfo(0 to OS__COUNT-1) as OsInfo
extern archinfo(0 to ARCH__COUNT-1) as ArchInfo

declare function osParse(byref s as string) as integer
declare function archParse(byref s as string) as integer

type TargetInfo
	as byte os, arch
	declare function id() as string
end type

type ApiBits
	const BitsArrayElements = 4
	const MaxApis = sizeof(ulongint) * BitsArrayElements * 8

	bits(0 to BitsArrayElements-1) as ulongint

	declare function calcAccess(byval api as integer, byref element as integer) as ulongint
	declare sub set(byval api as integer)
	declare sub set(byref rhs as ApiBits)
	declare function isSet(byval api as integer) as integer
	declare function equals(byref rhs as ApiBits) as integer
	declare function coversAtLeast(byref rhs as ApiBits) as integer
	declare function hasAtLeast1Set() as integer
	declare function containsNoneOf(byref rhs as ApiBits) as integer
	declare function dump() as string
end type
#assert sizeof(ApiBits) = 32
