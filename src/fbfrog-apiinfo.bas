#include once "fbfrog-apiinfo.bi"

#include once "ast-merge.bi"
#include once "fbfrog-replacements.bi"
#include once "fbfrog.bi"
#include once "tk.bi"
#include once "util.bi"

using tktokens

constructor ApiInfo()
	for i as integer = lbound(renameopt) to ubound(renameopt)
		renameopt(i).constructor(3, FALSE)
	next
	log = astNewGROUP()
end constructor

destructor ApiInfo()
	for i as integer = 0 to replacementcount - 1
		deallocate(replacements[i].fromcode)
		deallocate(replacements[i].tocode)
	next
	deallocate(replacements)
	delete log
end destructor

sub ApiInfo.addReplacement(byval fromcode as zstring ptr, byval tocode as zstring ptr, byval tofb as integer)
	var i = replacementcount
	replacementcount += 1
	replacements = reallocate(replacements, replacementcount * sizeof(*replacements))
	clear(replacements[i], 0, sizeof(*replacements))
	with replacements[i]
		.fromcode = strDuplicate(fromcode)
		.tocode = strDuplicate(tocode)
		.tofb = tofb
	end with
end sub

sub ApiInfo.loadOption(byval opt as integer, byval param1 as zstring ptr, byval param2 as zstring ptr)
	select case as const opt
	case OPT_WINDOWSMS        : windowsms        = TRUE
	case OPT_CLONG32          : clong32          = TRUE
	case OPT_FIXUNSIZEDARRAYS : fixunsizedarrays = TRUE
	case OPT_NOFUNCTIONBODIES : nofunctionbodies = TRUE
	case OPT_DROPMACROBODYSCOPES : dropmacrobodyscopes = TRUE
	case OPT_REMOVEEMPTYRESERVEDDEFINES : removeEmptyReservedDefines = TRUE

	case OPT_RENAMETYPEDEF, OPT_RENAMETAG, OPT_RENAMEPROC, _
	     OPT_RENAMEDEFINE, OPT_RENAMEMACROPARAM, OPT_RENAME
		renameopt(opt).addOverwrite(param1, param2)
		have_renames = TRUE

	case OPT_RENAME_, OPT_REMOVE, OPT_REMOVEDEFINE, OPT_REMOVEPROC, OPT_REMOVEVAR, OPT_REMOVE1ST, OPT_REMOVE2ND, _
	     OPT_DROPPROCBODY, OPT_TYPEDEFHINT, OPT_ADDFORWARDDECL, OPT_UNDEFBEFOREDECL, OPT_IFNDEFDECL, _
	     OPT_CONVBODYTOKENS, OPT_FORCEFUNCTION2MACRO, OPT_EXPANDINDEFINE, OPT_NOEXPAND, OPT_EXPAND
		if opt = OPT_RENAME_ then
			have_renames = TRUE
		end if
		idopt(opt).addPattern(param1)

	case OPT_NOSTRING, OPT_STRING
		patterns(opt).parseAndAdd(*param1)

	case OPT_REMOVEINCLUDE
		removeinclude.addOverwrite(param1, NULL)

	case OPT_SETARRAYSIZE
		setarraysizeoptions.addOverwrite(param1, param2)

	case OPT_MOVEABOVE
		var n = astNewTEXT(param1)
		n->setAlias(param2)
		astBuildGroupAndAppend(moveaboveoptions, n)

	case OPT_REPLACEMENTS
		dim parser as ReplacementsParser = ReplacementsParser(*param1)
		parser.parse(this)

	'' Distribute .bi-file-specific options for this API to invidiual .bi files
	case OPT_INCLIB, OPT_UNDEF, OPT_ADDINCLUDE
		dim bi as integer
		if param2 then
			bi = frogLookupBiFromBi(*param2)
			if bi < 0 then
				oops("couldn't find destination .bi '" + *param2 + "', for '" + *tkInfoText(opt) + " " + *param1 + "'")
			end if
		else
			'' No destination .bi file given for this -inclib/-undef option;
			'' add it to the first .bi.
			'' This allows the simple use case where there's just one output .bi,
			'' and it's not necessary to specify a destination .bi for each option.
			bi = 0
		end if

		assert((bi >= 0) and (bi < frog.bicount))
		with frog.bis[bi]
			select case opt
			case OPT_INCLIB
				astBuildGroupAndAppend(.options.inclibs, astNew(ASTKIND_INCLIB, param1))
			case OPT_UNDEF
				astBuildGroupAndAppend(.options.undefs, astNew(ASTKIND_UNDEF, param1))
			case OPT_ADDINCLUDE
				astBuildGroupAndAppend(.options.addincludes, astNew(ASTKIND_PPINCLUDE, param1))
			end select
		end with
	end select
end sub

sub ApiInfo.loadOptions()
	var i = script->head
	while i
		assert(i->kind = ASTKIND_OPTION)
		loadOption(i->opt, i->text, i->alias_)
		i = i->nxt
	wend
end sub

sub ApiInfo.print(byref ln as string)
	log->append(astNewTEXT(ln))
end sub

function ApiInfo.prettyId() as string
	var s = target.id()
	var extras = astDumpPrettyVersion(verand)
	if len(extras) > 0 then
		s += " " + extras
	end if
	function = s
end function
