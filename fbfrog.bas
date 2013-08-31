'' Main module, command line interface

#include once "fbfrog.bi"
#include once "file.bi"

dim shared verbose as integer

private sub hPrintHelp( byref message as string )
	if( len( message ) > 0 ) then
		print message
	end if
	print "fbfrog 0.1 from " + __DATE_ISO__ + ", usage: fbfrog [options] *.h"
	print "options:"
	print "  -m           Merge multiple headers into one"
	print "  -v           Show debugging info"
	print "By default, fbfrog will generate a *.bi file for each given *.h file."
	print "*.bi files need to be reviewed and tested! Watch out for calling conventions!"
	end (iif( len( message ) > 0, 1, 0 ))
end sub

type FROGSTUFF
	files		as TLIST '' FROGFILE
	filehash	as THASH
	commonparent	as string
end type

dim shared frog as FROGSTUFF

private function frogDownload( byref url as string, byref file as string ) as integer
	if( fileexists( "tarballs/" & file ) ) then
		function = TRUE
	else
		function = hShell( "mkdir -p tarballs" ) andalso _
			hShell( "wget " + url + " -O tarballs/" + file )
	end if
end function

private function frogExtract( byref tarball as string, byref dirname as string ) as integer
	if( strEndsWith( tarball, ".zip" ) ) then
		function = hShell( "unzip -q -d " + dirname + " tarballs/" + tarball )
	elseif( strEndsWith( tarball, ".tar.gz" ) or _
	        strEndsWith( tarball, ".tar.bz2" ) or _
	        strEndsWith( tarball, ".tar.xz" ) ) then
		function = hShell( "tar xf tarballs/" + tarball + " -C " + dirname )
	else
		function = FALSE
	end if
end function

private function hFindCommonParent( ) as string
	dim as string s
	dim as FROGFILE ptr f = listGetHead( @frog.files )
	while( f )
		if( f->missing = FALSE ) then
			if( len( s ) > 0 ) then
				s = pathFindCommonBase( s, f->normed )
			else
				s = pathOnly( f->normed )
			end if
		end if
		f = listGetNext( f )
	wend
	function = s
end function

private function frogAddFile _
	( _
		byval context as FROGFILE ptr, _
		byref pretty as string _
	) as FROGFILE ptr

	dim as string normed, report

	if( context ) then
		'' Search for #included files in one of the parent directories
		'' of the current file. Usually the #include will refer to a
		'' file in the same directory or in a sub-directory at the same
		'' level or some levels up.

		var parent = pathOnly( context->normed )
		do
			'' File not found anywhere, ignore it.
			if( len( parent ) = 0 ) then
				normed = ""
				exit do
			end if

			normed = parent + pretty
			if( hFileExists( normed ) ) then
				if( verbose ) then
					if( len( report ) ) then print report
					report = "    found: " + normed
				end if
				exit do
			end if

			if( verbose ) then
				if( len( report ) ) then print report
				report = "    not found: " + normed
			end if

			'' Stop searching parent directories after trying
			'' the common base
			if( parent = frog.commonparent ) then
				normed = ""
				exit do
			end if

			parent = pathStripLastComponent( parent )
		loop
	else
		normed = pathMakeAbsolute( pretty )
		if( verbose ) then
			report = "    root: " + pretty
		end if
	end if

	var missing = FALSE
	if( len( normed ) > 0 ) then
		normed = pathNormalize( normed )
	else
		'' File missing/not found; still add it to the hash
		normed = pretty
		missing = TRUE
	end if

	var hash = hashHash( normed )
	var item = hashLookup( @frog.filehash, normed, hash )
	if( item->s ) then
		'' Already exists
		if( verbose ) then
			print report + " (old news)"
		end if
		return item->data
	end if

	if( verbose ) then
		if( missing ) then
			print report + " (missing)"
		else
			print report + " (new)"
		end if
	end if

	'' Add file
	dim as FROGFILE ptr f = listAppend( @frog.files )
	f->pretty = pretty
	f->normed = normed
	f->missing = missing

	'' Add to hash table
	hashAdd( @frog.filehash, item, hash, f->normed, f )

	'' Update/recalculate common base path
	frog.commonparent = hFindCommonParent( )

	function = f
end function

private function frogParseVersion _
	( _
		byval pre as FROGPRESET ptr, _
		byval f as FROGFILE ptr, _
		byval version as ASTNODE ptr _
	) as ASTNODE ptr

	if( verbose ) then
		if( version ) then
			print "version " + *version->text
		end if
	end if

	tkInit( )

	var comments = ((pre->options and PRESETOPT_COMMENTS) <> 0)
	lexLoadFile( 0, f, LEXMODE_C, comments )

	'' Parse PP directives, and expand #includes if wanted and possible.
	''
	'' If new tokens were loaded from an #include, we have to parse for PP
	'' directives etc. again, to handle any PP directives in the added
	'' tokens. There may even be new #include directives in them which
	'' themselves may need expanding.

	var have_new_tokens = TRUE
	while( have_new_tokens )

		ppComments( )
		ppDividers( )
		ppDirectives1( )
		have_new_tokens = FALSE

		if( (pre->options and PRESETOPT_NOMERGE) = 0 ) then
			var x = 0
			while( tkGet( x ) <> TK_EOF )

				'' #include?
				if( tkGet( x ) = TK_PPINCLUDE ) then
					var incfile = *tkGetText( x )
					var incf = frogAddFile( f, incfile )

					if( (not incf->missing) and (incf->refcount = 1) and _
					    ((incf->mergeparent = NULL) or (incf->mergeparent = f)) and _
					    (incf <> f) ) then
						'' Replace #include by included file's content
						tkRemove( x, x )
						lexLoadFile( x, incf, LEXMODE_C, comments )
						have_new_tokens = TRUE

						'' Counter the +1 below, so this position is re-parsed
						x -= 1

						incf->mergeparent = f
						if( verbose ) then
							print "    merged in: " + incf->pretty
						end if
					end if
				end if

				x += 1
			wend
		end if
	wend

	ppDirectives2( )

	''
	'' Macro expansion, #if evaluation
	''
	ppEvalInit( )

	scope
		var child = pre->undefs->head
		while( child )
			ppAddSym( child->text, FALSE )
			child = child->next
		wend
	end scope

	scope
		var child = pre->defines->head
		while( child )
			ppAddSym( child->text, TRUE )
			child = child->next
		wend
	end scope

	scope
		var child = pre->expands->head
		while( child )
			ppExpandSym( child->text )
			child = child->next
		wend
	end scope

	scope
		var child = pre->macros->head
		while( child )
			ppAddMacro( astClone( child ) )
			child = child->next
		wend
	end scope

	if( (pre->options and PRESETOPT_NOPP) = 0 ) then
		ppEval( )
	else
		ppParseIfExprOnly( ((pre->options and PRESETOPT_NOPPFOLD) = 0) )
	end if
	ppEvalEnd( )
	ppRemoveEOLs( )

	'' Parse C constructs
	var ast = cFile( )

	tkEnd( )

	''
	'' Work on the AST
	''
	'astRemoveParamNames( ast )
	astFixArrayParams( ast )
	astRemoveRedundantTypedefs( ast )
	if( (pre->options and PRESETOPT_NOAUTOEXTERN) = 0 ) then
		astAutoExtern( ast )
	end if
	astMergeDIVIDERs( ast )

	function = ast
end function

private sub frogParse( byval pre as FROGPRESET ptr, byval f as FROGFILE ptr )
	print "parsing: ";f->pretty

	dim as ASTNODE ptr ast

	'' Any versions specified?
	var version = pre->versions->head
	if( version ) then
		var fullversion = astNewVERSION( )

		'' For each version...
		do
			'' Parse files into AST, put the AST into a VERSION block, and merge
			'' it with previous ones
			ast = astMergeVersions( ast, astNewVERSION( version, frogParseVersion( pre, f, version ) ) )

			'' Collect each version's version number
			astCloneAndAddAllChildrenOf( fullversion->expr, version->expr )

			version = version->next
		loop while( version )

		'' Remove VERSION blocks if they cover all versions, because if
		'' code they contain appears in all versions, then the VERSION
		'' block isn't needed. VERSION blocks are only needed for code
		'' that is specific to some versions but not all.
		ast = astSolveVersionsOut( ast, fullversion )
	else
		'' Just do a single pass, don't worry about version specifics or AST merging
		ast = frogParseVersion( pre, f, NULL )
	end if

	f->ast = ast
end sub

private sub frogWork( byval pre as FROGPRESET ptr, byref presetfile as string )
	listInit( @frog.files, sizeof( FROGFILE ) )
	hashInit( @frog.filehash, 6 )

	'' Download tarballs
	scope
		var child = pre->downloads->head
		while( child )
			if( frogDownload( *child->text, *child->comment ) = FALSE ) then
				oops( "failed to download " + *child->comment )
			end if
			child = child->next
		wend
	end scope

	'' Extract tarballs
	scope
		var child = pre->extracts->head
		while( child )
			if( frogExtract( *child->text, *child->comment ) = FALSE ) then
				oops( "failed to extract " + *child->text )
			end if
			child = child->next
		wend
	end scope

	'' Input files
	scope
		var child = pre->files->head
		while( child )
			'' File from command line, search in current directory
			frogAddFile( NULL, *child->text )
			child = child->next
		wend
	end scope

	'' Input files from directories
	scope
		var child = pre->dirs->head
		while( child )

			dim as TLIST list
			listInit( @list, sizeof( string ) )

			hScanDirectoryForH( *child->text, @list )

			dim as string ptr s = listGetHead( @list )
			while( s )
				frogAddFile( NULL, *s )
				*s = ""
				s = listGetNext( s )
			wend

			listEnd( @list )

			child = child->next
		wend
	end scope

	if( listGetHead( @frog.files ) = NULL ) then
		if( len( presetfile ) > 0 ) then
			oops( "no input files for '" + presetfile + "'" )
		else
			oops( "no input files" )
		end if
	end if

	dim as FROGFILE ptr f

	if( (pre->options and PRESETOPT_NOMERGE) = 0 ) then
		print "preparsing to determine #include dependencies..."

		'' Preparse to find #includes and calculate refcounts
		'' Files newly registered by the inner loop will eventually be worked
		'' off by the outer loop, as they're appended to the files list.
		f = listGetHead( @frog.files )
		while( f )
			if( f->missing = FALSE ) then
				print "preparsing: ";f->pretty

				tkInit( )
				lexLoadFile( 0, f, LEXMODE_C, FALSE )

				ppDirectives1( )

				'' Find #include directives
				var x = 0
				while( tkGet( x ) <> TK_EOF )

					if( tkGet( x ) = TK_PPINCLUDE ) then
						var incfile = *tkGetText( x )

						print "  #include: " & incfile;
						if( verbose ) then
							print
						end if

						var incf = frogAddFile( f, incfile )
						incf->refcount += 1

						if( verbose = FALSE ) then
							if( incf->missing ) then
								print " (not found)"
							end if
						end if
					end if

					x += 1
				wend

				tkEnd( )
			end if

			f = listGetNext( f )
		wend

		'' Pass 1: Process any files that don't look like they'll be
		'' merged, i.e. refcount <> 1.
		f = listGetHead( @frog.files )
		while( f )

			if( (not f->missing) and (f->refcount <> 1) ) then
				assert( f->mergeparent = NULL )
				frogParse( pre, f )

				dim as FROGFILE ptr incf = listGetHead( @frog.files )
				while( incf )
					if( incf->mergeparent = f ) then
						print "merged in: " + incf->pretty
					end if
					incf = listGetNext( incf )
				wend
			end if

			f = listGetNext( f )
		wend

		'' Pass 2: Process files that looked like they should be merged,
		'' but weren't. This happens with recursive #includes, where all
		'' have refcount > 0, so none of them were merged in anywhere
		'' during the 1st pass.
		f = listGetHead( @frog.files )
		while( f )

			if( (not f->missing) and (f->refcount = 1) and (f->mergeparent = NULL) ) then
				frogParse( pre, f )

				dim as FROGFILE ptr incf = listGetHead( @frog.files )
				while( incf )
					if( incf->mergeparent = f ) then
						print "merged in: " + incf->pretty
					end if
					incf = listGetNext( incf )
				wend
			end if

			f = listGetNext( f )
		wend

		'' Concatenate files with refcount=0
		dim as FROGFILE ptr first
		f = listGetHead( @frog.files )
		while( f )
			if( f->refcount = 0 ) then
				if( first ) then
					'' Already have a first; append to it
					if( f->ast ) then
						astAppend( first->ast, f->ast )
						f->ast = NULL
					end if
				else
					'' This is the first
					first = f
				end if
			end if
			f = listGetNext( f )
		wend
	else
		'' No merging requested, just process each file that was found
		'' individually.
		f = listGetHead( @frog.files )
		while( f )
			if( f->missing = FALSE ) then
				frogParse( pre, f )
			end if
			f = listGetNext( f )
		wend
	end if

	'' Emit all files that have an AST (i.e. weren't merged into or
	'' appended to anything)
	f = listGetHead( @frog.files )
	while( f )
		if( f->ast ) then
			var binormed = pathStripExt( f->normed ) + ".bi"
			var bipretty = pathStripExt( f->pretty ) + ".bi"
			print "emitting: " + bipretty
			'astDump( f->ast )
			emitFile( binormed, f->ast )
		end if
		f = listGetNext( f )
	wend

	hashEnd( @frog.filehash )
	do
		f = listGetHead( @frog.files )
		if( f = NULL ) then exit do
		f->pretty = ""
		f->normed = ""
		astDelete( f->ast )
		listDelete( @frog.files, f )
	loop
	listEnd( @frog.files )
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	'' Input files and various other info from command line
	dim cmdline as FROGPRESET
	presetInit( @cmdline )
	cmdline.options or= PRESETOPT_NOMERGE

	'' *.fbfrog files from command line
	var presetfiles = astNewGROUP( )

	for i as integer = 1 to __FB_ARGC__-1
		var arg = *__FB_ARGV__[i]

		'' option?
		if( left( arg, 1 ) = "-" ) then
			'' Strip all preceding '-'s
			do
				arg = right( arg, len( arg ) - 1 )
			loop while( left( arg, 1 ) = "-" )

			select case( arg )
			case "h", "?", "help", "version"
				hPrintHelp( "" )
			case "m", "merge"
				cmdline.options and= not PRESETOPT_NOMERGE
			case "v", "verbose"
				verbose = TRUE
			case else
				hPrintHelp( "unknown option: " + *__FB_ARGV__[i] )
			end select
		else
			select case( pathExtOnly( arg ) )
			case "h", "hh", "hxx", "hpp", "c", "cc", "cxx", "cpp"
				presetAddFile( @cmdline, arg )
			case ""
				'' No extension? Treat as directory
				presetAddDir( @cmdline, arg )
			case "fbfrog"
				astAppend( presetfiles, astNewTEXT( arg ) )
			case else
				hPrintHelp( "'" + arg + "' is not a *.h file" )
			end select
		end if
	next

	'' If *.fbfrog files were given, work them off one by one
	var presetfile = presetfiles->head
	if( presetfile ) then
		'' For each *.fbfrog file...
		do
			var presetfilename = *presetfile->text

			'' Read in the *.fbfrog preset
			dim as FROGPRESET pre
			presetInit( @pre )
			presetParse( @pre, presetfilename )

			'' Any additional *.h input files given on the command
			'' line (besides the *.fbfrog file(s)) override input
			'' files from the *.fbfrog file. This allows the preset
			'' to be used on other files for testing.
			if( presetHasInput( @cmdline ) ) then
				presetOverrideInput( @pre, @cmdline )
			end if

			frogWork( @pre, presetfilename )
			presetEnd( @pre )

			presetfile = presetfile->next
		loop while( presetfile )
	else
		'' Otherwise just work off the input files and options given
		'' on the command line, it's basically a "preset" too, just not
		'' stored in a *.fbfrog file but given through command line
		'' options.
		frogWork( @cmdline, "" )
	end if
