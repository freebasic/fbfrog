'' Main module, command line interface

#include once "fbfrog.bi"

dim shared verbose as integer

private sub hPrintHelp( byref message as string )
	if( len( message ) > 0 ) then
		print message
	end if
	print "fbfrog 0.1 from " + __DATE_ISO__ + ", usage: fbfrog [options] *.h"
	print "options:"
	print "  -m    Merge multiple headers into one"
	print "  -w    Whitespace: Try to preserve comments and empty lines"
	print "  -v    Show debugging info"
	print "By default, fbfrog will generate a *.bi file for each given *.h file."
	print "*.bi files should be reviewed and need to be tested!"
	end (iif( len( message ) > 0, 1, 0 ))
end sub

private function frogDownload( byref url as string, byref file as string ) as integer
	var downloadfile = "tarballs" + PATHDIV + file
	if( hFileExists( downloadfile ) ) then
		function = TRUE
	else
		hMkdir( "tarballs" )
		if( hShell( "wget '" + url + "' -O """ + downloadfile + """" ) ) then
			function = TRUE
		else
			hKill( downloadfile )
			function = FALSE
		end if
	end if
end function

private function frogExtract( byref tarball as string, byref dirname as string ) as integer
	hMkdir( dirname )

	dim s as string

	if( strEndsWith( tarball, ".zip" ) ) then
		s = "unzip -q"
		s += " -d """ + dirname + """"
		s += " ""tarballs/" + tarball + """"
	elseif( strEndsWith( tarball, ".tar.gz" ) or _
	        strEndsWith( tarball, ".tar.bz2" ) or _
	        strEndsWith( tarball, ".tar.xz" ) ) then
		s = "tar xf ""tarballs/" + tarball + """"
		s += " -C """ + dirname + """"
	end if

	if( len( s ) > 0 ) then
		function = hShell( s )
	else
		function = FALSE
	end if
end function

private function frogAddFile _
	( _
		byval files as ASTNODE ptr, _
		byval context as ASTNODE ptr, _
		byval pretty as zstring ptr _
	) as ASTNODE ptr

	dim as string normed, report

	if( context ) then
		'' Search for #included files in one of the parent directories
		'' of the current file. Usually the #include will refer to a
		'' file in the same directory or in a sub-directory at the same
		'' level or some levels up.

		var parent = pathOnly( *context->text )
		do
			'' File not found anywhere?
			if( len( parent ) = 0 ) then
				normed = ""
				exit do
			end if

			normed = parent + *pretty
			if( hFileExists( normed ) ) then
				if( verbose ) then
					if( len( report ) ) then print report
					report = "        found: " + normed
				end if
				exit do
			end if

			if( verbose ) then
				if( len( report ) ) then print report
				report = "    not found: " + normed
			end if

			parent = pathStripLastComponent( parent )
		loop
	else
		normed = pathMakeAbsolute( *pretty )
		if( verbose ) then
			report = "    root: " + *pretty
		end if
	end if

	var missing = FALSE
	if( len( normed ) > 0 ) then
		normed = pathNormalize( normed )
	else
		'' File missing/not found; still add it to the hash
		normed = *pretty
		missing = TRUE
	end if

	var f = files->head
	while( f )

		if( *f->text = normed ) then
			'' Already exists
			if( verbose ) then
				print report + " (old news)"
			end if
			return f
		end if

		f = f->next
	wend

	if( verbose ) then
		if( missing ) then
			print report + " (missing)"
		else
			print report + " (new)"
		end if
	end if

	'' Add file
	f = astNewFROGFILE( normed, pretty )
	f->attrib or= ASTATTRIB_MISSING and missing
	astAppend( files, f )

	function = f
end function

private sub frogWorkFile _
	( _
		byval pre as FROGPRESET ptr, _
		byval presetcode as ASTNODE ptr, _
		byval targetversion as ASTNODE ptr, _
		byval files as ASTNODE ptr, _
		byval f as ASTNODE ptr _
	)

	print "parsing: " + *f->comment

	tkInit( )

	var whitespace = ((pre->options and PRESETOPT_WHITESPACE) <> 0)
	lexLoadFile( 0, f, FALSE, whitespace )

	'' Parse PP directives, and expand #includes if wanted and possible.
	''
	'' If new tokens were loaded from an #include, we have to parse for PP
	'' directives etc. again, to handle any PP directives in the added
	'' tokens. There may even be new #include directives in them which
	'' themselves may need expanding.

	var have_new_tokens = TRUE
	while( have_new_tokens )

		if( whitespace ) then
			ppComments( )
			ppDividers( )
		end if
		ppDirectives1( )
		have_new_tokens = FALSE

		if( (pre->options and PRESETOPT_NOMERGE) = 0 ) then
			var x = 0
			while( tkGet( x ) <> TK_EOF )

				'' #include?
				if( tkGet( x ) = TK_PPINCLUDE ) then
					var location = tkGetLocation( x )
					var contextf = location->file
					if( contextf = NULL ) then
						contextf = f
					end if
					var incfile = *tkGetText( x )
					if( verbose ) then
						print "    #include: " + incfile
					end if
					var incf = frogAddFile( files, contextf, incfile )

					if( ((incf->attrib and ASTATTRIB_MISSING) = 0) and (incf->refcount = 1) and _
					    ((incf->mergeparent = NULL) or (incf->mergeparent = f)) and _
					    (incf <> f) ) then
						'' Replace #include by included file's content
						tkRemove( x, x )
						lexLoadFile( x, incf, FALSE, whitespace )
						have_new_tokens = TRUE

						'' Counter the +1 below, so this position is re-parsed
						x -= 1

						incf->mergeparent = f
						if( verbose ) then
							print "    (merged in)"
						end if
					else
						if( verbose ) then
							print "    (not merged)"
						end if
					end if
				end if

				x += 1
			wend
		end if
	wend

	''
	'' Macro expansion, #if evaluation
	''
	ppEvalInit( )

	if( presetcode ) then
		var child = presetcode->head
		while( child )

			select case( child->class )
			case ASTCLASS_NOEXPAND
				ppNoExpandSym( child->text )
			case ASTCLASS_PPDEFINE
				ppPreDefine( child )
				ppRemoveSym( child->text )
			case ASTCLASS_PPUNDEF
				ppPreUndef( child->text )
				ppRemoveSym( child->text )
			case ASTCLASS_REMOVE
				ppRemoveSym( child->text )
			end select

			child = child->next
		wend
	end if

	if( (pre->options and PRESETOPT_NOPP) = 0 ) then
		ppEval( )
	else
		ppParseIfExprOnly( ((pre->options and PRESETOPT_NOPPFOLD) = 0) )
	end if
	ppEvalEnd( )
	ppRemoveEOLs( )
	tkTurnCPPTokensIntoCIds( )

	'' Parse C constructs
	var ast = cFile( )

	tkEnd( )

	''
	'' Work on the AST
	''
	if( (pre->options and PRESETOPT_NOPPFOLD) = 0 ) then
		astCleanUpExpressions( ast )
	end if
	'astRemoveParamNames( ast )
	astFixArrayParams( ast )
	astFixAnonUDTs( ast )
	astRemoveRedundantTypedefs( ast )
	if( (pre->options and PRESETOPT_NOAUTOEXTERN) = 0 ) then
		astAutoExtern( ast, ((pre->options and PRESETOPT_WINDOWSMS) <> 0) )
	end if
	astMergeDIVIDERs( ast )

	'' Put file's AST into a VERBLOCK, if a targetversion was given,
	'' in preparation for the astMergeFiles() call later
	if( targetversion ) then
		ast = astWrapFileInVerblock( ast, targetversion )
	end if

	f->expr = ast

	'' Report merged #includes
	var incf = files->head
	while( incf )
		if( incf->mergeparent = f ) then
			print "merged in: " + *incf->comment
		end if
		incf = incf->next
	wend

end sub

private function frogWorkVersion _
	( _
		byval pre as FROGPRESET ptr, _
		byval targetversion as ASTNODE ptr, _
		byval presetcode as ASTNODE ptr, _
		byref presetfilename as string, _
		byref presetprefix as string _
	) as ASTNODE ptr

	var files = astNewGROUP( )

	var child = presetcode->head
	while( child )

		select case( child->class )
		case ASTCLASS_DOWNLOAD
			'' Download tarballs
			if( frogDownload( *child->text, *child->comment ) = FALSE ) then
				oops( "failed to download " + *child->comment )
			end if

		case ASTCLASS_EXTRACT
			'' Extract tarballs
			if( frogExtract( *child->text, presetprefix + *child->comment ) = FALSE ) then
				oops( "failed to extract " + *child->text )
			end if

		case ASTCLASS_FILE
			'' Input files
			frogAddFile( files, NULL, presetprefix + *child->text )

		case ASTCLASS_DIR
			'' Input files from directories
			dim as TLIST list
			listInit( @list, sizeof( string ) )

			hScanDirectory( presetprefix + *child->text, "*.h", @list )

			dim as string ptr s = listGetHead( @list )
			while( s )
				frogAddFile( files, NULL, *s )
				*s = ""
				s = listGetNext( s )
			wend

			listEnd( @list )

		end select

		child = child->next
	wend

	if( files->head = NULL ) then
		if( len( presetfilename ) > 0 ) then
			if( targetversion ) then
				oops( "no input files for version " + emitAst( targetversion ) + " in " + presetfilename )
			else
				oops( "no input files for " + presetfilename )
			end if
		else
			oops( "no input files" )
		end if
	end if

	dim as ASTNODE ptr f

	if( (pre->options and PRESETOPT_NOMERGE) = 0 ) then
		print "preparsing to determine #include dependencies..."

		'' Preparse to find #includes and calculate refcounts
		'' Files newly registered by the inner loop will eventually be worked
		'' off by the outer loop, as they're appended to the files list.
		f = files->head
		while( f )
			if( (f->attrib and ASTATTRIB_MISSING) = 0 ) then
				print "preparsing: " + *f->comment

				tkInit( )
				lexLoadFile( 0, f, FALSE, FALSE )

				ppDirectives1( )

				'' Find #include directives
				var x = 0
				while( tkGet( x ) <> TK_EOF )

					if( tkGet( x ) = TK_PPINCLUDE ) then
						var incfile = tkGetText( x )

						var report = "    #include: " + *incfile
						if( verbose ) then
							print report
						end if

						var location = tkGetLocation( x )
						var contextf = location->file
						if( contextf = NULL ) then
							contextf = f
						end if
						var incf = frogAddFile( files, contextf, incfile )
						incf->refcount += 1

						if( verbose = FALSE ) then
							if( incf->attrib and ASTATTRIB_MISSING ) then
								report += " (not found)"
							end if
							print report
						end if
					end if

					x += 1
				wend

				tkEnd( )
			end if

			f = f->next
		wend

		if( verbose ) then
			print "#include refcounts (how often #included):"
			f = files->head
			while( f )
				print "    " & f->refcount, *f->comment
				f = f->next
			wend
		end if

		'' Pass 1: Process any files that don't look like they'll be
		'' merged, i.e. refcount <> 1.
		f = files->head
		while( f )

			if( ((f->attrib and ASTATTRIB_MISSING) = 0) and (f->refcount <> 1) ) then
				assert( f->mergeparent = NULL )
				frogWorkFile( pre, presetcode, targetversion, files, f )
			end if

			f = f->next
		wend

		'' Pass 2: Process files that looked like they should be merged,
		'' but weren't. This happens with recursive #includes, where all
		'' have refcount > 0, so none of them were merged in anywhere
		'' during the 1st pass.
		f = files->head
		while( f )

			if( ((f->attrib and ASTATTRIB_MISSING) = 0) and (f->refcount = 1) and (f->mergeparent = NULL) ) then
				frogWorkFile( pre, presetcode, targetversion, files, f )
			end if

			f = f->next
		wend

		'' Concatenate files with refcount=0
		dim as ASTNODE ptr first
		f = files->head
		while( f )
			if( f->refcount = 0 ) then
				if( first ) then
					'' Already have a first; append to it
					if( f->expr ) then
						print "concatenating: " + *f->comment
						astAppend( first->expr, f->expr )
						f->expr = NULL
					end if
				else
					'' This is the first
					first = f
					print "concatenating: " + *f->comment + " (first)"
				end if
			end if
			f = f->next
		wend
	else
		'' No merging requested, just process each file that was found
		'' individually.
		f = files->head
		while( f )
			if( (f->attrib and ASTATTRIB_MISSING) = 0 ) then
				frogWorkFile( pre, presetcode, targetversion, files, f )
			end if
			f = f->next
		wend
	end if

	function = files
end function

private sub hPrintPresetVersions( byval versions as ASTNODE ptr, byval targets as integer )
	dim s as string

	var i = versions->head
	while( i )
		s += emitAst( i ) + ", "
		i = i->next
	wend
	s = left( s, len( s ) - 2 )
	if( len( s ) = 0 ) then
		s = "none specified"
	end if

	print "versions: " + s

	s = ""
	if( targets and ASTATTRIB_DOS   ) then s += "dos, "
	if( targets and ASTATTRIB_LINUX ) then s += "linux, "
	if( targets and ASTATTRIB_WIN32 ) then s += "win32, "
	s = left( s, len( s ) - 2 )

	print "targets: " + s
end sub

private sub frogWorkPreset _
	( _
		byval pre as FROGPRESET ptr, _
		byref presetfilename as string, _
		byref presetprefix as string _
	)

	dim as ASTNODE ptr files, versions, targetversions
	dim as integer targets

	versions = astCollectVersions( pre->code )
	targets = astCollectTargets( pre->code )

	'' If no targets given, assume all
	if( targets = 0 ) then
		targets = ASTATTRIB__ALLTARGET
	end if

	if( verbose ) then
		hPrintPresetVersions( versions, targets )
	end if

	targetversions = astCombineVersionsAndTargets( versions, targets )

	'' Multiple versions specified?
	if( targetversions->head <> targetversions->tail ) then
		var targetversion = targetversions->head

		'' For each version...
		do
			print "version: " + astDumpInline( targetversion )

			'' Determine preset code for that version
			var presetcode = astGet1VersionAndTargetOnly( pre->code, targetversion )

			'' Parse files for this version and combine them with files
			'' from previous versions if possible
			files = astMergeFiles( files, _
				frogWorkVersion( pre, targetversion, presetcode, presetfilename, presetprefix ) )

			astDelete( presetcode )

			targetversion = targetversion->next
		loop while( targetversion )

		var versiondefine = "__" + ucase( pathStripExt( pathStrip( presetfilename ) ), 1 ) + "_VERSION__"

		astProcessVerblocksOnFiles( files, versions, targets, versiondefine )
	else
		if( verbose ) then
			if( targetversions->head ) then
				print "just a single version specified, doing a single pass"
			else
				print "no version(s) specified, just doing a single pass"
			end if
		end if
		'' Just do a single pass, don't worry about version specifics or AST merging
		files = frogWorkVersion( pre, NULL, pre->code, presetfilename, presetprefix )
	end if

	'' Remove files that don't have an AST left, i.e. were combined into
	'' others and shouldn't be emitted individually anymore.
	var f = files->head
	while( f )
		if( f->expr = NULL ) then
			f = astRemove( files, f )
		else
			f = f->next
		end if
	wend

	'' There should always be at least 1 file left to emit
	assert( files->head )

	'' Normally, each .bi file should be generated just next to the
	'' corresponding .h file (or one of them in case of merging...).
	'' In that case no directories need to be created.
	''
	'' If an output directory was given, then all .bi files should be
	'' emitted there. This might require creating some sub-directories to
	'' preserve the original directory structure.
	''     commonparent/a.bi
	''     commonparent/foo/a.bi
	''     commonparent/foo/b.bi
	'' shouldn't just become:
	''     outdir/a.bi
	''     outdir/a.bi
	''     outdir/b.bi
	'' because a.bi would be overwritten and the foo/ sub-directory would
	'' be lost. Instead, it should be:
	''     outdir/a.bi
	''     outdir/foo/a.bi
	''     outdir/foo/b.bi
	if( pre->outdir ) then
		var f = files->head
		var commonparent = pathOnly( *f->text )
		f = f->next
		while( f )
			commonparent = pathFindCommonBase( commonparent, *f->text )
			f = f->next
		wend
		print "common parent: " + commonparent

		f = files->head
		do
			'' Remove the common parent prefix
			var normed = *f->text
			assert( strStartsWith( normed, commonparent ) )
			normed = right( normed, len( normed ) - len( commonparent ) )

			'' And prefix the output dir instead
			normed = pathAddDiv( *pre->outdir ) + normed

			astSetText( f, normed )
			astSetComment( f, normed )

			f = f->next
		loop while( f )
	end if

	f = files->head
	do
		'' Replace the file extension, .h -> .bi
		astSetText( f, pathStripExt( *f->text ) + ".bi" )
		astSetComment( f, pathStripExt( *f->comment ) + ".bi" )
		f = f->next
	loop while( f )

	f = files->head
	do
		print "emitting: " + *f->comment
		'astDump( f->expr )
		emitFile( *f->text, f->expr )
		f = f->next
	loop while( f )

	astDelete( files )
	astDelete( versions )
	astDelete( targetversions )

end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	if( __FB_ARGC__ = 1 ) then
		hPrintHelp( "" )
	end if

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
			case "w", "whitespace"
				cmdline.options or= PRESETOPT_WHITESPACE
			case "m", "merge"
				cmdline.options and= not PRESETOPT_NOMERGE
			case "o", "outdir"
				i += 1
				if( i >= __FB_ARGC__ ) then
					hPrintHelp( "missing output directory path for -o" )
				end if
				cmdline.outdir = strDuplicate( __FB_ARGV__[i] )
			case "v", "verbose"
				verbose = TRUE
			case else
				hPrintHelp( "unknown option: " + *__FB_ARGV__[i] )
			end select
		else
			if( hDirExists( arg ) ) then
				'' Search input directory for *.fbfrog files, if none found,
				'' add it for *.h search later
				dim as TLIST list
				listInit( @list, sizeof( string ) )

				hScanDirectory( arg, "*.fbfrog", @list )

				dim as string ptr s = listGetHead( @list )
				if( s ) then
					do
						astAppend( presetfiles, astNewTEXT( *s ) )
						*s = ""
						s = listGetNext( s )
					loop while( s )
				else
					astAppend( cmdline.code, astNew( ASTCLASS_DIR, arg ) )
				end if
				listEnd( @list )
			elseif( pathExtOnly( arg ) = "fbfrog" ) then
				astAppend( presetfiles, astNewTEXT( arg ) )
			else
				astAppend( cmdline.code, astNew( ASTCLASS_FILE, arg ) )
			end if
		end if
	next

	'' If *.fbfrog files were given, work them off one by one
	var presetfile = presetfiles->head
	if( presetfile ) then
		'' For each *.fbfrog file...
		do
			var presetfilename = *presetfile->text
			print "* " + presetfilename

			'' Read in the *.fbfrog preset
			dim as FROGPRESET pre
			presetInit( @pre )
			presetParse( @pre, presetfilename )

			'' The preset's input files are given relative to the preset's directory
			var presetdir = pathAddDiv( pathOnly( presetfilename ) )

			'' Allow overriding the preset's input files with those
			'' from the command line
			if( presetHasInputFiles( @cmdline ) ) then
				presetOverrideInputFiles( @pre, @cmdline )
				'' Also, then we'll rely on the paths from command line,
				'' and no preset specific directory can be prefixed.
				presetdir = ""
			end if

			frogWorkPreset( @pre, presetfilename, presetdir )
			presetEnd( @pre )

			presetfile = presetfile->next
		loop while( presetfile )
	else
		'' Otherwise just work off the input files and options given
		'' on the command line, it's basically a "preset" too, just not
		'' stored in a *.fbfrog file but given through command line
		'' options.
		frogWorkPreset( @cmdline, "", "" )
	end if

	if( verbose ) then
		astPrintStats( )
	end if
