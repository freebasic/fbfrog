'' Main module, command line interface

#include once "fbfrog.bi"

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

private function frogDownload( byref url as string, byref file as string ) as integer
	if( hFileExists( "tarballs/" + file ) ) then
		function = TRUE
	else
		hMkdir( "tarballs" )
		function = hShell( "wget '" + url + "' -O ""tarballs/" + file + """" )
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
		byval version as ASTNODE ptr, _
		byval files as ASTNODE ptr, _
		byval f as ASTNODE ptr _
	)

	print "parsing: " + *f->comment

	tkInit( )

	var comments = ((pre->options and PRESETOPT_COMMENTS) <> 0)
	lexLoadFile( 0, f, FALSE, comments )

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
						lexLoadFile( x, incf, FALSE, comments )
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
		astAutoExtern( ast, ((pre->options and PRESETOPT_WINDOWSMS) <> 0) )
	end if
	astMergeDIVIDERs( ast )

	'' Put file's AST into a VERBLOCK, if a version was given
	if( version ) then
		ast = astNewVERBLOCK( astClone( version ), NULL, ast )
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
		byval version as ASTNODE ptr, _
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

			hScanDirectoryForH( presetprefix + *child->text, @list )

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
			if( version ) then
				oops( "no input files for version " + astStringifyVersion( version ) + " in " + presetfilename )
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
				frogWorkFile( pre, presetcode, version, files, f )
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
				frogWorkFile( pre, presetcode, version, files, f )
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
				frogWorkFile( pre, presetcode, version, files, f )
			end if
			f = f->next
		wend
	end if

	function = files
end function

private sub frogWorkPreset _
	( _
		byval pre as FROGPRESET ptr, _
		byref presetfilename as string, _
		byref presetprefix as string _
	)

	var versions = astCollectVersions( pre->code )
	dim as ASTNODE ptr files

	'' Any versions specified?
	var version = versions->head
	if( version ) then
		'' For each version...
		do
			print "version: " + astStringifyVersion( version )

			'' Determine preset code for that version
			var presetcode = astGet1VersionOnly( pre->code, version )

			'' Parse files for this version and combine them with files
			'' from previous versions if possible
			files = astMergeFiles( files, _
				frogWorkVersion( pre, version, presetcode, presetfilename, presetprefix ) )

			astDelete( presetcode )

			version = version->next
		loop while( version )

		astRemoveFullVerBlockWrappingFromFiles( files, versions )
	else
		'' Just do a single pass, don't worry about version specifics or AST merging
		files = frogWorkVersion( pre, NULL, pre->code, presetfilename, presetprefix )
	end if

	'' Emit all files that have an AST (i.e. weren't merged into or
	'' appended to anything)
	var f = files->head
	while( f )
		if( f->expr ) then
			var binormed = pathStripExt( *f->text ) + ".bi"
			var bipretty = pathStripExt( *f->comment ) + ".bi"
			print "emitting: " + bipretty
			emitFile( binormed, f->expr )
		end if
		f = f->next
	wend

	astDelete( files )
	astDelete( versions )

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
			case "comments"
				cmdline.options or= PRESETOPT_COMMENTS
			case "m", "merge"
				cmdline.options and= not PRESETOPT_NOMERGE
			case "v", "verbose"
				verbose = TRUE
			case else
				hPrintHelp( "unknown option: " + *__FB_ARGV__[i] )
			end select
		else
			if( hDirExists( arg ) ) then
				astAppend( cmdline.code, astNew( ASTCLASS_DIR, arg ) )
			else
				select case( pathExtOnly( arg ) )
				case "h", "hh", "hxx", "hpp", "c", "cc", "cxx", "cpp"
					astAppend( cmdline.code, astNew( ASTCLASS_FILE, arg ) )
				case "fbfrog"
					astAppend( presetfiles, astNewTEXT( arg ) )
				case else
					hPrintHelp( "'" + arg + "' is not a *.h file" )
				end select
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

			'' The preset's input/output files, especially extracted
			'' tarballs, should be put into a directory specific to
			'' the preset:
			'' foo/bar.fbfrog  ->  foo/bar/
			var presetdir = pathStripExt( presetfilename )
			hMkdir( presetdir )
			presetdir += PATHDIV

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
