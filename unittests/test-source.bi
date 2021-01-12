'' No sources at all
scope
	dim ctx as SourceContext

	scope
		var decoded = ctx.decode(type<TkLocation>(0))
		TESTEQ(decoded.source, NULL)
		TESTEQ(decoded.linenum, -1)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(1))
		TESTEQ(decoded.source, NULL)
		TESTEQ(decoded.linenum, -1)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(2))
		TESTEQ(decoded.source, NULL)
		TESTEQ(decoded.linenum, -1)
	end scope
end scope

'' Empty sources only
scope
	dim ctx as SourceContext
	ctx.addInternalSource("title", "")
	ctx.addInternalSource("title", "")
	ctx.addInternalSource("title", "")
	ctx.addInternalSource("title", "")

	scope
		var decoded = ctx.decode(type<TkLocation>(0))
		TESTEQ(decoded.source, NULL)
		TESTEQ(decoded.linenum, -1)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(1))
		TESTEQ(decoded.source, NULL)
		TESTEQ(decoded.linenum, -1)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(2))
		TESTEQ(decoded.source, NULL)
		TESTEQ(decoded.linenum, -1)
	end scope
end scope

'' Mixed empty/non-empty sources
scope
	dim ctx as SourceContext
	var source1 = ctx.addInternalSource("title", "a")
	var source2 = ctx.addInternalSource("title", "")
	var source3 = ctx.addInternalSource("title", "b")
	var source4 = ctx.addInternalSource("title", "")

	scope
		var decoded = ctx.decode(type<TkLocation>(0))
		TESTEQ(decoded.source, NULL)
		TESTEQ(decoded.linenum, -1)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(1))
		TESTEQ(decoded.source, source1)
		TESTEQ(decoded.linenum, 1)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(2))
		TESTEQ(decoded.source, source3)
		TESTEQ(decoded.linenum, 1)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(3))
		TESTEQ(decoded.source, NULL)
		TESTEQ(decoded.linenum, -1)
	end scope
end scope

'' Normal usage
scope
	dim ctx as SourceContext

	var source1 = ctx.addInternalSource("title", !"a\nb\nc\n")
	TESTEQ(source1->linecount, 3)
	TESTEQ(ctx.encode(type<DecodedLocation>(source1, 1)).value, 1)
	TESTEQ(ctx.encode(type<DecodedLocation>(source1, 2)).value, 2)
	TESTEQ(ctx.encode(type<DecodedLocation>(source1, 3)).value, 3)

	var source2 = ctx.addInternalSource("title", "1")
	TESTEQ(source2->linecount, 1)
	TESTEQ(ctx.encode(type<DecodedLocation>(source2, 1)).value, 4)

	var source3 = ctx.addInternalSource("title", !"111\r\n2\r\n3\r\n4")
	TESTEQ(source3->linecount, 4)
	TESTEQ(ctx.encode(type<DecodedLocation>(source3, 1)).value, 5)
	TESTEQ(ctx.encode(type<DecodedLocation>(source3, 2)).value, 6)
	TESTEQ(ctx.encode(type<DecodedLocation>(source3, 3)).value, 7)
	TESTEQ(ctx.encode(type<DecodedLocation>(source3, 4)).value, 8)

	'' Invalid TkLocation value
	scope
		var decoded = ctx.decode(type<TkLocation>(0))
		TESTEQ(decoded.source, NULL)
		TESTEQ(decoded.linenum, -1)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(1))
		TESTEQ(decoded.source, source1)
		TESTEQ(decoded.linenum, 1)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(2))
		TESTEQ(decoded.source, source1)
		TESTEQ(decoded.linenum, 2)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(3))
		TESTEQ(decoded.source, source1)
		TESTEQ(decoded.linenum, 3)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(4))
		TESTEQ(decoded.source, source2)
		TESTEQ(decoded.linenum, 1)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(5))
		TESTEQ(decoded.source, source3)
		TESTEQ(decoded.linenum, 1)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(6))
		TESTEQ(decoded.source, source3)
		TESTEQ(decoded.linenum, 2)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(7))
		TESTEQ(decoded.source, source3)
		TESTEQ(decoded.linenum, 3)
	end scope

	scope
		var decoded = ctx.decode(type<TkLocation>(8))
		TESTEQ(decoded.source, source3)
		TESTEQ(decoded.linenum, 4)
	end scope

	'' Out-of-bounds TkLocation value
	scope
		var decoded = ctx.decode(type<TkLocation>(9))
		TESTEQ(decoded.source, NULL)
		TESTEQ(decoded.linenum, -1)
	end scope
end scope
