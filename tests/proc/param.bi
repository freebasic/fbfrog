declare sub f( byval a as long )
declare sub f( byval a as long, byval b as long )
declare sub f( byval a as long, byval b as long, byval c as long )
declare sub f( byval a as long ptr, byval b as long ptr ptr ptr )

declare sub f( byval as long )
declare sub f( byval as long ptr, byval as long, byval as long ptr ptr ptr, byval as long )

declare sub f( byval i as long = 123 )
declare sub f( byval a as long, byval b as long = 123 )
declare sub f( byval a as long = 123, byval b as long )
declare sub f( byval a as long, byval b as long = 123, byval c as long )
declare sub f( byval a as long = 1, byval b as long = 2, byval c as long = 3 )

declare sub f( byval i as long ptr )
declare sub f( byval a as long, byval b as long ptr )
declare sub f( byval a as long ptr, byval b as long )
declare sub f( byval a as long, byval b as long ptr, byval c as long )
declare sub f( byval a as long ptr, byval b as long ptr, byval c as long ptr )
