type T
	/' TODO: token 93 '/ *(*p)(/' TODO: token 93 '/*)/' TODO: token 44 '/
end type

/' TODO: token 109 '/ f(/' TODO: token 109 '/ (*foo)())/' TODO: token 44 '/

/' sub ptr '/
/' TODO: token 109 '/ (*fp01)()/' TODO: token 44 '/

/' function pointer, with function pointer param '/
double *(*fp02)(/' TODO: token 93 '/ ***(*)(/' TODO: token 72 '/**))/' TODO: token 44 '/
