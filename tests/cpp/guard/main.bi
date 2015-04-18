#pragma once

extern "C"

#define HAD_A
declare sub inside_a()
#define HAD_B
declare sub inside_b()
declare sub outside_c()
#define HAD_C
declare sub inside_c()
declare sub outside_c()
#define HAD_D
declare sub inside_d()
declare sub outside_d()
declare sub outside_d()
#define HAD_E
declare sub inside_e_1()
declare sub inside_e_2()
#define HAD_F
declare sub inside_f_1()
declare sub inside_f_2()
#define HAD_G_1
#define HAD_G_2
#undef HAD_G_2
#define HAD_G_2

end extern
