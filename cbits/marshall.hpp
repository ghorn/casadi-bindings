#ifndef __MARSHALL_THEM_BINDINGS_H__
#define __MARSHALL_THEM_BINDINGS_H__

#include <iostream>
#include <casadi.hpp>

void * get_void_ptr(void);

std::string marshall(char * x);
int marshall(int x);
double marshall(double x);
void foo(const double * const x);
std::vector<CasADi::SXMatrix> marshall(CasADi::SXMatrix * const * inputs);
std::vector<CasADi::SXMatrix> marshall(CasADi::SXMatrix * const & inputs);


#endif // __MARSHALL_THEM_BINDINGS_H__
