#ifndef __MARSHALL_THEM_BINDINGS_H__
#define __MARSHALL_THEM_BINDINGS_H__

#include <iostream>
#include <casadi.hpp>

void * get_null_ptr(void);

std::string marshall(char * x);
int marshall(int x);
double marshall(double x);
std::vector<CasADi::SXMatrix> marshall(std::vector<CasADi::SXMatrix*> const & inputs);

std::vector<void*> * hs_marshall_vec(void * inputs[], int length);
void hs_delete_vec(std::vector<void*> * vec);


#endif // __MARSHALL_THEM_BINDINGS_H__
