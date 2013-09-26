#ifndef __MAH_HS_TOOLS_H__
#define __MAH_HS_TOOLS_H__

#include <iostream>
#include <vector>
//#include <casadi.hpp>

void * get_null_ptr(void);

std::vector<void*> * hs_marshal_vec(void * inputs[], int length);
void hs_delete_vec(std::vector<void*> * vec);
int vec_size(std::vector<int> * vec);
void hs_unmarshal_vec(std::vector<int> * vec, int outputs[]);


#endif // __MAH_HS_TOOLS_H__
