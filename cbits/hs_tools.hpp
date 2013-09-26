#ifndef __MAH_HS_TOOLS_H__
#define __MAH_HS_TOOLS_H__

#include <iostream>
#include <vector>
//#include <casadi.hpp>

void * get_null_ptr(void);

int hs_read_bool(bool * x);
bool * hs_marshall_bool(int x);
void hs_delete_bool(bool * x);


// copying vectors to arrays
int vec_int_size(std::vector<int> * vec);
void vec_int_copy(std::vector<int> * vec, int outputs[]);
int string_length(std::string * str);
void string_copy(std::string * str, char outputs[]);


// converting arrays to vectors
std::vector<void*> * hs_marshal_vec_void_ptrs(void * inputs[], int length);
void hs_delete_vec_void_ptrs(std::vector<void*> * vec);

std::vector<unsigned char> * hs_marshal_vec_uchar(unsigned char inputs[], int length);
void hs_delete_vec_uchar(std::vector<unsigned char> * vec);

std::vector<double> * hs_marshal_vec_double(double inputs[], int length);
void hs_delete_vec_double(std::vector<double> * vec);

std::vector<int> * hs_marshal_vec_int(int inputs[], int length);
void hs_delete_vec_int(std::vector<int> * vec);

std::vector<size_t> * hs_marshal_vec_size_t(size_t inputs[], int length);
void hs_delete_vec_size_t(std::vector<size_t> * vec);



#endif // __MAH_HS_TOOLS_H__
