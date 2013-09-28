#ifndef __MAH_HS_TOOLS_H__
#define __MAH_HS_TOOLS_H__

#include <iostream>
#include <vector>
//#include <casadi.hpp>

void * get_null_ptr(void);

///////// scalars ///////////
// bool
int hs_read_bool(bool * x);
bool * hs_new_bool(int x);
void hs_delete_bool(bool * x);

// string
int hs_string_length(std::string * str);
void hs_string_copy(std::string * str, char outputs[]);
std::string * hs_new_string(char x[]);
void hs_delete_string(std::string * x);



////////////////////////// copying vectors to arrays /////////////////////
int vec_int_size(std::vector<int> * vec);
void vec_int_copy(std::vector<int> * vec, int outputs[]);


////////////////////////    CREATING VECTORS FROM ARRAYS ///////////////////////////////////
//// 1-d
//std::vector<void*> * hs_new_vec_voidp(void * inputs[], int length);
//std::vector<int8_t> * hs_new_vec_8(int8_t inputs[], int length);
//std::vector<int16_t> * hs_new_vec_16(int16_t inputs[], int length);
//std::vector<int32_t> * hs_new_vec_32(int32_t inputs[], int length);
//std::vector<int64_t> * hs_new_vec_64(int64_t inputs[], int length);
//
//void hs_delete_vec_voidp(std::vector<void*> * vec);
//void hs_delete_vec_8(std::vector< int8_t> * vec);
//void hs_delete_vec_16(std::vector<int16_t> * vec);
//void hs_delete_vec_32(std::vector<int32_t> * vec);
//void hs_delete_vec_64(std::vector<int64_t> * vec);
//
//// 2-d
//std::vector<std::vector<void*> > * hs_new_vvec_voidp(voidint8_t inputs[], int length_outer, int lengths_inner[]);
//std::vector<std::vector<int8_t> > * hs_new_vvec_8(int8_t inputs[], int length_outer, int lengths_inner[]);
//std::vector<std::vector<int16_t> > * hs_new_vvec_16(int8_t inputs[], int length_outer, int lengths_inner[]);
//std::vector<std::vector<int32_t> > * hs_new_vvec_32(int8_t inputs[], int length_outer, int lengths_inner[]);
//std::vector<std::vector<int64_t> > * hs_new_vvec_64(int8_t inputs[], int length_outer, int lengths_inner[]);
//
//void hs_delete_vvec_voidp(std::vector<std::vector<void*> > * vec);
//void hs_delete_vvec_8(std::vector<std::vector<int8_t> > * vec);
//void hs_delete_vvec_16(std::vector<std::vector<int16_t> > * vec);
//void hs_delete_vvec_32(std::vector<std::vector<int32_t> > * vec);
//void hs_delete_vvec_64(std::vector<std::vector<int64_t> > * vec);


#endif // __MAH_HS_TOOLS_H__
