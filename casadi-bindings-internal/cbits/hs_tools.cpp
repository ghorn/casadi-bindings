#include <iostream>
#include <vector>
#include "string.h"

using namespace std;

///////////// scalars /////////////
// string
extern "C" int hs_length_string(string * str);
int hs_length_string(string * str){
    return str->length();
}
extern "C" void hs_copy_string(string * str, char outputs[]);
void hs_copy_string(string * str, char outputs[]){
    strcpy(outputs, str->c_str());
}
extern "C" string * hs_new_string(char x[]);
string * hs_new_string(char x[]){
    return new string(x);
}
extern "C" void hs_delete_string(string * x);
void hs_delete_string(string * x){
    delete x;
}


////////////////////////// copying vectors to arrays /////////////////////
template <typename T>
void hs_copy_vec_T(vector<T> * vec, T outputs[]){
    memcpy(outputs, &((*vec)[0]), vec->size()*sizeof(T));
}

//////////////////////    CREATING VECTORS FROM ARRAYS ///////////////////////////////////
// 1-dimensional
template <typename T>
vector<T> * hs_new_vec_T(T inputs[], int length){
    vector<T> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<T>(vec);
}

////////////////////////////////////////////////////////////////////////////////////////////
#define WRITE_STUFF(name, type) \
    extern "C" vector<type>* hs_new_vec_##name( type inputs[], int length); \
    vector<type>* hs_new_vec_##name( type inputs[], int length){return hs_new_vec_T(inputs, length);} \
    extern "C" void hs_delete_vec_##name(vector<type> * vec); \
    void hs_delete_vec_##name(vector<type> * vec){ delete vec; } \
    extern "C" void hs_copy_vec_##name(vector<type> * vec, type outputs[]); \
    void hs_copy_vec_##name(vector<type> * vec, type outputs[]){hs_copy_vec_T(vec, outputs);} \
    extern "C" int hs_size_vec_##name(vector<type> * vec); \
    int hs_size_vec_##name(vector<type> * vec){ return vec->size(); }

WRITE_STUFF(int,int)
WRITE_STUFF(voidp, void*)
WRITE_STUFF(uchar, unsigned char)
WRITE_STUFF(double, double)
