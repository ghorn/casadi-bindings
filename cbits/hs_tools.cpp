#include "hs_tools.hpp"
#include "string.h"

using namespace std;

void * get_null_ptr(void){
    return 0;
}

vector<void*> * hs_marshal_vec_void_ptrs(void * inputs[], int length){
    vector<void*> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<void*>(vec);
}
void hs_delete_vec_void_ptrs(vector<void*> * vec){ delete vec; }

vector<unsigned char> * hs_marshal_vec_uchar(unsigned char inputs[], int length){
    vector<unsigned char> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<unsigned char>(vec);
}
void hs_delete_vec_uchar(vector<unsigned char> * vec){ delete vec; }

vector<double> * hs_marshal_vec_double(double inputs[], int length){
    vector<double> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<double>(vec);
}
void hs_delete_vec_double(vector<double> * vec){ delete vec; }

vector<int> * hs_marshal_vec_int(int inputs[], int length){
    vector<int> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<int>(vec);
}
void hs_delete_vec_int(vector<int> * vec){ delete vec; }

vector<size_t> * hs_marshal_vec_size_t(size_t inputs[], int length){
    vector<size_t> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<size_t>(vec);
}
void hs_delete_vec_size_t(vector<size_t> * vec){ delete vec; }



int vec_size(vector<int> * vec){
    return vec->size();
}
void hs_unmarshal_vec(vector<int> * vec, int outputs[]){
    memcpy(outputs, &(vec[0]), vec->size()*sizeof(int));
}
