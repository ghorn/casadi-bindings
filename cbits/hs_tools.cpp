#include "hs_tools.hpp"
#include "string.h"

using namespace std;

void * get_null_ptr(void){
    return 0;
}

int hs_read_bool(bool * x){
    if (*x == false)
        return 0;
    else
        return 1;
}
bool * hs_marshall_bool(int x){
    if (x == 0)
        return new bool(false);
    else
        return new bool(true);
}
void hs_delete_bool(bool * x){ delete x; }


// copying vectors to arrays
int vec_int_size(vector<int> * vec){
    return vec->size();
}
void vec_int_copy(vector<int> * vec, int outputs[]){
    memcpy(outputs, &(vec[0]), vec->size()*sizeof(int));
}

// void pointers
int vec_voidp_size(vector<void*> * vec){
    return vec->size();
}
void vec_voidp_copy(vector<void*> * vec, void* outputs[]){
    memcpy(outputs, &(vec[0]), vec->size()*sizeof(void*));
}

// 2d void pointers
int vec_vec_voidp_size(vector<vector<void*> > * vec){
    return vec->size();
}
void vec_vec_voidp_sizes(vector<vector<void*> > * vec, int sizes[]){
    for (int k=0; k<vec->size(); k++){
        sizes[k] = (*vec)[k].size();
    }
}
void vec_vec_voidp_copy(vector<vector<void*> > * vec, void** outputs[]){
    for (int k=0; k<vec->size(); k++){
        memcpy(outputs[k], &(vec[k][0]), vec[k].size()*sizeof(void*));
    }
}

int string_length(string * str){
    return str->length();
}
void string_copy(string * str, char outputs[]){
    strcpy(outputs, str->c_str());
}
string * new_string(char x[]){
    return new string(x);
}
void delete_string(string * x){
    delete x;
}

// converting arrays to vectors
// void pointers
vector<void*> * hs_marshal_vec_void_ptrs(void * inputs[], int length){
    vector<void*> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<void*>(vec);
}
void hs_delete_vec_void_ptrs(vector<void*> * vec){ delete vec; }

// 2-dimensional void pointers
vector<vector<void*> > * hs_marshal_vec_vec_void_ptrs(void * inputs[], int length_outer, int lengths_inner[]){
    vector<vector<void*> > vec;
    int counter = 0;
    for (int k=0; k<length_outer; k++){
        vector<void*> inner;
        for (int j=0; j<lengths_inner[k]; j++){
            inner.push_back( inputs[counter] );
            counter++;
        }
        vec.push_back(inner);
    }
    return new vector<vector<void*> >(vec);
}

void hs_delete_vec_vec_void_ptrs(vector<vector<void*> > * vec){ delete vec; }


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
