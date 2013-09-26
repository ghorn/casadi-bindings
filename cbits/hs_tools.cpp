#include "hs_tools.hpp"
#include "string.h"

using namespace std;

void * get_null_ptr(void){
    return 0;
}

///////////// scalars /////////////
// bool
int hs_read_bool(bool * x){
    if (*x == false)
        return 0;
    else
        return 1;
}
bool * hs_new_bool(int x){
    if (x == 0)
        return new bool(false);
    else
        return new bool(true);
}
void hs_delete_bool(bool * x){ delete x; }

// string
int hs_string_length(string * str){
    return str->length();
}
void hs_string_copy(string * str, char outputs[]){
    strcpy(outputs, str->c_str());
}
string * hs_new_string(char x[]){
    return new string(x);
}
void hs_delete_string(string * x){
    delete x;
}


////////////////////////// copying vectors to arrays /////////////////////
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


//////////////////////    CREATING VECTORS FROM ARRAYS ///////////////////////////////////
// 1-dimensional
vector<void*> * hs_new_vec_voidp(void * inputs[], int length){
    vector<void*> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<void*>(vec);
}
vector<int8_t> * hs_new_vec_8(int8_t inputs[], int length){
    vector<int8_t> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<int8_t>(vec);
}
vector<int16_t> * hs_new_vec_16(int16_t inputs[], int length){
    vector<int16_t> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<int16_t>(vec);
}
vector<int32_t> * hs_new_vec_32(int32_t inputs[], int length){
    vector<int32_t> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<int32_t>(vec);
}
vector<int64_t> * hs_new_vec_64(int64_t inputs[], int length){
    vector<int64_t> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<int64_t>(vec);
}

void hs_delete_vec_voidp(vector<void*> * vec){ delete vec; }
void hs_delete_vec_8(vector<int8_t> * vec){ delete vec; }
void hs_delete_vec_16(vector<int16_t> * vec){ delete vec; }
void hs_delete_vec_32(vector<int32_t> * vec){ delete vec; }
void hs_delete_vec_64(vector<int64_t> * vec){ delete vec; }


// 2-dimensional
vector<vector<void*> > * hs_new_vvec_voidp(void * inputs[], int length_outer, int lengths_inner[]){
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
vector<vector<int8_t> > * hs_new_vvec_8(int8_t inputs[], int length_outer, int lengths_inner[]){
    vector<vector<int8_t> > vec;
    int counter = 0;
    for (int k=0; k<length_outer; k++){
        vector<int8_t> inner;
        for (int j=0; j<lengths_inner[k]; j++){
            inner.push_back( inputs[counter] );
            counter++;
        }
        vec.push_back(inner);
    }
    return new vector<vector<int8_t> >(vec);
}
vector<vector<int16_t> > * hs_new_vvec_16(int16_t inputs[], int length_outer, int lengths_inner[]){
    vector<vector<int16_t> > vec;
    int counter = 0;
    for (int k=0; k<length_outer; k++){
        vector<int16_t> inner;
        for (int j=0; j<lengths_inner[k]; j++){
            inner.push_back( inputs[counter] );
            counter++;
        }
        vec.push_back(inner);
    }
    return new vector<vector<int16_t> >(vec);
}
vector<vector<int32_t> > * hs_new_vvec_32(int32_t inputs[], int length_outer, int lengths_inner[]){
    vector<vector<int32_t> > vec;
    int counter = 0;
    for (int k=0; k<length_outer; k++){
        vector<int32_t> inner;
        for (int j=0; j<lengths_inner[k]; j++){
            inner.push_back( inputs[counter] );
            counter++;
        }
        vec.push_back(inner);
    }
    return new vector<vector<int32_t> >(vec);
}
vector<vector<int64_t> > * hs_new_vvec_64(int64_t inputs[], int length_outer, int lengths_inner[]){
    vector<vector<int64_t> > vec;
    int counter = 0;
    for (int k=0; k<length_outer; k++){
        vector<int64_t> inner;
        for (int j=0; j<lengths_inner[k]; j++){
            inner.push_back( inputs[counter] );
            counter++;
        }
        vec.push_back(inner);
    }
    return new vector<vector<int64_t> >(vec);
}

void hs_delete_vvec_voidp(vector<vector<void*> > * vec){ delete vec; }
void hs_delete_vvec_8(vector<vector<int8_t> > * vec){ delete vec; }
void hs_delete_vvec_16(vector<vector<int16_t> > * vec){ delete vec; }
void hs_delete_vvec_32(vector<vector<int32_t> > * vec){ delete vec; }
void hs_delete_vvec_64(vector<vector<int64_t> > * vec){ delete vec; }

