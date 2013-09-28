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
template <typename T>
void vec_copy(vector<T> * vec, T outputs[]){
    memcpy(outputs, &((*vec)[0]), vec->size()*sizeof(T));
}
void vec_copy_double(vector<double> * vec, double outputs[]){
    vec_copy(vec, outputs);
}
void vec_copy_voidp(vector<void*> * vec, void* outputs[]){
    vec_copy(vec, outputs);
}

int vec_size_voidp(vector<void*> * vec){ return vec->size(); }
int vec_size_int(vector<int> * vec){ return vec->size(); }


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
template <typename T>
vector<T> * hs_new_vec_T(T inputs[], int length){
    vector<T> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<T>(vec);
}

// 2-dimensional
template <typename T>
vector<vector<T> > * hs_new_vec_vec_T(T inputs[], int length_outer, int lengths_inner[]){
    vector<vector<T> > vec;
    int counter = 0;
    for (int k=0; k<length_outer; k++){
        vector<T> inner;
        for (int j=0; j<lengths_inner[k]; j++){
            inner.push_back( inputs[counter] );
            counter++;
        }
        vec.push_back(inner);
    }
    return new vector<vector<T> >(vec);
}
#define WRITE_STUFF(name, type) \
    vector<type>* hs_new_vec_##name( type inputs[], int length){return hs_new_vec_T(inputs, length);} \
    void hs_delete_vec_##name(vector<type> * vec){ delete vec; } \
    vector<vector<type> >   * hs_new_vec_vec_##name(type x[], int y, int z[]){ return hs_new_vec_vec_T(x,y,z); } \
    void hs_delete_vec_vec_##name(vector<vector<type> >  * vec){ delete vec; } \

WRITE_STUFF(int,int)
WRITE_STUFF(voidp, void*)
WRITE_STUFF(uchar, unsigned char)
WRITE_STUFF(double, double)
