#include "hs_tools.hpp"
#include "string.h"

using namespace std;

void * get_null_ptr(void){
    return 0;
}

vector<void*> * hs_marshall_vec(void * inputs[], int length){
    vector<void*> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<void*>(vec);
}
void hs_delete_vec(vector<void*> * vec){ delete vec; }

int vec_size(vector<int> * vec){
    return vec->size();
}
void hs_unmarshall_vec(vector<int> * vec, int outputs[]){
    memcpy(outputs, &(vec[0]), vec->size()*sizeof(int));
}
