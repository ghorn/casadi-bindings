#include "marshall.hpp"

using namespace std;

void * get_null_ptr(void){
    return 0;
}

string marshall(char * x){ return string(x); }
int marshall(int x){ return x; }
double marshall(double x){ return x; }

vector<CasADi::SXMatrix> marshall(vector<CasADi::SXMatrix*> const & inputs){
    vector<CasADi::SXMatrix> vec;
    for (int k=0; k<inputs.size(); k++){
        vec.push_back( *(inputs[k]) );
    }
    return vec;
}


vector<void*> * hs_marshall_vec(void * inputs[], int length){
    vector<void*> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<void*>(vec);
}
void hs_delete_vec(vector<void*> * vec){ delete vec; }

