#include "marshall.hpp"

void * get_void_ptr(void){
    return 0;
}

std::string marshall(char * x){ return std::string(x); }
int marshall(int x){ return x; }
double marshall(double x){ return x; }

std::vector<CasADi::SXMatrix> marshall(CasADi::SXMatrix * const * inputs){
    std::vector<CasADi::SXMatrix> vec;
    for (int k=0;; k++){
        if (inputs[k] == 0)
            break;
        else
            vec.push_back(*(inputs[k]));
    }
    return vec;
}

std::vector<CasADi::SXMatrix> marshall(CasADi::SXMatrix * const & inputs){
    std::vector<CasADi::SXMatrix> vec;
    for (int k=0;; k++){
        if ((&inputs)[k] == 0)
            break;
        else
            vec.push_back(*((&inputs)[k]));
    }
    return vec;
}
