#include "marshall.hpp"
#include "string.h"

using namespace std;

string marshall(char * x){ return string(x); }
int marshall(int x){ return x; }
double marshall(double x){ return x; }
const CasADi::MX& marshall(const CasADi::MX& x){ return x; }

vector<CasADi::SXMatrix> marshall(vector<CasADi::SXMatrix*> const & inputs){
    vector<CasADi::SXMatrix> vec;
    for (unsigned int k=0; k<inputs.size(); k++){
        vec.push_back( *(inputs[k]) );
    }
    return vec;
}


