#include <iostream>
#include <symbolic/casadi.hpp>
#include <symbolic/fx/nlp_solver.hpp>
#include <symbolic/fx/c_function.hpp>

using namespace std;
using namespace CasADi;

extern "C" {
typedef void (*hs_callback_t)(CFunction &f, int nfdir, int nadir);
}


void dummy_wrapper(CFunction &f, int nfdir, int nadir, void * user_data){
    hs_callback_t hs_callback = reinterpret_cast<hs_callback_t>(user_data);
    hs_callback(f, nfdir, nadir);
}


extern "C" CFunction * new_cfunction_with_callback(hs_callback_t hs_callback);
CFunction * new_cfunction_with_callback(hs_callback_t hs_callback){
    CFunction * cfun = new CFunction(dummy_wrapper);
    cfun->setOption("user_data", reinterpret_cast<void*>(hs_callback));
    return cfun;
}

extern "C"
void add_iteration_callback(const vector<CRSSparsity*> &inputschemeP,
                            const vector<CRSSparsity*> &outputschemeP,
                            hs_callback_t hs_callback, NLPSolver &nlp );
void add_iteration_callback(const vector<CRSSparsity*> &inputschemeP,
                            const vector<CRSSparsity*> &outputschemeP,
                            hs_callback_t hs_callback, NLPSolver &nlp ){
    std::vector< CRSSparsity > inputscheme;
    for (unsigned int k=0; k<inputschemeP.size(); k++){
        inputscheme.push_back( *(inputschemeP[k]) );
    }
    std::vector< CRSSparsity > outputscheme;
    for (unsigned int k=0; k<outputschemeP.size(); k++){
        outputscheme.push_back( *(outputschemeP[k]) );
    }

    CFunction cfun = CFunction(dummy_wrapper, inputscheme, outputscheme);
    cfun.setOption("user_data", reinterpret_cast<void*>(hs_callback));
    cfun.init();
    nlp.setOption("iteration_callback", cfun);
}

//  typedef void (*CFunctionWrapper)(CFunction &f, int nfdir, int nadir, void* user_data);
//  explicit CFunction(CFunctionWrapper c_fcn);
