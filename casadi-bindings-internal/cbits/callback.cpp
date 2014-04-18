#include <iostream>
#include "casadi/symbolic/casadi.hpp"
#include "casadi/symbolic/function/custom_function.hpp"
#include "casadi/symbolic/functor_internal.hpp"

#include "HsFFI.h"

using namespace std;
using namespace casadi;

extern "C" {
typedef int (*hs_callback_t)(Function &f);
}

namespace casadi {
  class FunctorHaskellInternal {
    public:
      FunctorHaskellInternal(hs_callback_t hscb)
      {
          hs_callback = hscb;
      }
      ~FunctorHaskellInternal() {
          // free the haskell FunPtr
          hs_free_fun_ptr(HsFunPtr(hs_callback));
      }
    protected:
      hs_callback_t hs_callback;
  };

  class CallbackHaskellInternal : public CallbackInternal, FunctorHaskellInternal {
    friend class CallbackHaskell;

      CallbackHaskellInternal(hs_callback_t hscb) : FunctorHaskellInternal(hscb) {}
    virtual int call(Function& fcn, void* user_data);
    virtual CallbackHaskellInternal* clone() const { return new CallbackHaskellInternal(hs_callback); }
  };

  class CallbackHaskell : public Callback {
    public:
      CallbackHaskell(hs_callback_t hscb) { assignNode(new CallbackHaskellInternal(hscb)); }
  };

  int CallbackHaskellInternal::call(Function& fcn, void* user_data) {
      return hs_callback(fcn);
  }
}

extern "C" CallbackHaskell * new_callback_haskell(hs_callback_t hs_callback);
CallbackHaskell * new_callback_haskell(hs_callback_t hs_callback){
    return new CallbackHaskell(hs_callback);
}

extern "C" void delete_callback_haskell(casadi::CallbackHaskell* obj);
void delete_callback_haskell(casadi::CallbackHaskell* obj){
    delete obj;
}
