#include <iostream>
#include "casadi/core/core.hpp"
#include "casadi/core/function/custom_function.hpp"
#include "casadi/core/functor_internal.hpp"

#include "HsFFI.h"

// #define DEBUG_WOO

using namespace std;
using namespace casadi;

extern "C" {
    //todo: use casadi typedefs in functor.hpp
    typedef int (*hs_callback_t)(Function &f);
    typedef void (*hs_custom_evaluate_t)(CustomFunction &f);
    typedef Function& (*hs_derivative_generator_t)(Function &f, int nder);
}

namespace casadi {
    // functor
    template <class T> class FunctorHaskellInternal {
    public:
        FunctorHaskellInternal(T hscb) {
            functor_fun_ptr = hscb;
        }
        ~FunctorHaskellInternal() {
            // free the haskell FunPtr
            #ifdef DEBUG_WOO
            std::cout << "functor_internal_free\n";
            #endif
            hs_free_fun_ptr(HsFunPtr(functor_fun_ptr));
            #ifdef DEBUG_WOO
            std::cout << "functor_internal_free finished\n";
            #endif
        }
    protected:
        T functor_fun_ptr;
    };


    // Callback
    class CallbackHaskellInternal :
        public CallbackInternal, FunctorHaskellInternal<hs_callback_t> {

        friend class CallbackHaskell;
        CallbackHaskellInternal(hs_callback_t hscb) : FunctorHaskellInternal<hs_callback_t>(hscb) {}
        virtual int call(Function& fcn, void* user_data);
        virtual CallbackHaskellInternal* clone() const {
            return new CallbackHaskellInternal(functor_fun_ptr);
        }
    };

    class CallbackHaskell : public Callback {
    public:
        CallbackHaskell(hs_callback_t hscb) {
            assignNode(new CallbackHaskellInternal(hscb));
        }
    };

    int CallbackHaskellInternal::call(Function& fcn, void* user_data) {
        #ifdef DEBUG_WOO
        std::cout << "CallbackHaskellInternal::call()\n";
        #endif
        int f = functor_fun_ptr(fcn);
        #ifdef DEBUG_WOO
        std::cout << "CallbackHaskellInternal::call() finished\n";
        #endif
        return f;
    }


    // CustomEvaluate
    class CustomEvaluateHaskellInternal :
        public CustomEvaluateInternal, FunctorHaskellInternal<hs_custom_evaluate_t> {

        friend class CustomEvaluateHaskell;

        CustomEvaluateHaskellInternal(hs_custom_evaluate_t hsce) :
            FunctorHaskellInternal<hs_custom_evaluate_t>(hsce) {}
        virtual void call(CustomFunction& fcn, void* user_data);
        virtual CustomEvaluateHaskellInternal* clone() const {
            return new CustomEvaluateHaskellInternal(functor_fun_ptr);
        }
    };

    class CustomEvaluateHaskell : public CustomEvaluate {
    public:
        CustomEvaluateHaskell(hs_custom_evaluate_t hsce) {
            assignNode(new CustomEvaluateHaskellInternal(hsce));
        }
    };

    void CustomEvaluateHaskellInternal::call(CustomFunction& fcn, void* user_data) {
        #ifdef DEBUG_WOO
        std::cout << "CustomEvaluateHaskellInternal::call()\n";
        #endif
        functor_fun_ptr(fcn);
        #ifdef DEBUG_WOO
        std::cout << "CustomEvaluateHaskellInternal::call() finished\n";
        #endif
    }

    
    // DerivateGenerator
    class DerivativeGeneratorHaskellInternal :
        public DerivativeGeneratorInternal, FunctorHaskellInternal<hs_derivative_generator_t> {

        friend class DerivativeGeneratorHaskell;

        DerivativeGeneratorHaskellInternal(hs_derivative_generator_t hsdg) :
            FunctorHaskellInternal<hs_derivative_generator_t>(hsdg) {}
        virtual Function call(Function& fcn, int nder, void* user_data);
        virtual DerivativeGeneratorHaskellInternal* clone() const {
            return new DerivativeGeneratorHaskellInternal(functor_fun_ptr);
        }
    };

    class DerivativeGeneratorHaskell : public DerivativeGenerator {
    public:
        DerivativeGeneratorHaskell(hs_derivative_generator_t hsce) {
            assignNode(new DerivativeGeneratorHaskellInternal(hsce));
        }
    };

    Function DerivativeGeneratorHaskellInternal::call(Function& fcn, int nder, void* user_data) {
        #ifdef DEBUG_WOO
        std::cout << "DerivativeGeneratorHaskellInternal::call()\n";
        #endif
        Function& f = functor_fun_ptr(fcn, nder);
        #ifdef DEBUG_WOO
        std::cout << "DerivativeGeneratorHaskellInternal::call() finished\n";
        #endif
        return f;
    }

    
} // namespace casadi

extern "C" CallbackHaskell * new_callback_haskell(hs_callback_t hs_callback);
CallbackHaskell * new_callback_haskell(hs_callback_t hs_callback){
    return new CallbackHaskell(hs_callback);
}
extern "C" CustomEvaluateHaskell * new_custom_evaluate_haskell(hs_custom_evaluate_t hs_custom_evaluate);
CustomEvaluateHaskell * new_custom_evaluate_haskell(hs_custom_evaluate_t hs_custom_evaluate){
    return new CustomEvaluateHaskell(hs_custom_evaluate);
}
extern "C" DerivativeGeneratorHaskell * new_derivative_generator_haskell(hs_derivative_generator_t hs_derivative_generator);
DerivativeGeneratorHaskell * new_derivative_generator_haskell(hs_derivative_generator_t hs_derivative_generator){
    return new DerivativeGeneratorHaskell(hs_derivative_generator);
}



extern "C" void delete_callback_haskell(casadi::CallbackHaskell* obj);
void delete_callback_haskell(casadi::CallbackHaskell* obj){
    delete obj;
}
extern "C" void delete_custom_evaluate_haskell(casadi::CustomEvaluateHaskell* obj);
void delete_custom_evaluate_haskell(casadi::CustomEvaluateHaskell* obj){
    delete obj;
}
extern "C" void delete_derivative_generator_haskell(casadi::DerivativeGeneratorHaskell* obj);
void delete_derivative_generator_haskell(casadi::DerivativeGeneratorHaskell* obj){
    delete obj;
}
