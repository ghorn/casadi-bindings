#include "math.h"
#include <iostream>
#include "casadi/core/core.hpp"
#include "casadi/core/callback.hpp"

#include "HsFFI.h"

// #define DEBUG_WOO

#ifdef DEBUG_WOO
#define debug(msg)    \
  std::cerr << "* " << msg << std::endl;
#else
#define debug(msg){}
#endif

typedef std::vector<casadi::DM>*
  (*hs_callback_thing)(std::vector<casadi::DM*>*,
                       std::map<std::string, casadi::GenericType*>*);

class HaskellCallback : public casadi::Callback {
public:
  HaskellCallback(const std::string &name,
                  HsFunPtr hsfp,
                  std::vector<casadi::Sparsity> &sp_in_,
                  std::vector<casadi::Sparsity> &sp_out_) : cb_name(name), sp_in(sp_in_), sp_out(sp_out_) {
    debug(cb_name << ": HaskellCallback constructor called...");
    construct(name);
    hs_fun_ptr = hsfp;
  }

  // Initialize the object
  void init() override {
    debug(cb_name << ": HaskellCallback::init() called...");
  }

  // Number of inputs and outputs
  long long get_n_in() override { return sp_in.size(); }
  long long get_n_out() override { return sp_out.size(); }

  casadi::Sparsity get_sparsity_in(long long i) override {
    debug(cb_name << ": HaskellCallback::get_sparsity_in called");
    return sp_in[i];
  }
  casadi::Sparsity get_sparsity_out(long long i) override {
    debug(cb_name << ": HaskellCallback::get_sparsity_out called");
    return sp_out[i];
  }

  std::vector<casadi::DM> eval(const std::vector<casadi::DM>& args) const override {
    debug(cb_name << ": HaskellCallback::eval() called. Converting inputs to haskell-friendly...");
    std::vector<casadi::DM*> *hsArgs = new std::vector<casadi::DM*>();
    for (size_t k = 0; k < args.size(); k++) {
      hsArgs->push_back(new casadi::DM(args[k]));
    }

    std::map<std::string, casadi::GenericType> latestStats = stats();
    debug(cb_name << ": HaskellCallback::eval() called. Converting stats to haskell-friendly...");
    std::map<std::string, casadi::GenericType*> *hsStats =
      new std::map<std::string, casadi::GenericType*>();
    std::map<std::string, casadi::GenericType>::iterator it;
    for (it = latestStats.begin(); it != latestStats.end(); it++) {
      std::string key = it->first;
      casadi::GenericType *val = new casadi::GenericType(it->second);
      std::pair<std::string, casadi::GenericType*> keyVal(key, val);
      hsStats->insert(keyVal);
    }

    debug(cb_name << ": HaskellCallback::eval() calling back to the haskell function...");
    std::vector<casadi::DM>* hsRet = ((hs_callback_thing)hs_fun_ptr)(hsArgs, hsStats);

    debug(cb_name << ": HaskellCallback::call() converting haskell outputs to casadi-friendly...");
    std::vector<casadi::DM> ret;
    for (size_t k = 0; k < hsRet->size(); k++) {
      ret.push_back((*hsRet)[k]);
    }
    delete hsRet;

    debug(cb_name << ": HaskellCallback::call() return size: " << ret.size());
    debug(cb_name << ": HaskellCallback::call() returning casadi outputs to casadi...");
    return ret;
  }

  ~HaskellCallback() {
    // free the haskell FunPtr
    debug(cb_name << ": HaskellCallback destructor started");
    hs_free_fun_ptr(hs_fun_ptr);
    debug(cb_name << ": HaskellCallback destructor finished");
  }
  std::string cb_name;
private:
  HsFunPtr hs_fun_ptr;
  std::vector<casadi::Sparsity> sp_in;
  std::vector<casadi::Sparsity> sp_out;

}; // class HaskellCallback


extern "C"
void delete_haskell_callback(casadi::Function *haskell_callback0);
void delete_haskell_callback(casadi::Function *haskell_callback0) {
  HaskellCallback *haskell_callback = static_cast<HaskellCallback*>(haskell_callback0);
  const std::string name = haskell_callback->cb_name; // copy so we don't use after delete
  debug(name << ": deleting haskell callback...");
  delete haskell_callback;
  debug(name << ": haskell callback deleted");
}

extern "C"
casadi::Function * new_haskell_callback(const std::string *cb_name,
                                        HsFunPtr hsfp,
                                        std::vector<casadi::Sparsity*> *sp_in,
                                        std::vector<casadi::Sparsity*> *sp_out);
casadi::Function * new_haskell_callback(const std::string *cb_name,
                                        HsFunPtr hsfp,
                                        std::vector<casadi::Sparsity*> *sp_in_p,
                                        std::vector<casadi::Sparsity*> *sp_out_p) {
  debug(*cb_name << ": new_callback_haskell called. Unpacking sparsity_in/sparsity_out");
  std::vector<casadi::Sparsity> sp_in;
  for (size_t k=0; k<sp_in_p->size(); k++) {
    sp_in.push_back(*((*sp_in_p)[k]));
  }
  std::vector<casadi::Sparsity> sp_out;
  for (size_t k=0; k<sp_out_p->size(); k++) {
    sp_out.push_back(*((*sp_out_p)[k]));
  }
  return static_cast<casadi::Function*>(new HaskellCallback(*cb_name, hsfp, sp_in, sp_out));
}

extern "C"
std::vector<casadi::DM>* to_dm_vec(std::vector<casadi::DM*> *x);
std::vector<casadi::DM>* to_dm_vec(std::vector<casadi::DM*> *x) {
  std::vector<casadi::DM> *ret = new std::vector<casadi::DM>();
  for (size_t k=0; k<x->size(); k++) {
    ret->push_back(*((*x)[k]));
  }
  return ret;
}

extern "C" double c_fmod(double x, double y);
double c_fmod(double x, double y) {
  return fmod(x, y);
}

extern "C" float c_fmodf(float x, float y);
float c_fmodf(float x, float y) {
  return fmodf(x, y);
}

extern "C"
void hs_call_casadi_function_with_pointers(
  std::string ** err_msg,
  casadi::Function &f,
  double ** arg, int narg,
  double ** res, int nres) {
  try {
    // setup inputs
    std::vector<const double*> argv(narg);
    for (int k = 0; k < narg; k++) {
      argv[k] = arg[k];
    }

    // setup outputs
    std::vector<double*> resv(nres);
    for (int k = 0; k < nres; k++) {
      resv[k] = res[k];
    }

    // call the function
    f(argv, resv);
  } catch (std::exception& ex) {
    *err_msg = new std::string(ex.what());
  }
}
