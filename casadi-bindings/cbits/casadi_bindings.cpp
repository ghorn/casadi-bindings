#include "math.h"
#include <iostream>
#include "casadi/core/core.hpp"
#include "casadi/core/function/callback.hpp"

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
  HaskellCallback(HsFunPtr hsfp,
                  std::vector<casadi::Sparsity> &sp_in_,
                  std::vector<casadi::Sparsity> &sp_out_) : sp_in(sp_in_), sp_out(sp_out_) {
    debug("HaskellCallback constructor called...");
    hs_fun_ptr = hsfp;
  }

  // Creator function, creates an owning reference
  static casadi::Function create(const std::string& name,
                                 HsFunPtr hsfp,
                                 std::vector<casadi::Sparsity> &sp_in_,
                                 std::vector<casadi::Sparsity> &sp_out_,
                                 const casadi::Dict& opts=casadi::Dict()) {
    debug("HaskellCallback create called...");
    return casadi::Callback::create(name, new HaskellCallback(hsfp, sp_in_, sp_out_), opts);
  }

  // Number of inputs and outputs
  virtual int get_n_in() { return sp_in.size(); }
  virtual int get_n_out() { return sp_out.size(); }
  virtual casadi::Sparsity get_sparsity_in(int i) {
    debug("HaskellCallback::get_sparsity_in called");
    return sp_in[i];
  }
  virtual casadi::Sparsity get_sparsity_out(int i) {
    debug("HaskellCallback::get_sparsity_out called");
    return sp_out[i];
  }
  
  virtual std::vector<casadi::DM> eval(const std::vector<casadi::DM>& args) {
    debug("HaskellCallback::eval() called. Converting inputs to haskell-friendly...");
    std::vector<casadi::DM*> *hsArgs = new std::vector<casadi::DM*>();
    for (int k = 0; k < args.size(); k++) {
      hsArgs->push_back(new casadi::DM(args[k]));
    }
    
    std::map<std::string, casadi::GenericType> latestStats = stats();
    debug("HaskellCallback::eval() called. Converting stats to haskell-friendly...");
    std::map<std::string, casadi::GenericType*> *hsStats =
      new std::map<std::string, casadi::GenericType*>();
    std::map<std::string, casadi::GenericType>::iterator it;
    for (it = latestStats.begin(); it != latestStats.end(); it++) {
      std::string key = it->first;
      casadi::GenericType *val = new casadi::GenericType(it->second);
      std::pair<std::string, casadi::GenericType*> keyVal(key, val);
      hsStats->insert(keyVal);
    }
    
    debug("HaskellCallback::eval() calling back to the haskell function...");
    std::vector<casadi::DM>* hsRet = ((hs_callback_thing)hs_fun_ptr)(hsArgs, hsStats);

    debug("HaskellCallback::call() converting haskell outputs to casadi-friendly...");
    std::vector<casadi::DM> ret;
    for (int k = 0; k < hsRet->size(); k++) {
      ret.push_back((*hsRet)[k]);
    }
    delete hsRet;

    debug("HaskellCallback::call() return size: " << ret.size());
    debug("HaskellCallback::call() returning casadi outputs to casadi...");
    return ret;
  }
  
  // Initialize the object
  virtual void init() {
    debug("HaskellCallback::init() called...");
  }

  ~HaskellCallback() {
    // free the haskell FunPtr
    debug("HaskellCallback destructor started\n");
    hs_free_fun_ptr(hs_fun_ptr);
    debug("HaskellCallback destructor finished\n");
  }
private:
  HsFunPtr hs_fun_ptr;
  std::vector<casadi::Sparsity> sp_in;
  std::vector<casadi::Sparsity> sp_out;

}; // class HaskellCallback


extern "C"
casadi::Function * new_callback_haskell(HsFunPtr hsfp,
                                        std::vector<casadi::Sparsity*> *sp_in,
                                        std::vector<casadi::Sparsity*> *sp_out);
casadi::Function * new_callback_haskell(HsFunPtr hsfp,
                                        std::vector<casadi::Sparsity*> *sp_in_p,
                                        std::vector<casadi::Sparsity*> *sp_out_p) {
  casadi::Dict options = casadi::Dict();
  debug("new_callback_haskell called. Unpacking sparsity_in/sparsity_out");
  std::vector<casadi::Sparsity> sp_in;
  for (int k=0; k<sp_in_p->size(); k++) {
    sp_in.push_back(*((*sp_in_p)[k]));
  }
  std::vector<casadi::Sparsity> sp_out;
  for (int k=0; k<sp_out_p->size(); k++) {
    sp_out.push_back(*((*sp_out_p)[k]));
  }
  return new casadi::Function(HaskellCallback::create("haskell_callback",
                                                      hsfp,
                                                      sp_in, sp_out,
                                                      options));
}

extern "C"
std::vector<casadi::DM>* to_dm_vec(std::vector<casadi::DM*> *x);
std::vector<casadi::DM>* to_dm_vec(std::vector<casadi::DM*> *x) {
  std::vector<casadi::DM> *ret = new std::vector<casadi::DM>();
  for (int k=0; k<x->size(); k++) {
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
