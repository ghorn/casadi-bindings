#include <casadi/casadi.hpp>
#include <cmath>
#include <stdexcept>
#include "marshal.hpp"

//extern "C"
//std::string * casadi__custom__generateCode(std::string ** err_msg, casadi::Function* obj, int x0);
//std::string * casadi__custom__generateCode(std::string ** err_msg, casadi::Function* obj, int x0){
//    try {
//        bool x0_ = Marshaling<bool,int>::marshal(x0);
//        std::stringstream ret;
//        obj->generateCode(ret, x0_);
//        return new std::string(ret.str());
//    } catch (std::exception& ex) {
//         *err_msg = new std::string(ex.what());
//         return 0;
//    }
//}
