//#include <casadi/casadi.hpp>
//#include <cmath>
//#include <stdexcept>
//#include <iostream>
//
//extern "C"
//casadi::GenericType *
//custom_generic_dictionary(std::string ** err_msg,
//                          std::vector< std::string* >& keys,
//                          std::vector< casadi::GenericType* >& vals);
//
//casadi::GenericType *
//custom_generic_dictionary(std::string ** err_msg,
//                          std::vector< std::string* >& keys,
//                          std::vector< casadi::GenericType* >& vals) {
//    try {
//        const int nk = keys.size();
//        const int nv = vals.size();
//        if (nk != nv) throw "dictionary key/value length mismatch";
//        
//        casadi::Dictionary dict;
//        for (int k = 0; k < nk; k++)
//            dict.insert(std::pair<std::string,casadi::GenericType>(*(keys[k]), *(vals[k])));
//
//        return new casadi::GenericType(dict);
//
//    } catch (std::exception& ex) {
//         *err_msg = new std::string(ex.what());
//         return 0;
//    }
//}
