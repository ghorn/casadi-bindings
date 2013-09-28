#ifndef __MARSHAL_THEM_BINDINGS_H__
#define __MARSHAL_THEM_BINDINGS_H__

#include <iostream>
#include <symbolic/casadi.hpp>


void * get_null_ptr(void);

template<class T1, class T2>
class Marshaling {
 public:
  static T1 marshal(T2 x) {
    return x;
  }
}; 



template<class T>
class Marshaling< T, T* > {
  public:
  static T marshal(T* x){
    return *x;
  }
};

template<>
class Marshaling< std::string , char** > {
  public:
  static std::string marshal(char ** x) {
    return *x;
  }
};


template<class T1, class T2>
class Marshaling< std::vector< T1 >,std::vector< T2* > &> {
  public:
  static std::vector< T1 > marshal(const std::vector< T2* > & inputs){
    std::vector< T1 > vec;
    for (unsigned int k=0; k<inputs.size(); k++){
        vec.push_back( Marshaling< T1, T2* >::marshal(inputs[k]) );
    }
    return vec;
  }
};

template<class T1, class T2>
class Marshaling< const std::vector< T1 >,const std::vector< T2* > & > {
  public:
  static const std::vector< T1 > marshal(const std::vector< T2* > & inputs){
    std::vector< T1 > vec;
    for (unsigned int k=0; k<inputs.size(); k++){
        vec.push_back( Marshaling< T1, T2* >::marshal(inputs[k]) );
    }
    return vec;
  }
};

template<class T1, class T2>
class Marshaling< std::vector< std::vector< T1 > >,std::vector< std::vector< T2*> > &> {
  public:
  static std::vector< std::vector< T1 > > marshal(const std::vector< std::vector< T2* > > & inputs){
    std::vector< std::vector< T1 > > vec;
    for (unsigned int k=0; k<inputs.size(); k++){
        vec.push_back( Marshaling< std::vector< T1 >, std::vector< T2*> &>::marshal(inputs[k]) );
    }
    return vec;
  }
};

template<class T1, class T2>
class Marshaling< const std::vector< std::vector< T1 > >,const std::vector< std::vector< T2* > > & > {
  public:
  static const std::vector< std::vector< T1 > > marshal(const std::vector< std::vector< T2*> > & inputs){
    std::vector< std::vector< T1 > > vec;
    for (unsigned int k=0; k<inputs.size(); k++){
        vec.push_back( Marshaling< const std::vector< T1 >, const std::vector< T2* > &>::marshal(inputs[k]) );
    }
    return vec;
  }
};

#endif // __MARSHAL_THEM_BINDINGS_H__
