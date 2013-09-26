#ifndef __MARSHALL_THEM_BINDINGS_H__
#define __MARSHALL_THEM_BINDINGS_H__

#include <iostream>
#include <symbolic/casadi.hpp>


void * get_null_ptr(void);

template<class T1, class T2>
class Marshalling {
 public:
  static T1 marshall(T2 x) {
    return x;
  }
}; 



template<class T>
class Marshalling< T, T* > {
  public:
  static T marshall(T* x){
    return *x;
  }
};

template<>
class Marshalling< std::string , char** > {
  public:
  static std::string marshall(char ** x) {
    return *x;
  }
};


template<class T1, class T2>
class Marshalling< std::vector< T1 >,std::vector< T2* > &> {
  public:
  static std::vector< T1 > marshall(const std::vector< T2* > & inputs){
    std::vector< T1 > vec;
    for (unsigned int k=0; k<inputs.size(); k++){
        vec.push_back( Marshalling< T1, T2* >::marshall(inputs[k]) );
    }
    return vec;
  }
};

template<class T1, class T2>
class Marshalling< const std::vector< T1 >,const std::vector< T2* > & > {
  public:
  static const std::vector< T1 > marshall(const std::vector< T2* > & inputs){
    std::vector< T1 > vec;
    for (unsigned int k=0; k<inputs.size(); k++){
        vec.push_back( Marshalling< T1, T2* >::marshall(inputs[k]) );
    }
    return vec;
  }
};

template<class T1, class T2>
class Marshalling< std::vector< std::vector< T1 > >,std::vector< std::vector< T2*> > &> {
  public:
  static std::vector< std::vector< T1 > > marshall(const std::vector< std::vector< T2* > > & inputs){
    std::vector< std::vector< T1 > > vec;
    for (unsigned int k=0; k<inputs.size(); k++){
        vec.push_back( Marshalling< std::vector< T1 >, std::vector< T2*> &>::marshall(inputs[k]) );
    }
    return vec;
  }
};

template<class T1, class T2>
class Marshalling< const std::vector< std::vector< T1 > >,const std::vector< std::vector< T2* > > & > {
  public:
  static const std::vector< std::vector< T1 > > marshall(const std::vector< std::vector< T2*> > & inputs){
    std::vector< std::vector< T1 > > vec;
    for (unsigned int k=0; k<inputs.size(); k++){
        vec.push_back( Marshalling< const std::vector< T1 >, const std::vector< T2* > &>::marshall(inputs[k]) );
    }
    return vec;
  }
};

#endif // __MARSHALL_THEM_BINDINGS_H__
