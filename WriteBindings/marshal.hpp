#ifndef __MARSHAL_THEM_BINDINGS_H__
#define __MARSHAL_THEM_BINDINGS_H__

#include <iostream>

template<class T1, class T2>
class Marshaling {
public:
    static T1 marshal(T2 x){ return static_cast<T1>(x); }
};

template<class T>
class Marshaling< T, T* > {
public:
    static T marshal(T* x){ return *x; }
};

template<class T>
class Marshaling< T&, T* > {
public:
    static T& marshal(T* x){ return *x; }
};

template<class T>
class Marshaling< T const &, T* > {
public:
    static T const & marshal(T* x){ return *x; }
};

template<class T1, class T2>
class Marshaling< std::vector< T1 >, std::vector< T2 > > {
  public:
  static std::vector< T1 > marshal(const std::vector< T2 > inputs){
    std::vector< T1 > vec;
    for (unsigned int k=0; k<inputs.size(); k++){
        vec.push_back( Marshaling< T1, T2 >::marshal(inputs[k]) );
    }
    return vec;
  }
};


template<class T1, class T2>
class Marshaling< T1, T2* > {
  public:
  static T1 marshal(T2* x){
      return Marshaling<T1, T2>::marshal( *x );
  }
};


/* ---------------------------------------------------------- */
/* call "new" on a bunch of stuff  */
template<class T1, class T2>
class WrapReturn {
 public:
    static T1 wrapReturn(T2 x){ return x; }
};

template<class T1, class T2>
class WrapReturn< std::vector< T1 >, std::vector< T2 > > {
  public:
  static std::vector< T1 > wrapReturn(std::vector< T2 > inputs){
    std::vector< T1 > vec;
    for (unsigned int k=0; k<inputs.size(); k++){
        vec.push_back( WrapReturn< T1, T2 >::wrapReturn(inputs[k]) );
    }
    return vec;
  }
};

template<class T1, class T2>
class WrapReturn< T1*, T2 > {
  public:
  static T1* wrapReturn(T2 x){
      return new T1( WrapReturn<T1,T2>::wrapReturn(x) );
  }
};


// some examples:

//#include <vector>
//#include <string>
//class A{};
//
//void testWrapReturn(){
//    // make a bunch of dummy inputs
//    A a = A();
//    std::vector< A > v_a = std::vector< A >();
//    std::vector< A > const v_c_a = std::vector< A >();
//    std::vector< std::vector< A > > v_v_a = std::vector< std::vector< A > >();
//    std::vector< std::vector< int > > v_v_int = std::vector< std::vector< int > >();
//    //std::vector< A >* pv_a = new std::vector< A >();
//    //std::vector< A* > v_pa = std::vector< A* >();
//    //std::vector< std::vector< A* >* >* pv_pv_pa = new std::vector< std::vector< A* >* >();
//
//    // try to use wrapReturn on the dummy inputs
//    A* o0 __attribute__((unused)) = WrapReturn< A*, A>::wrapReturn(a);
//
//
//    std::vector< A* >* o4 __attribute__((unused)) =
//        WrapReturn< std::vector< A* >*, std::vector< A > >::wrapReturn(v_a);
//
//    std::vector< A* >* o7 __attribute__((unused)) =
//        WrapReturn< std::vector< A* >*, std::vector< A > >::wrapReturn(v_c_a);
//
//
//    std::vector< std::vector< A* >* >* o5 __attribute__((unused)) =
//        WrapReturn< std::vector< std::vector< A* >* >*,
//                    std::vector< std::vector< A > >
//                    >::wrapReturn(v_v_a);
//
//
//    std::vector< std::vector< int >* >* o6 __attribute__((unused)) =
//        WrapReturn< std::vector< std::vector< int >* >*,
//                    std::vector< std::vector< int > >
//                    >::wrapReturn(v_v_int);
//    //std::vector< A > o5 = WrapReturn< std::vector< A >, std::vector< A* > >::wrapReturn(v_pa);
//    //
//    //std::vector< A > o6 = WrapReturn< std::vector< A >, std::vector< A* >* >::wrapReturn(pv_pa);
//    //std::vector< std::vector< A > > o7 =
//    //    WrapReturn< std::vector< std::vector< A > >,
//    //                std::vector< std::vector< A* >* >*
//    //                >::wrapReturn(pv_pv_pa);
//}
//
//void testMarshal(){
//    // make a bunch of dummy inputs
//    A* pa = new A();
//    std::vector< A* >* pv_pa = new std::vector< A* >();
//    std::vector< A >* pv_a = new std::vector< A >();
//    std::vector< A* > v_pa = std::vector< A* >();
//    std::vector< std::vector< A* >* >* pv_pv_pa = new std::vector< std::vector< A* >* >();
//
//    // try to use marshal on the dummy inputs
//    A o0 __attribute__((unused)) = Marshaling< A, A*>::marshal(pa);
//    A& o1 __attribute__((unused)) = Marshaling< A&, A*>::marshal(pa);
//    A const & o2 __attribute__((unused)) = Marshaling< A const &, A*>::marshal(pa);
//
//    std::vector< A > o3 = Marshaling< std::vector< A >, std::vector< A >* >::marshal(pv_a);
//    std::vector< A* > o4 = Marshaling< std::vector< A* >, std::vector< A* >* >::marshal(pv_pa);
//    std::vector< A > o5 = Marshaling< std::vector< A >, std::vector< A* > >::marshal(v_pa);
//
//    std::vector< A > o6 = Marshaling< std::vector< A >, std::vector< A* >* >::marshal(pv_pa);
//    std::vector< std::vector< A > > o7 =
//        Marshaling< std::vector< std::vector< A > >,
//                    std::vector< std::vector< A* >* >*
//                    >::marshal(pv_pv_pa);
//}


//
//
//using namespace CasADi;
//using namespace std;
//
//// test program
//int main(){
//    string name = string("x");
//    cout << "calling ssym\n";
//    Matrix< SX >* x = CasADi__ssym_TIC(&name, 1);
//    cout << "ssym results: " << *x << "\n\n";
//
//    cout << "calling .data()\n";
//    vector< SX* >* vecsx =
//        CasADi__Matrix_CasADi__SX___data(x);
//    cout << "data results: " << *vecsx << "\n";
//    cout << "data results: " << *((*vecsx)[0]) << "\n";
//    cout << "size of data: " << vecsx->size() << "\n\n";
//
//    cout << "calling SXMatrix(vector<SX*>)\n";
//    Matrix< SX >* sxmat =
//        CasADi__Matrix_CasADi__SX___SXMatrix_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC_TIC( vecsx );
//    cout << "calling SXMatrix.getDescription()\n";
//    cout << sxmat->getDescription() << "\n";
//    cout << "\n\n===== horray =====\n";
//    return 0;
//}


#endif // __MARSHAL_THEM_BINDINGS_H__
