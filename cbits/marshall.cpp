#include <iostream>

extern "C" std::string* marshall_stdstr(char* x);
std::string* marshall_stdstr(char * x){
    return new std::string(x);
}

extern "C" void free_stdstr(std::string* x);
void free_stdstr(std::string* x){
    delete x;
}

extern "C" void casadi_bindings_delete(void* x);
void casadi_bindings_delete(void* x){
    delete x;
}
