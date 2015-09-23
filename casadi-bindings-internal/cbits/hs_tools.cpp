#include <iostream>
#include <vector>
#include <map>
#include "string.h"

using namespace std;

///////////// scalars /////////////
// string
extern "C" int hs_length_string(string * str);
int hs_length_string(string * str){
    return str->length();
}
extern "C" void hs_copy_string(string * str, char outputs[]);
void hs_copy_string(string * str, char outputs[]){
    strcpy(outputs, str->c_str());
}
extern "C" string * hs_new_string(char x[]);
string * hs_new_string(char x[]){
    return new string(x);
}
extern "C" void hs_delete_string(string * x);
void hs_delete_string(string * x){
    delete x;
}

////////////////// std::pair /////////////////
extern "C" std::pair<void*,void*> * hs_new_stdpair(void * x, void * y);
std::pair<void*,void*> * hs_new_stdpair(void * x, void * y) {
    return new std::pair<void*,void*>(x, y);
}
extern "C" void * hs_stdpair_fst(std::pair<void*,void*> * stdpair);
void * hs_stdpair_fst(std::pair<void*,void*> * stdpair) {
    return stdpair->first;
}
extern "C" void * hs_stdpair_snd(std::pair<void*,void*> * stdpair);
void * hs_stdpair_snd(std::pair<void*,void*> * stdpair) {
    return stdpair->second;
}
extern "C" void hs_delete_stdpair(std::pair<void*,void*> * pair);
void hs_delete_stdpair(std::pair<void*,void*> * pair){
    delete pair;
}

extern "C" std::pair<int,int> * hs_new_stdpair_int(int x, int y);
std::pair<int,int> * hs_new_stdpair_int(int x, int y) {
    return new std::pair<int,int>(x, y);
}
extern "C" int hs_stdpair_fst_int(std::pair<int,int> * stdpair);
int hs_stdpair_fst_int(std::pair<int,int> * stdpair) {
    return stdpair->first;
}
extern "C" int hs_stdpair_snd_int(std::pair<int,int> * stdpair);
int hs_stdpair_snd_int(std::pair<int,int> * stdpair) {
    return stdpair->second;
}
extern "C" void hs_delete_stdpair_int(std::pair<int,int> * pair);
void hs_delete_stdpair_int(std::pair<int,int> * pair){
    delete pair;
}


////////////////// std::map /////////////////
extern "C" std::map<string,void*> * hs_new_dict(std::vector<std::string*> * keys,
                                                std::vector<void*> * vals);
std::map<string,void*> * hs_new_dict(std::vector<std::string*> * keys, std::vector<void*> * vals) {
    std::map<std::string,void*> * ret = new std::map<std::string,void*>;
    for (int k = 0; k < keys->size(); k++) {
        std::string key = *((*keys)[k]);
        void * val = (*vals)[k];
        std::pair<std::string, void*> keyVal(key, val);
        ret->insert(keyVal);
    }
    return ret;
}

extern "C" int hs_dict_size(std::map<std::string, void*> * map);
int hs_dict_size(std::map<std::string, void*> * map) {
    return map->size();
}

extern "C" void hs_dict_copy(std::map<std::string, void*> * map,
                             std::string * keys[],
                             void * vals[]);
void hs_dict_copy(std::map<std::string, void*> * map,
                  std::string * keys[],
                  void * vals[]) {
    int k = 0;
    for(std::map<std::string, void*>::iterator it = map->begin(); it != map->end(); it++) {
        std::string * key = new std::string(it->first);
        void * val = it->second;
        keys[k] = key;
        vals[k] = val;
        k++;
    }
}

extern "C" void * hs_lookup_dict(std::map<std::string, void*> * map, std::string* key);
void * hs_lookup_dict(std::map<std::string, void*> * map, std::string* key) {
    return map->at(*key);
}

extern "C" void hs_delete_dict(std::map<std::string,void*> *);
void hs_delete_dict(std::map<std::string,void*> * dict) {
    delete dict;
}

////////////////////////// copying vectors to arrays /////////////////////
template <typename T>
void hs_copy_vec_T(vector<T> * vec, T outputs[]){
    memcpy(outputs, &((*vec)[0]), vec->size()*sizeof(T));
}

//////////////////////    CREATING VECTORS FROM ARRAYS ///////////////////////////////////
// 1-dimensional
template <typename T>
vector<T> * hs_new_vec_T(T inputs[], int length){
    vector<T> vec;
    for (int k=0; k<length; k++)
        vec.push_back(inputs[k]);
    return new vector<T>(vec);
}

////////////////////////////////////////////////////////////////////////////////////////////
#define WRITE_STUFF(name, type) \
    extern "C" vector<type>* hs_new_vec_##name( type inputs[], int length); \
    vector<type>* hs_new_vec_##name( type inputs[], int length){return hs_new_vec_T(inputs, length);} \
    extern "C" void hs_delete_vec_##name(vector<type> * vec); \
    void hs_delete_vec_##name(vector<type> * vec){ delete vec; } \
    extern "C" void hs_copy_vec_##name(vector<type> * vec, type outputs[]); \
    void hs_copy_vec_##name(vector<type> * vec, type outputs[]){hs_copy_vec_T(vec, outputs);} \
    extern "C" int hs_size_vec_##name(vector<type> * vec); \
    int hs_size_vec_##name(vector<type> * vec){ return vec->size(); }

WRITE_STUFF(int,int)
WRITE_STUFF(voidp, void*)
WRITE_STUFF(uchar, unsigned char)
WRITE_STUFF(double, double)
