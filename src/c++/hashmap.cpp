#include "hashmap.h"

// #include <iostream>
#include <algorithm>

using namespace std;

HashMap* hashmap_create(){
    return new HashMap();
}

HashMap* hashmap_create_sized(std::size_t size){
    return new HashMap(size);
}

void hashmap_destroy(HashMap* h){
    delete h;
    h = NULL;
    // cout << "hashmap_destory" << endl;
}

void hashmap_insert(HashMap* h, const Key::value_type* key, size_t nK, const Value::value_type* val, size_t nV){
    // cout << "[I]" << nK << ":" << nV << endl;
    (*h)[Key(key, key + nK)] = Value(val, val + nV);
    // h->emplace(make_pair(Key(key, key + nK), Value(val, val + nV)));
}

void hashmap_lookup(HashMap* h, const Key::value_type* key, size_t nK, Value::value_type** pVal, size_t* pNV){
    // cout << "[L]" << nK << ":";
    auto it = h->find(Key(key, key + nK));
    if(it == h->end()){
	*pVal = NULL;
	*pNV = 0;
    }
    else{
	*pVal = const_cast<Value::value_type*>(it->second.data());
	// *pVal = it->second.data();
	*pNV = it->second.size();
    }
    // cout << *pNV << endl;
}

void hashmap_delete(HashMap* h, const Key::value_type* key, size_t nK){
    // cout << "[D]" << nK << endl;
    h->erase(Key(key, key + nK));
}

size_t hashmap_size(const HashMap* h){
    return h->size();
}

HashMap::iterator* iter_create(HashMap* h){
    return new HashMap::iterator(h->begin());
}

void iter_destroy(HashMap::iterator* it){
    delete it;
    // cout << "iter_destory" << endl;
}

bool iter_hasNext(HashMap* h, HashMap::iterator* it){
    return (*it != h->end());
}

void iter_next(HashMap* h, HashMap::iterator* it, Key::value_type** pKey, std::size_t* pNK, Value::value_type** pVal, std::size_t* pNV){
    *pKey = const_cast<Key::value_type*>((*it)->first.data());
    *pNK = (*it)->first.size();
    *pVal = const_cast<Value::value_type*>((*it)->second.data());
    // *pVal = (*it)->second.data();
    *pNV = (*it)->second.size();
    ++(*it);
}
