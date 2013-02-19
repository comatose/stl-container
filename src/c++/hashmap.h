#ifndef __hashmap_h__
#define __hashmap_h__

#include <cstdint>
#include <vector>
#include <string>
#include <unordered_map>

/* typedef std::vector<uint8_t> Key; */
/* typedef std::vector<uint8_t> Value; */

/* namespace std { */
/*     template<> */
/* 	struct hash<Key> { */
/*     public: */
/* 	size_t operator()(const Key &s) const  */
/* 	{ */
/* 	    return std::_Hash_impl::hash(s.data(), s.size()); */
/* 	    /\* return std::hash<std::string>()(reinterpret_cast<const char*>(s.data())); *\/ */
/* 	} */
/*     }; */
/* } */

typedef std::string Key;
typedef std::string Value;

typedef std::unordered_map<Key, Value, std::hash<Key>> HashMap;
typedef HashMap::iterator HashMapIter;

#ifdef __cplusplus
extern "C"
{
#endif

    HashMap* hashmap_create();
    HashMap* hashmap_create_sized(std::size_t size);
    void hashmap_destroy(HashMap* h);

    void hashmap_insert(HashMap* h, const Key::value_type* key, std::size_t nK, const Value::value_type* val, std::size_t nV);
    void hashmap_lookup(HashMap* h, const Key::value_type* key, std::size_t nK, Value::value_type** pVal, std::size_t* pNV);
    void hashmap_delete(HashMap* h, const Key::value_type* key, std::size_t nK);
    std::size_t hashmap_size(const HashMap* h);

    HashMap::iterator* iter_create(HashMap* h);
    void iter_destroy(HashMap::iterator* it);

    bool iter_hasNext(HashMap* h, HashMap::iterator* it);
    void iter_next(HashMap* h, HashMap::iterator* it, Key::value_type** pKey, std::size_t* pNK
		   , Value::value_type** pVal, std::size_t* pNV);
    
#ifdef __cplusplus
}
#endif

#endif
