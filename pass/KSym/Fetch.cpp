#include "Project.h"

// fetch signatures
static const map<string, FetchDef> FILTERS({
    // most common ones
    {string("_copy_from_user"),             {1, 2, 0}},
    {string("call __get_user_${4:P}"),      {0, 1, -1}},
    {string("memdup_user"),                 {0, 1, -1}},
    // less common ones
    {string("memdup_user_nul"),                 {0, 1, -1}},
    {string("__copy_user_flushcache"),      {1, 2, 0}},
    {string("__copy_user_nocache"),         {1, 2, 0}},
    //freebsd
    {string("copyin"),                    {0,2,1}},
    {string("copyin_nofault"),             {0,2,1}},
    {string("fubyte"),                              {0,-2,-1}},
    {string("fuword"),                              {0,-2,-1}},
    {string("fuword16"),                              {0,-2,-1}},
    {string("fuword32"),                              {0,-2,-1}},
    {string("fuword64"),                              {0,-2,-1}},
    {string("fueword"),                              {0,-2,-1}},
    {string("fueword32"),                              {0,-2,-1}},
    {string("fueword64"),                              {0,-2,-1}},
    });

// helpers
const FetchDef *Fetch::findDefMatch(const string &name) {
  auto i = FILTERS.find(name);
  if(i == FILTERS.end()){
    return nullptr;
  } else {
    return &i->second;
  }
}
//return -2 indicate this is not a fetch function
const FetchDef * Fetch::isfetch(CallInst* ci){
    const FetchDef *def;
    if(ci->isInlineAsm()){
        InlineAsm *target = cast<InlineAsm>(ci->getCalledValue());
        def = Fetch::findDefMatch(target->getAsmString());
    } else {
        Function *target = ci->getCalledFunction();
        if(target == nullptr){
            // TODO handle indirect call
            def = nullptr;
        } else {
            def = Fetch::findDefMatch(target->getName().str());
        }
    }
    if(def != nullptr){
        return def;
    }
    else
        return nullptr;
}

