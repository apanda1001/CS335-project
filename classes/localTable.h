#include <string>
#include <vector>
#include <map>
#include "globalTableEntry.h"

using namespace std;

class LocalTable {
    public:
        GlobalTableEntry* gTE;
        map <int, ID> args;
        map <int, ID> declarations; // will figure out a way to hash it
        vector <LocalTable *> children;
};