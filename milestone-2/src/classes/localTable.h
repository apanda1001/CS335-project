#include <string>
#include <vector>
#include <map>
#include "method.h"

using namespace std;

class LocalTable {
    public:
        Method* m;
        map <int, ID> args;
        map <int, ID> declarations; 
};