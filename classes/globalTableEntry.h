#include <string>
#include <vector>
#include <map>
#include "identifier.h"

using namespace std;

class GlobalTableEntry {
    public:
        string function_name; 
        string return_type;
        vector<string> calls;
        map<int, ID> args;
        GlobalTableEntry(string f, string r) {
            function_name = f;
            return_type = r;
            vector<string> c;
            calls = c;
        }
};