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
        int getKey() {
            int key_score = 0;
            for(int i = 0; i < function_name.length(); i++) {
                key_score += (function_name[i] - 'a' + 1);
            }
            return (31 * key_score);
        }
};