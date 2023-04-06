#include <vector>
#include "identifier.h"
#include <map>
using namespace std;

class Method {
    public:
        nodeptr* methodPointer;
        string lexeme;
        string return_type; 
        vector<string> appearances;
        map<int, ID> arguments;
        int getKey() {
            int key_score = 0;
            for ( int i = 0; i < lexeme.length(); i++ ) {
                key_score += ( lexeme[ i ] - 'a' + 1 );
            }
            return ( 31 * key_score );
        }
};
