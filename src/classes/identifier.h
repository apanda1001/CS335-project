#include <vector>
#include "nodeptr.h"
using namespace std;
class ID {
public:
    nodeptr* varPointer;
    string value;
    string lexeme;
    string type; 
    vector<string> appearances;
    int getKey() {
        int key_score = 0;
        for( int i = 0; i < lexeme.length(); i++ ) {
            key_score += ( lexeme[ i ] - 'a' + 1 );
        }
        return ( 31 * key_score );
    }
};