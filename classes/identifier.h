#include <string>
#include <vector>
using namespace std;
class ID {
public:
    string value;
    string lexeme;
    string type; // aur kuch daalu?
    vector<string> appearances;
    ID(string v, string l, string t) {
        value = v;
        lexeme  = l;
        type  = t;
        vector<string> a;
        appearances = a;
    }
    int getKey() {
        int key_score = 0;
        for(int i = 0; i < lexeme.length(); i++) {
            key_score += (lexeme[i] - 'a' + 1);
        }
        return (31 * key_score);
    }
};