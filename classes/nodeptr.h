
#include <string>
#include <vector>

using namespace std;

struct nodeptr{
    string Lexeme;
    string Token;
    string name;
	int lineNumber;
    int n;
    vector<nodeptr*> children;
};