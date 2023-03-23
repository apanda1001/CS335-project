#include <string>
#include <vector>
#include <map>

using namespace std;

class GlobalTable {
    public:
        map<int, ID> fields;
        vector< map<int, Method> > methods;
};