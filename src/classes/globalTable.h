#include <vector>
#include <map>

using namespace std;

class GlobalTable {
    public:
        map<int, ID> fields;
        map<int, Method> methods;
};