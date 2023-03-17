#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <iterator>
#include "localTable.h"



using namespace std;

int main() {
    ID id("5", "i", "int");
    vector<GlobalTableEntry> GlobalTable;
    GlobalTableEntry gTE1("my_function",  "int");
    cout << "gTE1: " << gTE1.function_name << "\t" << gTE1.return_type << "\n";
    GlobalTable.push_back(gTE1);
    cout << "pushed to global table\n";
    LocalTable * mf = (LocalTable *)malloc(sizeof(LocalTable));
    int key = id.getKey();
    pair<int, ID> p(key, id);
    map<int, ID> d;
    mf->declarations = d;
    (mf->declarations).insert(p);
    cout << "inserted id\n";
    mf->gTE = &(gTE1);
    cout << "This is my local table for function: " << mf->gTE->function_name << " with return type " << mf->gTE->return_type << endl;
    map<int, ID>:: iterator it;
    for(it = (mf->declarations).begin(); it != (mf->declarations).end(); it++) {
        cout << it->first << "\t" << it->second.lexeme << "\n";
    }
    return 0;
}