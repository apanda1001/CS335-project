#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <iterator>
#include <stdlib.h>
#include "localTable.h"
#include "globalTable.h"


using namespace std;

int main() {
    nodeptr * n = ( nodeptr * ) malloc( sizeof( nodeptr ) );
    ID id( "5", "i", "int" );
    id.varPointer = n;
    // cout << "pushed to global table\n";
    GlobalTable *GT = ( GlobalTable * ) malloc( sizeof( GlobalTable ) );
    // cout << "pushed to global table\n";
    Method m1( "my_function",  "int" );
    // cout << "pushed to global table\n";
    m1.methodPointer = n;
    // cout << "pushed to global table\n";
    map<int, Method> d;
    map<int, ID> d1;
    map<int, ID> d2;
    map<int, ID> d3;
    GT->methods = d;
    GT->fields = d1;
    GT->methods.insert( pair<int, Method>( m1.getKey(), m1 ) );
    // cout << "pushed to global table\n";
    LocalTable * mf = ( LocalTable * )malloc( sizeof( LocalTable ) );
    mf->declarations = d2;
    mf->args = d3;
    int key = id.getKey();
    mf->declarations.insert( pair<int, ID>( key, id ) );
    // cout << "inserted id\n";
    mf->m = &( m1 );
    cout << "This is my local table for function: " << mf->m->lexeme << " with return type " << mf->m->return_type << endl;
    map<int, ID>:: iterator it;
    for(it = (mf->declarations).begin(); it != (mf->declarations).end(); it++) {
        cout << it->first << "\t" << it->second.lexeme << "\n";
    }
    return 0;
}