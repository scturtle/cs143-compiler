#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
#include <vector>
#include <map>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;
typedef SymbolTable<Symbol, Entry> SymTable;

#include "cool-tree.h" // seems to be OK here ...

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
  private:
    std::map<Symbol, Class_>
    classMap; // a map from class name symbol to [Class_]
    int semant_errors;
    void install_basic_classes();
    ostream &error_stream;

  public:
    ClassTable(Classes);
    int errors() { return semant_errors; }
    ostream &semant_error();
    ostream &semant_error(Class_ c);
    ostream &semant_error(Symbol filename, tree_node *t);
    // Class_ operator[](const Symbol name);
    Class_ lookup(Symbol cls);
    Feature_class *lookup(Symbol cls, Symbol name);
    // method_class *lookup_method(Symbol name, Symbol meth);
    bool leq(const Symbol c, const Symbol p);
    bool leq(const Symbol c, const Symbol p, const Symbol cls);
    Symbol lub(const Symbol a, const Symbol b);
    Symbol lub(const Symbol a, const Symbol b, const Symbol cls);
};

#endif
