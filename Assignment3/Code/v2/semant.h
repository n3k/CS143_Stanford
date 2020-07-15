#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

class ScopeSymbol;
class Scope;
class ScopeManager;
class MethodObj;
// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  ScopeManager *_scope_manager;
  int semant_errors;
  Classes _classes;
  Classes install_basic_classes(Classes);
  ostream& error_stream;

public:
  ClassTable(ScopeManager*, Classes);
  Classes get_classes() { return this->_classes; }
  ScopeManager *get_scope_manager() { return this->_scope_manager; }
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

