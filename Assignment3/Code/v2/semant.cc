

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include "ScopeListNode.h"
#include <vector>
#include <map>

using namespace std;

extern int semant_debug;
extern char *curr_filename;

bool G_DEBUG = false;


//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol  arg, arg2,  Bool,  concat,  cool_abort,  xcopy,  Int,  in_int,  in_string,  IO,
    length,  Main,  main_meth,  No_class,  No_type,  Object,  out_int,  out_string,  prim_slot,
    self,  SELF_TYPE,  Str,  str_field,  substr,  type_name,  val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    xcopy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}





class ScopeSymbol {
    private:
        bool _default;
    public:
        Symbol sym; // bar
        Symbol type; 
        Symbol inherit_parent;  // Bar        
        ScopeSymbol(Symbol sym, Symbol type, Symbol parent, bool def) {     
            this->sym = sym;
            this->type = type;
            this->inherit_parent = parent;
            this->_default = def;
        }    

        bool isDefault() {
            return this->_default;
        }
};

class Scope {
private:
    string desc;
    std::vector<ScopeSymbol> _symbol_list;
public: 
    Scope(string desc) {
        this->desc = desc;
    }

    std::vector<ScopeSymbol> get_symbol_list() {
        return this->_symbol_list;
    }

    string get_desc() {
        return this->desc;
    }
   
    bool add_symbol(Symbol x, Symbol type, Symbol parent, bool def) {
        //cout << "checking if symbol defined?" << endl;
        //cout << "Description: " <<  desc << endl;
        ScopeSymbol *res = find_symbol(x);
        if (res == NULL) {
            ScopeSymbol newEntry = ScopeSymbol(x, type, parent, def);
            //cout << "Adding new ScopeSymbol: " << x << endl;
            this->_symbol_list.push_back(newEntry);
            //cout << "Vector size: " << this->_symbol_list.size() << endl;
            //cout << "Vector elto: " << this->_symbol_list[0].sym << endl;
            return true;
        }
        return false;
    }

    ScopeSymbol * find_symbol(Symbol x) {
        for (std::vector<ScopeSymbol>::iterator it = this->_symbol_list.begin(); it != this->_symbol_list.end(); ++it) {            
            //cout << *it->sym << endl;
            if (it->sym->equal_string(x->get_string(), x->get_len())) {                
                return &(*it);
            }
        }
        return NULL;
    } 


};

class MethodObj {
    private:
        Symbol id;
        Symbol retType;
        Symbol className;
    public:
        MethodObj(Symbol id, Symbol retType, Symbol className) {
            this->id = id;
            this->retType = retType;
            this->className = className;
        }

        Symbol get_id() {
            return this->id;
        }

        Symbol get_retType() {
            return this->retType;
        }

        Symbol get_className() {
            return this->className;
        }
};


class AttributeObj {
    private:
        Symbol id;       
        Symbol className;
    public:
        AttributeObj(Symbol id, Symbol className) {
            this->id = id;
            this->className = className;
        }

        Symbol get_id() {
            return this->id;
        }

        Symbol get_className() {
            return this->className;
        }
};


class ScopeManager {
private:
    MyList<MyNode<Scope> > *_scope_list_head;
    std::vector<MethodObj> _method_list;
    std::vector<AttributeObj> _attribute_list;
    Class_ CurrentClass;
    Symbol CurrentMethod;    
public:
    map<Symbol, Class_> ClassMap;
    /* When creating a ScopeManager, we already create a first Scope, the global Scope */
   ScopeManager() {
        Scope *scope = new Scope("global");       
        MyNode<Scope> *node = new MyNode<Scope>(scope);
        _scope_list_head = new MyList<MyNode<Scope> >(node);
    } 

    ~ScopeManager() {
        delete _scope_list_head;
    }

    Scope * get_current_scope() {
        return _scope_list_head->getTail()->getData();
    }

    Scope * get_global_scope() {
        return _scope_list_head->getHead()->getData();
    }

    MyNode<Scope> * get_head() {
        return _scope_list_head->getHead();
    }

    MyNode<Scope> * get_tail() {
        return _scope_list_head->getTail();
    }

    void print_current_scope() {
        Scope *scope = this->get_current_scope();
        std::vector<ScopeSymbol> vec = scope->get_symbol_list();
        for (std::vector<ScopeSymbol>::iterator it = vec.begin() ; it < vec.end(); ++it ) {
            cout << "Symbol: " <<  it->sym << endl;
        }
    }

    bool check_defined_in_global_scope(Symbol x) {
        Scope *global = this->get_global_scope();
        ScopeSymbol *res = global->find_symbol(x);
        if (res) {
            return true;
        }
        return false;
    }

    ScopeSymbol * find_in_global_scope(Symbol x) {
        Scope *global = this->get_global_scope();
        ScopeSymbol *res = global->find_symbol(x);
        return res;
    }
  
   ScopeSymbol * find_symbol(Symbol x) {
       /*
       Start looking for the symbol in the tail (the current scope)
       Then walk backwards through the chain until we get it.
       Otherwise return NULL
       */
       MyNode<Scope> *node = get_tail();
       Scope *scope = node->getData();
       ScopeSymbol *res = NULL;
       while (node != _scope_list_head->getHead()) {
           res = scope->find_symbol(x);
           if (res != NULL) {
                return res;
            }
            node = node->prev;
            scope = node->getData();
       }     
       scope = node->getData();
       return scope->find_symbol(x);

       /*
       This was solved with scope_management

       res = scope->find_symbol(x);
       if (res != NULL) {
           return res;
       }

       // If we still haven't found the symbol in the scope chain
       // it could still be defined in our inheritance chain!
       // we can re-use the check_attribute_redefinition algorithm for this
       res = this->find_in_global_scope(this->get_current_class()->get_name());
       Symbol p = res->inherit_parent;
       while (p != Object) {
           if (this->lookup_attribute_in_class(x, p)) {
                return true;
           }
           res = this->find_in_global_scope(p);
           p = res->inherit_parent;
       }
       return NULL;
       */
   }

   bool add_symbol(Symbol x, Symbol type, Symbol parent) {
       Scope *scope = get_current_scope();
       return scope->add_symbol(x, type, parent, false);
   }

   bool add_default_symbol(Symbol x, Symbol type, Symbol parent) {
       Scope *scope = get_current_scope();
       return scope->add_symbol(x, type, parent, true);
   }

   void enter_scope(string desc) {        
        Scope *scope = new Scope(desc);
        MyNode<Scope> *node = new MyNode<Scope>(scope);
        this->_scope_list_head->enqueue(node);        
   }

   void exit_scope() {
       this->_scope_list_head->dequeue();        
   }

   bool add_method(Symbol methodId, Symbol retType, Symbol className) {
       Symbol a;
       // Check if the method already exists for that className.
       if (this->lookup_method_in_class(methodId, className, &a)) {
           return false;
       }
       this->_method_list.push_back(MethodObj(methodId, retType, className));
       return true;
   }

   bool lookup_method_in_class(Symbol methodId, Symbol className, Symbol *retType) {
       for (std::vector<MethodObj>::iterator it = this->_method_list.begin(); it != this->_method_list.end(); ++it) {           
            if (it->get_id() == methodId && it->get_className() == className) {                
                *retType = it->get_retType();
                return true;
            }
        }       
        return false;
   }

   bool lookup_method_in_inheritance_chain(Symbol methodId, Symbol className, Symbol *retType) {
        if (this->lookup_method_in_class(methodId, className, retType)) {
            return true;
        }
        // We also need to look into parents!
        ScopeSymbol *ss = this->find_in_global_scope(className);
        Symbol p = ss->inherit_parent;        
        
        while (p != No_class) {
           if (this->lookup_method_in_class(methodId, p, retType)) {
                return true;
           }
           ss = this->find_in_global_scope(p);
           p = ss->inherit_parent;
        }
        return false;
   }

   bool check_valid_method_redefinition(Class_ current_class, method_class *_method, Symbol *param1, Symbol *param2, int *errType) {
       // Quickly check if the method is defined in the inheritance chain
       // If not, return true
       *errType = 0;
       Symbol methodId = _method->get_name();
       Symbol className = current_class->get_name();
       Symbol p = current_class->get_parent();
       Symbol a;
       if (!lookup_method_in_inheritance_chain(methodId, p, &a)) {
           return true;
       }
        // Otherwise, let's first find the parent that has the original method definition
        ScopeSymbol *ss = this->find_in_global_scope(className);             
        Symbol cur_parent = className;
        bool found = false;
        while (cur_parent != No_class && !found) {
           cur_parent = ss->inherit_parent;
           for (std::vector<MethodObj>::iterator it = this->_method_list.begin(); it != this->_method_list.end(); ++it) {           
                if (it->get_id() == methodId && it->get_className() == cur_parent) {                
                    found = true;
                    break;                    
                }
            }          
           ss = this->find_in_global_scope(cur_parent);           
        } 
        if(G_DEBUG) {
            cout<< "found parent " << cur_parent << " of original method " << methodId << endl;
        }
        Class_ _class = this->ClassMap[cur_parent];

        // Get the original method
        method_class *original_method;
        Features _features = _class->get_features();
        int feature_idx = 0;
        for (feature_idx = 0; feature_idx < _features->len(); feature_idx++) {
            Feature _cur_feature = _features->nth(feature_idx);
            if (_cur_feature->get_construct_type() == MethodType && _cur_feature->get_name() == methodId) {
                original_method = (method_class *)_cur_feature;
                break;
            }
        }

        if(G_DEBUG) {
            cout<< "original_method:  " << original_method <<  endl;
        }

        // Check formals types and retType match
        Formals _formals_of_redefined = _method->get_formals();
        Formals _formals_of_original = original_method->get_formals();
        if (_formals_of_original->len() != _formals_of_redefined->len()) {
            *errType = 2;
            return false;
        }
        if (_method->get_return_type() != original_method->get_return_type()) {
            return false;
        }
        int formal_idx = 0;
        for (formal_idx=0; formal_idx < _formals_of_original->len(); formal_idx++) {
            Formal original_formal = _formals_of_original->nth(formal_idx);            
            Formal redefined_formal = _formals_of_redefined->nth(formal_idx);
            if (redefined_formal->get_type_decl() != original_formal->get_type_decl()){
                *param1 = redefined_formal->get_type_decl();
                *param2 = original_formal->get_type_decl();
                *errType = 1;
                return false;
            }
        }
        return true;

   }

   bool check_attribute_redefinition(Symbol attrId, Symbol className) {
       // For attributes, we also need to check that the Parent chain don't have the same name
       // There is no attribute redefinition in Cool       
       ScopeSymbol *ss = this->find_in_global_scope(className);
       Symbol p = ss->inherit_parent;
       while (p != Object) {
           if (this->lookup_attribute_in_class(attrId, p)) {
                return true;
           }
           ss = this->find_in_global_scope(p);
           p = ss->inherit_parent;
       }
       return false;
   }

   bool add_attribute(Symbol attrId, Symbol className) {
       Symbol a;
       // Check if the method already exists for that className.
       if (this->lookup_attribute_in_class(attrId, className)) {
           return false;
       }       
       this->_attribute_list.push_back(AttributeObj(attrId, className));
       return true;
   }

   bool lookup_attribute_in_class(Symbol attrId, Symbol className) {
       for (std::vector<AttributeObj>::iterator it = this->_attribute_list.begin(); it != this->_attribute_list.end(); ++it) {           
            if (it->get_id() == attrId && it->get_className() == className) {               
                return true;
            }
        }
        return false;
   }

   Class_ get_current_class() {
       return this->CurrentClass;
   }

   void set_current_class(Class_ _class) {
       this->CurrentClass = _class;
   }

   Symbol get_current_method() {
       return this->CurrentMethod;
   }

   void set_current_method(Symbol method) {
       this->CurrentMethod = method;
   }   

   /* for debugging purposes */

   void print_method_list() {
       for (std::vector<MethodObj>::iterator it = this->_method_list.begin(); it != this->_method_list.end(); ++it) {           
            cout << "Class: " << it->get_className() << " - Method: " << it->get_id() << " - retType: " << it->get_retType() << endl;            
        }
   }
   
};



/*
The global scope is always the list of classes defined on every file
                                Just that
*/

ClassTable::ClassTable(ScopeManager *scope_manager, Classes classes) : semant_errors(0) , error_stream(cerr) {
    /*
    This is a first round of checks:
        1) There cannot be multiple definitions of a class 
        2) Class Main MUST be defined
        3) If Main class is defined, check it has the 'main' method defined  
        4) Check that are no multiple definitions of a method in a same class
        5) Check that attributes have a valid defined Type 
        6) Check there are no multiple definitions of an attribute in a same class
        7) Check for attribute overriding             
        8) For classes that have parents, those parents must exist in the global scope
            8.1) beware of cyclic references
        9) Check the formals for all defined methods of classes
            9.1) These should have a valid type
            9.2) Cannot be named self
            9.3) All arguments must have a different name

    We also need tp add all the defined methods and attributes to the vector of methods and attributes.
    */
    //this->_classes = classes;
    this->_scope_manager = scope_manager;

    int classes_idx = 0;
    // Set default to false for everything first
    for(classes_idx = 0; classes_idx < classes->len(); classes_idx++) {
        classes->nth(classes_idx)->set_non_default();        
    } 

    this->_classes = install_basic_classes(classes);  
    
    ////install_basic_classes(classes);  

    
    // 1) There cannot be multiple definitions of a class  and defaults cannot be redefined either!
    for(classes_idx = 0; classes_idx < classes->len(); classes_idx++) {
        Class_ cur = classes->nth(classes_idx);
        if (cur->is_default()) {
            continue;
        }
        if ( (cur->get_name() == SELF_TYPE) || !(this->_scope_manager->add_symbol(cur->get_name(), cur->get_name(), cur->get_parent())) ) {
            semant_error(cur)<<"Redefinition of a basic class " << cur->get_name() << ".\n";
            return;
        }     
    }   

   
    // 2) Class Main MUST be defined
    Class_ _main = NULL;
    for(classes_idx = 0; classes_idx < classes->len(); classes_idx++) { 
        Class_ cur =  classes->nth(classes_idx);            
		if(cur->get_name() == Main) {
             _main = cur;
             break;
        } 
    }
    if (!_main) { semant_error()<<"Class Main is not defined.\n"; }
    else{
        int feature_idx = 0;    
        int f = 0;            
        Features _features = _main->get_features();        
        for(feature_idx = 0; feature_idx < _features->len(); feature_idx++) {            
            Feature _feature = _features->nth(feature_idx);                          
            if (_feature->get_construct_type() == MethodType) {                                               
                if (_feature->get_name() == main_meth) {
                    f = 1;
                    break;
                }
            }     
        }
        if (!f) {
            semant_error()<<"method 'main' is not defined for Main class.\n";
        }
    }


    for(classes_idx = 0; classes_idx < classes->len(); classes_idx++) { 
        Class_ cur =  classes->nth(classes_idx);  
        if (cur->is_default()) {
            continue;
        }
        int feature_idx = 0;    
        Features _features = cur->get_features();    
        for(feature_idx = 0; feature_idx < _features->len(); feature_idx++) {           
            Feature _feature = _features->nth(feature_idx);                          
            if (_feature->get_construct_type() == MethodType) {   
                // Check if the return type of the method exists in the global scope
                Symbol retType = _feature->get_return_type();
                if (retType == SELF_TYPE) {
                    retType = cur->get_name();
                }
                if ( !this->_scope_manager->check_defined_in_global_scope(retType)) {
                    semant_error(cur)<<" xclasstable constructor xx type " << retType << " doesn't exist.\n";
                }         
                
                if ( !(this->_scope_manager->add_method(
                    _feature->get_name(),
                    _feature->get_return_type(), 
                    cur->get_name())) ) 
                {
                    semant_error()<<"Multiple definitions for method "<<  _feature->get_name() << " in class " << cur->get_name() << ".\n"; 
                }
            }  
            if (_feature->get_construct_type() == AttributeType) {   
                // Check the type declared for the attribute is valid
                Symbol type_decl = _feature->get_type_decl(); 
                if (!scope_manager->check_defined_in_global_scope(type_decl)) {
                    semant_error(cur)<<" classtable constructor - type " << type_decl << " doesn't exist.\n";
                }                
                if (_feature->get_name() == self) {
                    semant_error(cur)<<"self cannot be an attribute\n";
                    return;
                }  
                if ( !(this->_scope_manager->add_attribute(
                    _feature->get_name(),                     
                    cur->get_name())) ) 
                {                    
                    semant_error(cur)<<"Multiple definitions for attribute "<<  _feature->get_name() << " in class " << cur->get_name() << ".\n"; 
                }
                if (this->_scope_manager->check_attribute_redefinition(_feature->get_name(), cur->get_name())) {
                    //  Attribute moo is an attribute of an inherited class.
                    semant_error(cur)<<"Attribute "<<  _feature->get_name() << " is an attribute of an inherited class.\n"; 
                }
            }      
        }       
	}    
	
    // 3) For classes that have parents, those parents must exist in the global scope
    for(classes_idx = 0; classes_idx < classes->len(); classes_idx++) {
        Class_ cur = classes->nth(classes_idx);
        if (cur->is_default()) {
            continue;
        }
        Symbol parent = cur->get_parent();  
        // Add checks to go to the root.
        // If we can reach the root (Ojbect) then we're sure we don't have cycles
        if (G_DEBUG) {
            cout << " PARENT: " << parent << endl;
        }
        if (! (this->_scope_manager->find_symbol(parent)) ) {
            semant_error(cur)<<"Class " << cur->get_name() << " is inheriting from " << parent << ", which doesn't exist\n";
            return;
        }     
    }  

    // Check the formals of defined methods
    for(classes_idx = 0; classes_idx < classes->len(); classes_idx++) { 
        Class_ cur =  classes->nth(classes_idx);
        if (cur->is_default()) {
            continue;
        }  
        int feature_idx = 0;    
        Features _features = cur->get_features();    
        for(feature_idx = 0; feature_idx < _features->len(); feature_idx++) {           
            Feature _feature = _features->nth(feature_idx);                          
            if (_feature->get_construct_type() == MethodType) {
                method_class *_method = (method_class *)_feature;
                Formals _formals = _method->get_formals();
                int formal_idx = 0;
                for(formal_idx = 0; formal_idx < _formals->len(); formal_idx++) {
                    Formal _formal = _formals->nth(formal_idx);        
                    Symbol name = _formal->get_name();
                    Symbol type_decl = _formal->get_type_decl();
                    if (!scope_manager->check_defined_in_global_scope(type_decl)) {
                        semant_error(cur)<<"classtable constructor formal -- type " << type_decl << " doesn't exist.\n";
                    }
                    if (name == self) {
                        semant_error(cur)<<"formal name cannot be self.\n";
                    }

                    scope_manager->enter_scope("check_formals"); // We leverage the scope manager to check for multiple definitions
                    if (!scope_manager->add_symbol(name, type_decl, NULL)) {
                        semant_error(cur)<<"Multiple definitions of formal " << name << ".\n";
                    }                    
                    scope_manager->exit_scope();
                }   
            }
        }
    }

    
    // Reaching this place means everything looks good with global types
    // Let's add all classes to a map based on its Symbol Name
    for(classes_idx = 0; classes_idx < this->_classes->len(); classes_idx++) { 
        Class_ cur =  this->_classes->nth(classes_idx);
        this->_scope_manager->ClassMap[cur->get_name()] = cur;   
    }


    // Check method redefinitions are ok
    for(classes_idx = 0; classes_idx < classes->len(); classes_idx++) { 
        Class_ cur =  classes->nth(classes_idx);
        if (cur->is_default()) {
            continue;
        }  
        int feature_idx = 0;    
        Features _features = cur->get_features();    
        for(feature_idx = 0; feature_idx < _features->len(); feature_idx++) {           
            Feature _feature = _features->nth(feature_idx);                          
            if (_feature->get_construct_type() == MethodType) {
                method_class *_method = (method_class *)_feature;
                // We also need to check if we're redefining a method from a parent
                // If this is the case, the formals must be the exact same type
                // check_for_method_redefinition  
                    if (G_DEBUG) {
                        cout << "Check for method redefinition: " << endl;
                    }  
                    Symbol redefined;
                    Symbol original;
                    int errType = 0;
                    if (!scope_manager->check_valid_method_redefinition(
                        cur,
                        _method,
                        &original,
                        &redefined,
                        &errType)) {
                            switch(errType) {
                                case 1:
                                    semant_error(cur)<<" In redefined method "<<  _method->get_name() << ", parameter type " << redefined << " is different from original type " << original << ".\n"; 
                                    break;
                                case 2:
                                    semant_error(cur)<<"Incompatible number of formal parameters in redefined method " << _method->get_name() << ".\n"; 
                                    break;
                            }                        
                    }
            }
        }
    }

    
}

Classes ClassTable::install_basic_classes(Classes classes) {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(xcopy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);
    Object_class->set_default();
    classes = classes->append(classes, classes->single(Object_class));    
    this->_scope_manager->add_default_symbol(Object, Object, No_class);
    this->_scope_manager->add_method(cool_abort, Object, Object);
    this->_scope_manager->add_method(type_name, Str, Object);
    this->_scope_manager->add_method(xcopy, SELF_TYPE, Object);
    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    IO_class->set_default();
    classes = classes->append(classes, classes->single(IO_class));
    this->_scope_manager->add_default_symbol(IO, IO, Object);
    this->_scope_manager->add_method(out_string, SELF_TYPE, IO);
    this->_scope_manager->add_method(out_int, SELF_TYPE, IO);
    this->_scope_manager->add_method(in_string, Str, IO);
    this->_scope_manager->add_method(in_int, Int, IO);

    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    Int_class->set_default();
    classes = classes->append(classes, classes->single(Int_class));
    this->_scope_manager->add_default_symbol(Int, Int, Object);
    this->_scope_manager->add_attribute(val, Int);
    
    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class = 
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
    Bool_class->set_default();
    classes = classes->append(classes, classes->single(Bool_class));
    this->_scope_manager->add_default_symbol(Bool, Bool, Object);
    this->_scope_manager->add_attribute(val, Bool);

    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);
    Str_class->set_default();
    classes = classes->append(classes, classes->single(Str_class));
    this->_scope_manager->add_default_symbol(Str, Str, Object);
    this->_scope_manager->add_method(length, Int, Str);
    this->_scope_manager->add_method(concat, Str, Str);
    this->_scope_manager->add_method(substr, Str, Str);    
    this->_scope_manager->add_attribute(val, Str);
    this->_scope_manager->add_attribute(str_field, Str);
    return classes;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   
    This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
*/

bool check_inheritance_chain(ScopeManager *scope_manager, Symbol type1, Symbol type2) {
    // Checks that Type2 is equals or is a subtype of Type1
    // type1 >= type2?
    if (type2 == No_class) {
        return false;
    }
    if (type1 == type2) {
        return true;
    } 
    if (type2 == SELF_TYPE){
        return true;
    }   
    ScopeSymbol *ss = scope_manager->find_in_global_scope(type2);    
    
    if ( !ss ) {
        cout << "check_inheritance_chain -> this shouldn't happen: " << type2 << endl;
        exit(-1);
    }
    
    Symbol sx = ss->inherit_parent;    
    return check_inheritance_chain(scope_manager, type1, sx);
}

Symbol least_upper_bound(ScopeManager *scope_manager, Symbol type1, Symbol type2) {
    if (G_DEBUG) {
        cout << "LUB - Type1: " << type1 << " :: Type2: " << type2 << endl;
    }

    if (type1 == SELF_TYPE) {
        type1 = scope_manager->get_current_class()->get_name();
    }
    if (type2 == SELF_TYPE) {
        type2 = scope_manager->get_current_class()->get_name();
    }

    if (type1 == type2) {
        return type1;
    }

    if (type1 == Object || type2 == Object) {
        return Object;
    }

    std::vector<Symbol> inheritance_chain1, inheritance_chain2;
    inheritance_chain1.push_back(type1);
    inheritance_chain2.push_back(type2);
    Symbol sx = type1;
    while(sx != Object) {
        ScopeSymbol *ss = scope_manager->find_in_global_scope(sx);
        inheritance_chain1.push_back(ss->inherit_parent);
        sx = ss->inherit_parent;
    }
    sx = type2;
    while(sx != Object) {
        ScopeSymbol *ss = scope_manager->find_in_global_scope(sx);
        inheritance_chain2.push_back(ss->inherit_parent);
        sx = ss->inherit_parent;
    }
    for (std::vector<Symbol>::iterator it1 = inheritance_chain1.begin(); it1 != inheritance_chain1.end(); ++it1) { 
        for (std::vector<Symbol>::iterator it2 = inheritance_chain2.begin(); it2 != inheritance_chain2.end(); ++it2) { 
            if (*it1 == *it2) {
                return *it1;
            }
        }
    }
    return Object;
}

Symbol expression_semant(ClassTable *classtable, Expression expr);
Symbol case_semant(ClassTable *classtable, branch_class *branch)  {
    ScopeManager *scope_manager = classtable->get_scope_manager();
    Symbol type = branch->get_type_decl();
    if (!scope_manager->check_defined_in_global_scope(type)) {
        classtable->semant_error(scope_manager->get_current_class())<<"Branch " << type << " is not a valid.\n";
    }
    scope_manager->enter_scope(branch->get_name()->get_string());
    scope_manager->add_symbol(branch->get_name(), type, NULL);              
    Symbol res = expression_semant(classtable, branch->get_expr());
    scope_manager->exit_scope();
    return res;
}

void get_method_from_class_hierarchy(ClassTable *classtable, Symbol className, Symbol methodName, method_class **r_method) {    
    Classes classes = classtable->get_classes();
    Class_ _class = NULL;
    // cout << "class: "<<className<< " -- Method: " << methodName<< endl;
    int j = 0;
    for(j=0; j < classes->len(); j++) {       
        if(classes->nth(j)->get_name() == className) {
            _class = classes->nth(j);
            break;
        }
    }
    
    Features _features = _class->get_features();    
    for(j=0; j < _features->len(); j++) {
        Feature _feature = _features->nth(j);
        if (_feature->get_construct_type() == MethodType) {
            if (_feature->get_name() == methodName) {
                *r_method = (method_class *)_feature;
            }
        }        
    }
    // cout << "class: "<<className<< " -- Method: " << methodName<< endl;
    if (!*r_method) {
        // Check in parents!
        get_method_from_class_hierarchy(classtable, _class->get_parent(), methodName, r_method);
    }    
}

void formals_semant(ClassTable *classtable, Symbol methodName, Symbol className, Expressions actuals) {
    // Check that the actual parameters being passed are in conformance with the method definition
    ScopeManager *scope_manager = classtable->get_scope_manager();
    
    method_class *_method = NULL;
    get_method_from_class_hierarchy(classtable, className, methodName, &_method);    
    if (!_method) {
        cout <<  "This shouldn not ever happen xDDD" << endl;
        exit(-1);
    }    

    if (G_DEBUG) {
        cout << "CHECKING FORMALS " << endl;
    }

    Formals _formals = _method->get_formals();
    if(actuals->len() != _formals->len()) {
        classtable->semant_error(scope_manager->get_current_class())<<"Method " << methodName  << " called with the wrong number of arguments.\n";
    }
    int j = 0;
    for(j = 0; j < _formals->len(); j++) {
        Formal _formal = _formals->nth(j);
        Symbol formalType = _formal->get_type_decl();
        Expression actual = actuals->nth(j);
        Symbol actualType = expression_semant(classtable, actual);        
        Symbol type_to_check = actualType;
        if (actualType == SELF_TYPE) {
            type_to_check = scope_manager->get_current_class()->get_name();
        }
        if (!check_inheritance_chain(scope_manager, formalType, type_to_check)) {
            classtable->semant_error(scope_manager->get_current_class())<<"In call of method "<< methodName << ", type "<< actualType <<" of parameter "<< _formal->get_name() << " does not conform to declared type "<< formalType <<".\n";
        }
    }

}

Symbol expression_semant(ClassTable *classtable, Expression expr) {
    ScopeManager * scope_manager = classtable->get_scope_manager();
    //cout << "Expression Type: " << expr->get_construct_type() << endl;
    switch(expr->get_construct_type()) {     
        case AssignType: {                
                Symbol name = ((assign_class *) expr)->get_name();
                if (name == self) {
                    classtable->semant_error(scope_manager->get_current_class())<<"Cannot assign to 'self'.\n";
                    expr->set_type(No_type);
                    return No_type;
                }
                Expression e1 = ((assign_class *) expr)->get_expr(); 
            
                Symbol type = expression_semant(classtable, e1);               

                ScopeSymbol *ss = scope_manager->find_symbol(name);    
                if ( !ss ) {
                    cout << "Class: " << scope_manager->get_current_class()->get_name() << endl;
                    cout << "Type:" << name << endl;
                    cout << "AssignType -> this shouldn't happen" << endl;
                    exit(-1);
                }

                if (!check_inheritance_chain(scope_manager, ss->type, type)) {
                    classtable->semant_error(scope_manager->get_current_class())<<"Error1.\n";
                }  
                expr->set_type(type);
                return type;                            
                break;
            }
        case StaticDispathType: {
                // expression '@' TYPEID '.' OBJECTID '(' parameters ')'	 {$$ = static_dispatch($1,$3,$5,$7);}
                // For example, e@B.f() invokes the method f in class B on the object that is the value of e.
                static_dispatch_class *dispatch_expr = (static_dispatch_class *) expr;
                Expression _expr =  dispatch_expr->get_expr();
                Symbol type = expression_semant(classtable, _expr);
                Symbol type_to_check = type;
                if (type == SELF_TYPE) {
                    type_to_check = scope_manager->get_current_class()->get_name();
                }
                Symbol parentType = dispatch_expr->get_type_name();
                if (!scope_manager->check_defined_in_global_scope(parentType)) {
                    classtable->semant_error(scope_manager->get_current_class())<<"Type " << parentType << " is not a valid Class.\n";
                }
                if (!check_inheritance_chain(scope_manager, parentType, type_to_check)) {
                    classtable->semant_error(scope_manager->get_current_class())<<"Expression type " << type_to_check << " does not conform to declared static dispath type  " << parentType << ".\n";
                }
                Symbol ret;
                Symbol name = dispatch_expr->get_name();
                if (!scope_manager->lookup_method_in_inheritance_chain(name, parentType, &ret)) {
                    classtable->semant_error(scope_manager->get_current_class())<<"Dispatch to undefined method " << name << ".\n";
                    expr->set_type(No_type);
                    return No_type;
                }

                Expressions _parameters = dispatch_expr->get_actual();
                formals_semant(classtable, name, parentType, _parameters);
                Symbol res = ret;
                if (ret == SELF_TYPE) {
                    res = type_to_check;
                }
                expr->set_type(res);
                return res;                        
            }
        case DispatchType: {
            /*
            expression '.' OBJECTID '(' parameters ')'  {$$ = dispatch($1,$3,$5);}
            OBJECTID '(' parameters ')'                 {$$ = dispatch(object(idtable.add_string("self")), $1, $3);}
            */
                dispatch_class *dispatch_expr = (dispatch_class *) expr;

                Expression _expr =  dispatch_expr->get_expr();
                if (G_DEBUG) {
                    cout << "Dispatch - expr: " << _expr << endl;
                }

                Symbol type = expression_semant(classtable, _expr);
                if (type == SELF_TYPE) {
                    type = scope_manager->get_current_class()->get_name();
                }
                Symbol ret;
                Symbol name = dispatch_expr->get_name();
                
                if (G_DEBUG) {
                    cout << "Dispatch - method: " << name << endl;
                }

                if (!scope_manager->lookup_method_in_inheritance_chain(name, type, &ret)) {
                    classtable->semant_error(scope_manager->get_current_class())<<"Dispatch to undefined method " << name << ".\n";
                    expr->set_type(No_type);
                    return No_type;
                }  

                if (G_DEBUG) {
                    cout << "Dispatch - method: " << name << " classname: " << type << " retType: " << ret << endl;
                }            

                Expressions _parameters = dispatch_expr->get_actual();
                formals_semant(classtable, name, type, _parameters);

                if (G_DEBUG) {
                    cout << "Dixxxxxxxxxxxx "   << endl;
                }
                
                Symbol res = ret;
                if (ret == SELF_TYPE && scope_manager->get_current_class()->get_parent() == Object) { // hack
                    res = type;
                }
                expr->set_type(res);
                return res;                
            }
        case ConditionalType: {
                if (G_DEBUG) {
                    cout << " ConditionalType" << endl;
                }
                cond_class *cond_expr = (cond_class *) expr;
                Expression pred = cond_expr->get_pred();
                Expression then_exp = cond_expr->get_then_exp();
                Expression else_exp = cond_expr->get_else_exp();
                Symbol type = expression_semant(classtable, pred);
                if (type != Bool) {
                    classtable->semant_error(scope_manager->get_current_class())<<"Conditional predicate is not Bool\n";
                }
                Symbol type1 = expression_semant(classtable, then_exp);
                Symbol type2 = expression_semant(classtable, else_exp);
                if (G_DEBUG) {
                    cout << " About to least_upper_bound" << endl;
                }
                Symbol res = least_upper_bound(scope_manager, type1, type2);
                if (G_DEBUG) {
                    cout << " After least_upper_bound" << endl;
                }
                expr->set_type(res);                
                return res; 
            }
        case LoopType: {
                loop_class *loop_expr = (loop_class *) expr;
                Symbol type = expression_semant(classtable, loop_expr->get_pred());
                if (type != Bool) {
                    classtable->semant_error(scope_manager->get_current_class())<<"Loop condition does not have type Bool.\n";
                }
                expression_semant(classtable, loop_expr->get_body());
                //cout << "LOOP no type" << endl;
                expr->set_type(Object);
                return Object;            
            }
        case CaseType: {
                /*
                    Expression typcase(Expression expr, Cases cases) 

                    CASE expression OF case_list ESAC   { $$ = typcase($2, $4); }

                    case <expr0> of
                        <id1> : <type1> => <expr1>;
                        . . .
                        <idn> : <typen> => <exprn>;
                    esac

                      case_list:
                        case
                            { $$ = single_Cases($1); }
                            |	case_list case	
                            { $$ = append_Cases($1,single_Cases($2)); }
                        | { $$ = nil_Cases(); }
                            ;
                */
                Expression expr1 = ((typcase_class *) expr)->get_expr();
                expression_semant(classtable, expr1);
                Cases cases = ((typcase_class *) expr)->get_cases();                
               
                map<Symbol, int> type_ocurrences;
                std::vector<Symbol> returnTypes;
                Symbol ret;
                for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
                    Case c = cases->nth(i);                     
                    ret = case_semant(classtable, (branch_class *) c);
                    returnTypes.push_back(ret);
                    Symbol s = ((branch_class *) c)->get_type_decl();    
                    map<Symbol, int>::iterator it(type_ocurrences.find(s));
                    if (it != type_ocurrences.end()) {
                        it->second++;
                        classtable->semant_error(scope_manager->get_current_class())<< "Duplicate branch " << s << " in case statement.\n";
                    } else {
                        type_ocurrences[s] = 1;
                    }    
                }

                for(std::vector<Symbol>::iterator it = returnTypes.begin(); it != returnTypes.end(); ++it) {                    
                    ret = least_upper_bound(scope_manager, *it, ret);
                    //cout << "Branch type: " << *it  << " LUB: " << ret << endl;
                }

                expr->set_type(ret);
                return ret;
                break;
            }
        case BlockType: {
                Expressions body = ((block_class *) expr)->get_body();
                int expr_idx = 0;
                Symbol type;
                for(expr_idx = 0; expr_idx < body->len(); expr_idx++) {
                    type = expression_semant(classtable, body->nth(expr_idx));
                }
                //cout << "Block code return type " << type << endl;
                expr->set_type(type);
                return type;
                break;
            }
        case LetType: {
                /*
                    let ID : TYPE in expr
                    let ID : TYPE <- expr in expr
                    let ID : TYPE <- expr, ID : TYPE in expr
                    let ID : TYPE <- expr, ID : TYPE <- expr in expr

                    let(Symbol identifier, Symbol type_decl, Expression init, Expression body)
                */ 
                let_class * let_expr = ((let_class *)expr);
                Symbol identifier = let_expr->get_identifier();
                Symbol type = No_type;
                Symbol type2;
                Symbol type3 = No_type;
                if ( identifier == self ) {
                    classtable->semant_error(scope_manager->get_current_class())<<"'self' cannot be bound in a 'let' expression.\n";                    
                } else {
                    type = let_expr->get_type_decl();
           
                    type2 = expression_semant(classtable, let_expr->get_init());                    
                    if (type2 != No_type && !check_inheritance_chain(scope_manager, type, type2)) {
                        classtable->semant_error(scope_manager->get_current_class())<<"Bad initialization for let expression\n";
                    }

                    scope_manager->enter_scope("let_expr");
                    scope_manager->add_symbol(identifier, type, NULL);
                    type3 = expression_semant(classtable, let_expr->get_body());
                    scope_manager->exit_scope();
                }
                //cout << "LET return Type: " << type << endl;
                expr->set_type(type3);
                return type3;
            }

        case PlusType: {
                Symbol type1 = expression_semant(classtable, ((plus_class *)expr)->get_e1());
                Symbol type2 = expression_semant(classtable, ((plus_class *)expr)->get_e2());
                if(type1 != Int || type2 != Int) {
                    classtable->semant_error(scope_manager->get_current_class())<<"Plus with non Int Expressions.\n";
                }
                expr->set_type(Int);
                return Int;
            }
        case SubType: {
                Symbol type1 = expression_semant(classtable, ((sub_class *)expr)->get_e1());
                Symbol type2 = expression_semant(classtable, ((sub_class *)expr)->get_e2());
                if(type1 != Int || type2 != Int) {
                    classtable->semant_error(scope_manager->get_current_class())<<"Sub with non Int Expressions.\n";
                }
                expr->set_type(Int);
                return Int;
            }
        case MulType: {
                Expression e1 = ((mul_class *) expr)->get_e1();
                Expression e2 = ((mul_class *) expr)->get_e2();
                Symbol type1 = expression_semant(classtable, e1);
                Symbol type2 = expression_semant(classtable, e2);                
                if (type1 != Int || type2 != Int) {
                    //cout << "Are we dragons?? "<< endl;
                    classtable->semant_error(scope_manager->get_current_class())<<"Mul - Both legs exps are not Int type.\n";              
                }
                expr->set_type(Int);
                return Int;
                break;
            }
        case DivideType: {
                Symbol type1 = expression_semant(classtable, ((divide_class *)expr)->get_e1());
                Symbol type2 = expression_semant(classtable, ((divide_class *)expr)->get_e2());
                if(type1 != Int || type2 != Int) {
                    classtable->semant_error(scope_manager->get_current_class())<<"Division with non Int Expressions.\n";
                }
                expr->set_type(Int);
                return Int;
            }
        case NegType: {
                Symbol type1 = expression_semant(classtable, ((neg_class *)expr)->get_e1());
                if(type1 != Int) {
                    classtable->semant_error(scope_manager->get_current_class())<<"Neg - expression is not Int type.\n";
                }
                expr->set_type(Int);
                return Int;
            }
        case LtType: {
                Symbol type1 = expression_semant(classtable, ((lt_class *)expr)->get_e1());
                Symbol type2 = expression_semant(classtable, ((lt_class *)expr)->get_e2());
                if(type1 != Int || type2 != Int) 
                {
                    classtable->semant_error(scope_manager->get_current_class())<<"Lt Both leqs exps are not Int type.\n";
                }
                expr->set_type(Bool);
                return Bool; 
            }        
        case EqType: {
                Symbol type1 = expression_semant(classtable, ((eq_class *)expr)->get_e1());
                Symbol type2 = expression_semant(classtable, ((eq_class *)expr)->get_e2());
                /*
                The wrinkle in the rule for equality is that any types may be freely compared except Int, String
                and Bool, which may only be compared with objects of the same type.
                */
                if( !(((type1 == Int || type1 == Str || type1 == Bool) && type1 == type2)
                    ||  (type1 != Int && type1 != Str && type1 != Bool && type2 != Int && type2 != Str && type2 != Bool) ))
                {
                    //cout << "Type1: " << type1 << endl;
                    //cout << "Type2: " << type2 << endl;
                    classtable->semant_error(scope_manager->get_current_class())<<"Eq  Both leqs exps are not the same type.\n";
                }
                expr->set_type(Bool);
                return Bool;                
            }
        case LeqType: {
                Symbol type1 = expression_semant(classtable, ((leq_class *)expr)->get_e1());
                Symbol type2 = expression_semant(classtable, ((leq_class *)expr)->get_e2());
                if(type1 != Int || type2 != Int) {
                    classtable->semant_error(scope_manager->get_current_class())<<"Leq Both leqs exps are not Int type.\n";
                }
                expr->set_type(Bool);
                return Bool;
                break;
            }
        case CompType: {
            /*
            The expression not <expr> is the boolean complement of <expr>. The expression
            <expr> must have static type Bool and the entire expression has static type Bool.
            */
                Symbol type = expression_semant(classtable, ((comp_class *)expr)->get_e1());
                expr->set_type(type);
                return type;
                break;
            }
        case IntConstType: {
                expr->set_type(Int);
                return Int;
                break;
            }
        case BoolConstType:
            expr->set_type(Bool);
            return Bool;
            break;
        case StringConstType:
            expr->set_type(Str);
            return Str;
            break;
        case NewType: {
                Symbol type = ((new__class *)expr)->get_type_name();
                if (type == SELF_TYPE) {
                    expr->set_type(SELF_TYPE);
                    return type; // CHECK THIS
                }
                ScopeSymbol *ss = scope_manager->find_in_global_scope(type);
                if (!ss) {
                    classtable->semant_error(scope_manager->get_current_class())<<"NewType " << type << " is not defined.\n";
                }
                expr->set_type(ss->type);
                return ss->type;
                break;
            }
        case IsVoidType: {                        
            expression_semant(classtable, ((isvoid_class *)expr)->get_e1());
            expr->set_type(Bool);
            return Bool;
            break;
        }
        case NoExprType:
            //cout << "NoExprType no type" << endl;
            expr->set_type(No_type);
            return No_type;
            break;
        case ObjectType:  {
                Symbol name = ((object_class *) expr)->get_name();                
                if (name == self) {
                    expr->set_type(SELF_TYPE);
                    return SELF_TYPE;
                }
                ScopeSymbol *ss = scope_manager->find_symbol(name);
                if (!ss) {
                    //  Undeclared identifier
                    if (G_DEBUG) {
                        cout << "Class: " << scope_manager->get_current_class()->get_name() << endl;
                    }
                    classtable->semant_error(scope_manager->get_current_class())<<" Undeclared identifier " << name << ".\n";
                    expr->set_type(No_type);
                    return No_type;
                } else {                
                    expr->set_type(ss->type);
                    return ss->type;
                }
                break;
            }
        default: {
            cout << "Default? this shouldn't happen" << endl;
            return No_type;
        } 
    }
}

void attribute_semant(ClassTable *classtable, attr_class *_attribute) {    
    /*
     At this point we have already checked:
        - that type is not Self
        - That type exists in the global scope
        - The attribute defined is not a redefinition of an attribute in the parent
    The only thing left is:
     - to typecheck the expression against the declared type
    */
    //cout << "Attribute - name: " << _attribute->get_name() << " Type: " << _attribute->get_type_decl() << endl;
    ScopeManager *scope_manager = classtable->get_scope_manager();
    Symbol type = _attribute->get_type_decl(); 

    if (G_DEBUG) {
        cout << "Class: " << scope_manager->get_current_class()->get_name() << "  - Attr: " << _attribute->get_name() << " - Type: " << type << endl;
        scope_manager->print_current_scope();
    }   

    Expression expr = _attribute->get_init();    
    Symbol expr_type = expression_semant(classtable, expr);

    if (expr_type != No_type && !check_inheritance_chain(scope_manager, type, expr_type)) {
        classtable->semant_error(scope_manager->get_current_class())<<"Type " << expr_type << " is not subclass of " << type << ".\n";
    }    
} 

void method_semant(ClassTable *classtable, method_class *_method) {
    if (G_DEBUG) {
        cout << "Method Here: " << _method->get_name() << " - " << _method << endl;
    }
    // Check formals and add them to the method scope
    
    ScopeManager *scope_manager = classtable->get_scope_manager();

    scope_manager->enter_scope(_method->get_name()->get_string());
    Formals _formals = _method->get_formals();
    int formal_idx = 0;
    for(formal_idx = 0; formal_idx < _formals->len(); formal_idx++) {
        Formal _formal = _formals->nth(formal_idx);        
        Symbol name = _formal->get_name();
        Symbol type_decl = _formal->get_type_decl();
        if (!scope_manager->check_defined_in_global_scope(type_decl)) {
            classtable->semant_error(scope_manager->get_current_class())<<"method semant - type " << type_decl << " doesn't exist.\n";
        }
        if (name == self) {
            classtable->semant_error(scope_manager->get_current_class())<<"formal name cannot be self.\n";
        }
        if (!scope_manager->add_symbol(name, type_decl, NULL)) {
            classtable->semant_error(scope_manager->get_current_class())<<"Multiple definitions of formal " << name << ".\n";
        }                    
    }

    Symbol expressionType = expression_semant(classtable, _method->get_expression());    
    // Finally We get the returned type and check it against the defined ReturnType of the method   
    Symbol returnType = _method->get_return_type();
    if (expressionType!= No_type && !check_inheritance_chain(scope_manager, returnType, expressionType)) {
        classtable->semant_error(scope_manager->get_current_class())<<"Inferred return type " << expressionType << " of method " << _method->get_name() << " does not conform to declared return type " <<  returnType << ".\n";
    }
   // if ( _method->get_return_type() != Object)
    //_method->get_expression()->set_type(_method->get_return_type());

    classtable->get_scope_manager()->exit_scope();    
} 

void start_semant(ClassTable *classtable) {
    /*
        - We enter into a new scope for each class
        - Classes are made up of features
        - Features can be either attributes or methods
        - methods were already added into the method list (in scope_manager) at a previous step
        - We loop the features once and add all the attributes defined for the class
        - Then we step into each feature and check the semant of the expressions that compose them
    */
    Classes classes = classtable->get_classes();
    ScopeManager *scope_manager = classtable->get_scope_manager();
    int classes_idx;
    for(classes_idx = 0; classes_idx < classes->len(); classes_idx++) {

        Class_ cur = classes->nth(classes_idx);  
        if (cur->is_default()) {
            continue;
        }
        // For each class, we need to take into account its inheritance chain in the scope
        // so walk the inheritance chain and do enter scope for each parent up to the No_Class
        // While we do this, we need to add the attributes of corresponding parents into the scope
        Class_ cx = cur;
        int inheritance_depth = 0;
        while (cx->get_parent() != No_class) {
            Symbol parent = cx->get_parent();

            if (parent == Bool || parent == Str) {
                classtable->semant_error(cx) << "Class " << cur->get_name() << " cannot inherit class " << parent << ".\n";
            }

            cx = scope_manager->ClassMap[parent];
            //cout << "parent: " << cx << endl;
            inheritance_depth++;
            scope_manager->enter_scope(cx->get_name()->get_string()); 
            // Now walk the features attributes and add them to scope
            int feature_idx = 0;                
            Features _features = cx->get_features();        
            for(feature_idx = 0; feature_idx < _features->len(); feature_idx++) {
                Feature _feature = _features->nth(feature_idx);            
                if (_feature->get_construct_type() == AttributeType) {   
                // Add it to the scope
                if ( !(scope_manager->add_symbol(_feature->get_name(), _feature->get_type_decl(), NULL)) ) {
                        classtable->semant_error(cx)<<"Multiple definitions of attribute " << _feature->get_name() << " in class "<< cur->get_name() << ".\n";
                    }         
                }                       
            }     
            // mostSpecific -> Most Generic ---> 
            /// A inherits B  | B inherits C
            ///    [A B  C]   C B A     
            // This could be a prpoblem if we had attribute redefinition
            // As we don't this is NO ISSUE
        }

        // Now we enter scope for the current class!
        scope_manager->enter_scope(cur->get_name()->get_string()); 
        scope_manager->set_current_class(cur);

        int feature_idx = 0;                
        Features _features = cur->get_features();        
        for(feature_idx = 0; feature_idx < _features->len(); feature_idx++) {
            Feature _feature = _features->nth(feature_idx);            
            if (_feature->get_construct_type() == AttributeType) {   
               // Add it to the scope
               if (G_DEBUG) {
                   cout << "In class: " << cur->get_name() << "  - adding " << _feature->get_name() << " to scope" << endl;
               }
               if ( !(scope_manager->add_symbol(_feature->get_name(), _feature->get_type_decl(), NULL)) ) {
                    classtable->semant_error(cur)<<"Multiple definitions of attribute " << _feature->get_name() << " in class "<< cur->get_name() << ".\n";
                }         
            }                       
        }

        // At this point we have our current Class scope filled with attributes defined 
        // We're ready to do semantic checks over the expressions 
        for(feature_idx = 0; feature_idx < _features->len(); feature_idx++) {           
            Feature _feature = _features->nth(feature_idx);  
            if (G_DEBUG) {
                cout << "YYY - " << _feature->get_name() <<  endl;
            }         
            if (_feature->get_construct_type() == AttributeType) {                
                attribute_semant(classtable, (attr_class *) _feature);
            }                        
            else {                               
                method_semant(classtable, (method_class *) _feature);
            } 
            if (G_DEBUG) {
                cout << "XXXX" << endl;
            }              
        }
        //cout << "exiting class scope" << endl;
        scope_manager->exit_scope(); // exit current class scope
        //cout << "exiting zzxczxce" << endl;
        while(inheritance_depth > 0) {
            scope_manager->exit_scope(); // exit the parent scope
            inheritance_depth--;
        }
    }
    //cout << "wrapping up" << endl;
    //scope_manager->print_method_list();  
}


void program_class::semant()
{
    ScopeManager *scope_manager = new ScopeManager();

    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(scope_manager, classes);

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    start_semant(classtable);

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

// line 68
// line 475 different
// Lines 489, 490, 491
// 599 , 600