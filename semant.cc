#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <iostream>
#include <sstream>
#include <set>

#define PRINTLN(x) if(0)std::cout<<x<<std::endl
extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
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
    copy        = idtable.add_string("copy");
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

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    /* add basic classes */
    install_basic_classes();

    /* add defined classes */
    for(int i=classes->first(); classes->more(i); i=classes->next(i)){
        auto cls = classes->nth(i);
        Symbol name = cls->get_name();
        PRINTLN("Found class: " << name);
        if(classMap.find(name) == classMap.end()) // class not defined
            classMap[name] = cls;
        else // error in redefined class
            semant_error(cls) << "ERROR: class redefined" << std::endl;
    }
    PRINTLN("Total number of class: " << classMap.size());

    // check Main class and main method
    if(classMap.find(Main) != classMap.end()){
        bool ok = false;
        auto m = classMap[Main];
        auto fs = m->get_features();
        for(int i=fs->first();  fs->more(i); i=fs->next(i)){
            auto meth = fs->nth(i);
            if(meth->is_method() && meth->get_name() == main_meth)
                ok = true;
        }
        if(!ok) semant_error() << "no main method in Main class" << std::endl;
    } else semant_error() << "Class Main is not defined." << std::endl;

    // check circle
    std::set<Symbol> saw;
    for(auto& kv : classMap){
        auto cur = kv.second;
        saw.insert(cur->get_name());
        while(cur->get_name() != Object){
            Symbol par_name = cur->get_parent();
            if(par_name == SELF_TYPE) break; // hack
            if(classMap.find(par_name) == classMap.end()){
                std::ostringstream err;
                err << "Fatal Error: " << cur->get_name() << "'s parent class "
                    << par_name << " is not defined" << std::endl;
                fatal_error((char*)(err.str().c_str())); // wired
                // semant_error(curm->lookup(c)) << "parent class is not defined" << std::endl;
                break;
            } else if(saw.find(par_name) != saw.end()){
                std::ostringstream err;
                err << "Fatal Error: " << "circle in " << cur->get_name() << "'s inherits" << std::endl;
                fatal_error((char*)(err.str().c_str())); // wired
                // semant_error(classMap[par_name]m->lookup(c)) << "circle in class hireichy" << std::endl;
                break;
            } else {
                saw.insert(par_name);
                cur = classMap[par_name];
            }
        }
        saw.clear();
    }
}

void ClassTable::install_basic_classes() {

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
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

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

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
	class_(Int,
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
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

    // insert into class map
    classMap[Object] = Object_class;
    classMap[IO] = IO_class;
    classMap[Int] = Int_class;
    classMap[Bool] = Bool_class;
    classMap[Str] = Str_class;
}

// Class_ ClassTable::operator[](Symbol name){ return classMap[name]; }
Class_ ClassTable::lookup(Symbol name){
    if(classMap.find(name) == classMap.end())
        return nullptr;
    else
        return classMap[name];
}

bool ClassTable::leq(const Symbol c, const Symbol p) {
    Symbol t = c;
    while(t != Object && t != p)
        t = classMap[t]->get_parent();
    return t == p;
}

Symbol ClassTable::lub(const Symbol a, const Symbol b) {
    std::set<Symbol> pa;
    Symbol t = a;
    while(1){
        pa.insert(t);
        if(t == Object)
            break;
        else
            t = classMap[t]->get_parent();
    }
    t = b;
    while(pa.find(t) == pa.end())
        t = classMap[t]->get_parent();
    return t;
}
////////////////////////////////////////////////////////////////////

Symbol loop_class::tc(SymTable *o, ClassTableP m, Symbol c){
    Symbol tpred = this->pred->tc(o, m, c);
    Symbol tbody = this->body->tc(o, m, c);
    if(tpred != Bool)
        m->semant_error(m->lookup(c)) << "loop pred" << std::endl;
    this->type = Object;
    return this->type;
}

Symbol branch_class::tc(SymTable *o, ClassTableP m, Symbol c){
    o->enterscope();
    o->addid(this->name, this->type_decl);
    Symbol t = this->expr->tc(o, m, c);
    o->exitscope();
    // this->type = t;
    // return this->type;
    return t;
}

Symbol typcase_class::tc(SymTable *o, ClassTableP m, Symbol c){
    Symbol te = this->expr->tc(o, m, c); // unused
    std::set<Symbol> cts;
    for(int i = cases->first(); cases->more(i); i = cases->next(i)){
        auto cas = cases->nth(i);
        Symbol t = cas->tc(o, m, c);
        if(cts.find(t) == cts.end())
            cts.insert(t);
        else
            m->semant_error(m->lookup(c)) << "typcase duplicate branch type" << std::endl;
    }
    Symbol t = nullptr;
    for(auto ct : cts)
        if(t == nullptr) t = ct;
        else t = m->lub(t, ct);
    this->type = t;
    return this->type;
}

Symbol assign_class::tc(SymTable *o, ClassTableP m, Symbol c){
    if(this->name == self){
        m->semant_error(m->lookup(c)) << "Cannot assign to 'self'." << std::endl;
        return c;
    }
    Symbol newt = this->expr->tc(o, m, c);
    // NOTE if there is old type, check leq
    if(o->lookup(this->name) != nullptr){
        Symbol oldt = o->lookup(this->name);
        if(!m->leq(newt, oldt))
            m->semant_error(m->lookup(c)) << "assign" << std::endl;
    }
    this->type = newt;
    o->addid(this->name, newt); // NOTE
    return this->type;
}

Symbol new__class::tc(SymTable *o, ClassTableP m, Symbol c){
    Symbol tn = this->type_name;
    if(tn == SELF_TYPE)
        this->type = SELF_TYPE; // NOTE
    else {
        if(m->lookup(tn) != nullptr)
            this->type = tn;
        else {
            m->semant_error(m->lookup(c)) << "new, cannot find class " << tn << std::endl;
            this->type = Object; // NOTE
        }
    }
    return this->type;
}

Symbol static_dispatch_class::tc(SymTable *o, ClassTableP m, Symbol c){
    Symbol t0 = this->expr->tc(o, m, c);
    // t0 == self
    if(t0 == SELF_TYPE) t0 = c; // NOTE
    // check t0 <= T
    if(!m->leq(t0, this->type_name)){
        m->semant_error(m->lookup(c)) << "static_dispatch, not static?" << std::endl;
        t0 = this->type_name;
    }
    // parameter types
    std::vector<Symbol> pts;
    for(int i=actual->first(); actual->more(i); i=actual->next(i)){
        pts.push_back(actual->nth(i)->tc(o, m, c));
    }
    // lookup method and check
    if(m->lookup(t0) == nullptr){
        m->semant_error(m->lookup(c)) << "static_dispatch, cannot find class " << t0 << std::endl;
        this->type = Object; // NOTE
        return this->type;
    }
    auto fs = m->lookup(t0)->get_features();
    method_class *meth = nullptr;
    for(int i=fs->first(); fs->more(i); i=fs->next(i)){
        auto f = fs->nth(i);
        if(f->is_method() && f->get_name() == this->name)
            meth = dynamic_cast<method_class *>(f);
    }
    if(meth == nullptr){
        m->semant_error(m->lookup(c)) << "static_dispatch, cannot find method " << name << " " << t0 << std::endl;
        this->type = Object; // NOTE
        return this->type;
    }
    // check method formals
    auto formals = meth->get_formals();
    bool ok = pts.size() == static_cast<size_t>(formals->len());
    auto it = pts.begin();
    for(int i=formals->first(); ok && formals->more(i); i=formals->next(i)){
        ok = m->leq(*it, formals->nth(i)->get_type());
    }
    if(!ok){
        m->semant_error(m->lookup(c)) << "static_dispatch, parameters types not match" << std::endl;
        this->type = Object; // NOTE
        return this->type;
    }
    // self_type in method's return type
    if(meth->get_type() == SELF_TYPE) // NOTE
        this->type = t0;
    else
        this -> type = meth->get_type();
    return this->type;
}

Symbol dispatch_class::tc(SymTable *o, ClassTableP m, Symbol c){
    Symbol t0 = this->expr->tc(o, m, c);
    // t0 == self
    if(t0 == SELF_TYPE) t0 = c;
    // parameter types
    std::vector<Symbol> pts;
    for(int i=actual->first(); actual->more(i); i=actual->next(i)){
        pts.push_back(actual->nth(i)->tc(o, m, c));
    }
    // lookup method and check
    if(m->lookup(t0) == nullptr){
        m->semant_error(m->lookup(c)) << "dispatch, cannot find class " << t0 << std::endl;
        this->type = Object; // NOTE
        return this->type;
    }
    auto fs = m->lookup(t0)->get_features();
    method_class *meth = nullptr;
    for(int i=fs->first(); fs->more(i); i=fs->next(i)){
        auto f = fs->nth(i);
        if(f->is_method() && f->get_name() == this->name)
            meth = dynamic_cast<method_class *>(f);
    }
    if(meth == nullptr){
        m->semant_error(m->lookup(c)) << "dispatch, cannot find method " << name << " " << t0 << std::endl;
        this->type = Object; // NOTE
        return this->type;
    }
    // check method formals
    auto formals = meth->get_formals();
    bool ok = pts.size() == static_cast<size_t>(formals->len());
    // PRINTLN("DIS " << pts.size() << " " << formals->len());
    auto it = pts.begin();
    for(int i=formals->first(); ok && formals->more(i); i=formals->next(i)){
        ok = m->leq(*it, formals->nth(i)->get_type());
        ++it;
    }
    if(!ok){
        m->semant_error(m->lookup(c)) << "dispatch, parameters types not match" << std::endl;
        this->type = Object; // NOTE
        return this->type;
    }
    // self_type in method's return type
    if(meth->get_type() == SELF_TYPE) // NOTE
        this->type = t0;
    else
        this -> type = meth->get_type();
    return this->type;
}

Symbol cond_class::tc(SymTable *o, ClassTableP m, Symbol c){
    Symbol tp = this->pred->tc(o, m, c);
    Symbol t1 = this->then_exp->tc(o, m, c);
    Symbol t2 = this->else_exp->tc(o, m, c);
    if(tp != Bool)
        m->semant_error(m->lookup(c)) << "cond pred" << std::endl;
    this->type = m->lub(t1, t2);
    return this->type;
}

Symbol block_class::tc(SymTable *o, ClassTableP m, Symbol c){
    if(body->len() == 0)
        this->type = No_type;
    else {
        Symbol last;
        for(int i=body->first(); body->more(i); i=body->next(i))
            last = body->nth(i)->tc(o, m, c);
        this->type = last;
    }
    return this->type;
}

Symbol let_class::tc(SymTable *o, ClassTableP m, Symbol c){
    Symbol t0 = this->type_decl;
    if(t0 == SELF_TYPE)
        t0 = c;
    if(m->lookup(t0) == nullptr)
        m->semant_error(m->lookup(c)) << "let not found class " << t0 << std::endl;
    Symbol t1 = this->init->tc(o, m, c);
    if(t1 != No_type && !m->leq(t1, t0)) // no init
        m->semant_error(m->lookup(c)) << "let not leq" <<std::endl;
    o->enterscope();
    o->addid(this->identifier, t0);
    Symbol t2 = this->body->tc(o, m, c);
    o->exitscope();
    this->type = t2;
    return this->type;
}

Symbol plus_class::tc(SymTable *o, ClassTableP m, Symbol c){
    // subexpressions
    Symbol t1 = this->e1->tc(o, m, c);
    Symbol t2 = this->e2->tc(o, m, c);
    // check
    if(t1 != Int || t2 != Int)
        m->semant_error(m->lookup(c)) << "plus" <<std::endl;
    // return type
    this->type = Int;
    return this->type;
}

Symbol sub_class::tc(SymTable *o, ClassTableP m, Symbol c){
    // subexpressions
    Symbol t1 = this->e1->tc(o, m, c);
    Symbol t2 = this->e2->tc(o, m, c);
    // check
    if(t1 != Int || t2 != Int)
        m->semant_error(m->lookup(c)) << "sub" <<std::endl;
    // return type
    this->type = Int;
    return this->type;
}

Symbol mul_class::tc(SymTable *o, ClassTableP m, Symbol c){
    // subexpressions
    Symbol t1 = this->e1->tc(o, m, c);
    Symbol t2 = this->e2->tc(o, m, c);
    // check
    if(t1 != Int || t2 != Int)
        m->semant_error(m->lookup(c)) << "mul" <<std::endl;
    // return type
    this->type = Int;
    return this->type;
}

Symbol divide_class::tc(SymTable *o, ClassTableP m, Symbol c){
    // subexpressions
    Symbol t1 = this->e1->tc(o, m, c);
    Symbol t2 = this->e2->tc(o, m, c);
    // check
    if(t1 != Int || t2 != Int)
        m->semant_error(m->lookup(c)) << "divide" <<std::endl;
    // return type
    this->type = Int;
    return this->type;
}

Symbol neg_class::tc(SymTable *o, ClassTableP m, Symbol c){
    // subexpressions
    Symbol t1 = this->e1->tc(o, m, c);
    // check
    if(t1 != Int)
        m->semant_error(m->lookup(c)) << "neg" <<std::endl;
    // return type
    this->type = Int;
    return this->type;
}

Symbol lt_class::tc(SymTable *o, ClassTableP m, Symbol c){
    // subexpressions
    Symbol t1 = this->e1->tc(o, m, c);
    Symbol t2 = this->e2->tc(o, m, c);
    // check
    if(t1 != Int || t2 != Int)
        m->semant_error(m->lookup(c)) << "lt" <<std::endl;
    // return type
    this->type = Bool;
    return this->type;
}

Symbol eq_class::tc(SymTable *o, ClassTableP m, Symbol c){
    // subexpressions
    Symbol t1 = this->e1->tc(o, m, c);
    Symbol t2 = this->e2->tc(o, m, c);
    // check
    if(!((t1 == Int && t2 == Int) || (t1 == Bool && t2 == Bool)))
        m->semant_error(m->lookup(c)) << "eq" <<std::endl;
    // return type
    this->type = Bool;
    return this->type;
}

Symbol leq_class::tc(SymTable *o, ClassTableP m, Symbol c){
    // subexpressions
    Symbol t1 = this->e1->tc(o, m, c);
    Symbol t2 = this->e2->tc(o, m, c);
    // check
    if(t1 != Int || t2 != Int)
        m->semant_error(m->lookup(c)) << "leq" <<std::endl;
    // return type
    this->type = Bool;
    return this->type;
}

Symbol comp_class::tc(SymTable *o, ClassTableP m, Symbol c){
    // subexpressions
    Symbol t1 = this->e1->tc(o, m, c);
    // check
    if(t1 != Bool)
        m->semant_error(m->lookup(c)) << "comp" <<std::endl;
    // return type
    this->type = Bool;
    return this->type;
}

Symbol int_const_class::tc(SymTable *o, ClassTableP m, Symbol c){
    this->type = Int;
    return this->type;
}

Symbol bool_const_class::tc(SymTable *o, ClassTableP m, Symbol c){
    this->type = Bool;
    return this->type;
}

Symbol string_const_class::tc(SymTable *o, ClassTableP m, Symbol c){
    this->type = Str;
    return this->type;
}

Symbol isvoid_class::tc(SymTable *o, ClassTableP m, Symbol c){
    Symbol t1 = this->e1->tc(o, m, c); // unused
    this->type = Bool;
    return this->type;
}

Symbol no_expr_class::tc(SymTable *o, ClassTableP m, Symbol c){
    this->type = No_type;
    return this->type;
}

Symbol object_class::tc(SymTable *o, ClassTableP m, Symbol c){
    if(this->name == self){
        this->type = SELF_TYPE;
    } else if(o->lookup(this->name) == nullptr){
        m->semant_error(m->lookup(c)) << "object " << this->name << " not found" <<std::endl;
        this->type = Object;
    } else
        this->type = o->lookup(this->name);
    return this->type;
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
///////////////////////////////////////////////////////////////////////

Symbol method_class::tc(SymTable *o, ClassTableP m, Symbol c){
    o->enterscope();
    for(int i=formals->first(); formals->more(i); i=formals->next(i)){
        auto f = formals->nth(i);
        if(f->get_name() == self)
            m->semant_error(m->lookup(c)) << "'self' cannot be the name of a formal parameter." << std::endl;
        o->addid(f->get_name(), f->get_type());
    }
    Symbol t = this->expr->tc(o, m, c);
    o->exitscope();
    return return_type;
}

Symbol attr_class::tc(SymTable *o, ClassTableP m, Symbol c){
    Symbol t = init->tc(o, m, c);
    if(t != No_type && !(m->leq(t, type_decl)))
        m->semant_error(m->lookup(c)) << "attr type not match" << std::endl;
    return type_decl;
}

void class__class::tc(SymTable *o, ClassTableP m){
    if(parent == SELF_TYPE)
        m->semant_error(this) << "Class " << name << " cannot inherit class SELF_TYPE." << std::endl;
    if(parent == Bool)
        m->semant_error(this) << "Class " << name << " cannot inherit class Bool." << std::endl;
    if(parent == Int)
        m->semant_error(this) << "Class " << name << " cannot inherit class Int." << std::endl;
    if(parent == Str)
        m->semant_error(this) << "Class " << name << " cannot inherit class Str." << std::endl;
    std::set<Symbol> saw;
    o->enterscope();
    // add all attrs in symbol table
    for(int i=features->first(); features->more(i); i=features->next(i)){
        auto f = features->nth(i);
        if(saw.find(f->get_name()) != saw.end())
            m->semant_error(this) << "redefined " << f->get_name() << std::endl;
        if(f->get_name() == self){
            m->semant_error(this) << "self cannot be name of attr or method" << std::endl;
        }
        if(f->is_attr())
            o->addid(f->get_name(), f->get_type());
    }
    // infer type for all features
    for(int i=features->first(); features->more(i); i=features->next(i)){
        features->nth(i)->tc(o, m, this->name);
    }
    o->exitscope();
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);
    SymTable *symtable = new SymTable();

    /* some semantic analysis code may go here */
    for(int i=classes->first(); classes->more(i); i=classes->next(i)){
        classes->nth(i)->tc(symtable, classtable);
    }

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." <<std::endl;
	exit(1);
    }
}
