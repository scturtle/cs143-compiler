#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <vector>
#include <utility>
#include <map>

enum Basicness { Basic, NotBasic };
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol, CgenNode> {
  private:
    List<CgenNode> *nds;
    ostream &str;
    int stringclasstag;
    int intclasstag;
    int boolclasstag;

    // The following methods emit code for
    // constants and global declarations.

    void code_global_data();
    void code_global_text();
    void code_bools(int);
    void code_select_gc();
    void code_constants();

    // The following creates an inheritance graph from
    // a list of classes.  The graph is implemented as
    // a tree of `CgenNode', and class names are placed
    // in the base class symbol table.

    void install_basic_classes();
    void install_class(CgenNodeP nd);
    void install_classes(Classes cs);
    void build_inheritance_tree();
    void set_relations(CgenNodeP nd);

    void gen_name_tab(CgenNodeP nd);
    void gen_obj_tab(CgenNodeP nd);
    void lookup_features(CgenNodeP nd);
    void gen_disp_tab(List<CgenNode> *l);
    void gen_prot_obj(List<CgenNode> *l);
    void gen_init(List<CgenNode> *l);
    void gen_meth(List<CgenNode> *l);

  public:
    CgenClassTable(Classes, ostream &str);
    void code();
    CgenNodeP root();
};

class CgenNode : public class__class {
  private:
    CgenNodeP parentnd;       // Parent of class
    List<CgenNode> *children; // Children of class
    Basicness basic_status;   // `Basic' if class is basic
                              // `NotBasic' otherwise

  public:
    int tag;
    std::vector<std::pair<Symbol, Symbol> > meths;
    std::vector<Feature_class *> attrs;
    int get_meth_offset(Symbol meth_name);
    int get_attr_offset(Symbol attr_name);

    CgenNode(Class_ c, Basicness bstatus, CgenClassTableP class_table);

    void add_child(CgenNodeP child);
    List<CgenNode> *get_children() { return children; }
    void set_parentnd(CgenNodeP p);
    CgenNodeP get_parentnd() { return parentnd; }
    int basic() { return (basic_status == Basic); }
};

class BoolConst {
  private:
    int val;

  public:
    BoolConst(int);
    void code_def(ostream &, int boolclasstag);
    void code_ref(ostream &) const;
};
