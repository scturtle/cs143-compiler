//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <algorithm>

int tag_cnt = 0;
int label_cnt = 0;
int tmp_cnt = 0;
int max(int a, int b) { return a >= b ? a : b; }
int new_label() { return label_cnt++; }
int new_tmp() { return tmp_cnt++; }
void release_tmp() { --tmp_cnt; }
CgenNodeP cur_nd;
CgenClassTableP class_table;
SymbolTable<Symbol, int> tmp_table;

// lookup name in locals and current class's attrs
std::pair<char *, int> lookup(Symbol name) {
    int *loc = tmp_table.lookup(name);
    if (loc == nullptr)
        return std::make_pair((char *)SELF, cur_nd->get_attr_offset(name));
    else
        return std::make_pair((char *)FP, *loc);
}

extern void emit_string_constant(ostream &str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string, IO,
    length, Main, main_meth, No_class, No_type, Object, out_int, out_string,
    prim_slot, self, SELF_TYPE, Str, str_field, substr, type_name, val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void) {
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

static char *gc_init_names[] = { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] = { "_NoGC_Collect", "_GenGC_Collect",
                                    "_ScnGC_Collect" };

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) {
    // spim wants comments to start with '#'
    os << "# start of generated code\n";

    initialize_constants();
    CgenClassTable *codegen_classtable = new CgenClassTable(classes, os);

    os << "\n# end of generated code\n";
}

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg,
                      ostream &s) {
    s << LW << dest_reg << " " << offset *WORD_SIZE << "(" << source_reg << ")"
      << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg,
                       ostream &s) {
    s << SW << source_reg << " " << offset *WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream &s) {
    s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(char *dest_reg, char *address, ostream &s) {
    s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char *dest_reg, ostream &s) {
    s << LA << dest_reg << " ";
}

static void emit_load_bool(char *dest, const BoolConst &b, ostream &s) {
    emit_partial_load_address(dest, s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream &s) {
    emit_partial_load_address(dest, s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream &s) {
    emit_partial_load_address(dest, s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream &s) {
    s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char *dest, char *src1, ostream &s) {
    s << NEG << dest << " " << src1 << endl;
}

static void emit_add(char *dest, char *src1, char *src2, ostream &s) {
    s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream &s) {
    s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char *dest, char *src1, int imm, ostream &s) {
    s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char *dest, char *src1, char *src2, ostream &s) {
    s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream &s) {
    s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream &s) {
    s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char *dest, char *src1, int num, ostream &s) {
    s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char *dest, ostream &s) {
    s << JALR << "\t" << dest << endl;
}

static void emit_jal(char *address, ostream &s) { s << JAL << address << endl; }

static void emit_return(ostream &s) { s << RET << endl; }

static void emit_gc_assign(ostream &s) { s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream &s) {
    s << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream &s) {
    s << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream &s) { s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream &s) {
    s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream &s) {
    s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream &s) {
    emit_label_ref(l, s);
    s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s) {
    s << BEQZ << source << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s) {
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s) {
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s) {
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s) {
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s) {
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s) {
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_branch(int l, ostream &s) {
    s << BRANCH;
    emit_label_ref(l, s);
    s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream &str) {
    emit_store(reg, 0, SP, str);
    emit_addiu(SP, SP, -4, str);
}

static void emit_pop(char *reg, ostream &str) {
    emit_addiu(SP, SP, 4, str);
    emit_load(reg, 0, SP, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream &s) {
    emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

static void emit_fetch_bool(char *dest, char *source, ostream &s) {
    emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream &s) {
    emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_test_collector(ostream &s) {
    emit_push(ACC, s);
    emit_move(ACC, SP, s);  // stack end
    emit_move(A1, ZERO, s); // allocate nothing
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char *source, ostream &s) {
    if (source != (char *)A1)
        emit_move(A1, source, s);
    s << JAL << "_gc_check" << endl;
}

static void emit_func_before(int tmps, ostream &s) {
    emit_addiu(SP, SP, -(3 + tmps) * WORD_SIZE, s);
    emit_store(FP, 3 + tmps, SP, s);
    emit_store(SELF, 2 + tmps, SP, s);
    emit_store(RA, 1 + tmps, SP, s);
    emit_addiu(FP, SP, WORD_SIZE, s); // fp point to return addr
    emit_move(SELF, ACC, s);
}

static void emit_func_after(int tmps, int fms, ostream &s) {
    emit_load(FP, 3 + tmps, SP, s);
    emit_load(SELF, 2 + tmps, SP, s);
    emit_load(RA, 1 + tmps, SP, s);
    emit_addiu(SP, SP, (3 + tmps + fms) * WORD_SIZE, s);
    emit_return(s);
}

static void emit_method_call(Symbol cls, Symbol meth, ostream &s) {
    s << JAL;
    emit_method_ref(cls, meth, s);
    s << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream &s) { s << STRCONST_PREFIX << index; }

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream &s, int stringclasstag) {
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                          // label
      << WORD << stringclasstag << endl // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4)
      << endl // size
      << WORD;

    /***** Add dispatch information for class String ******/
    emit_disptable_ref(Str, s);

    s << endl; // dispatch table
    s << WORD;
    lensym->code_ref(s);
    s << endl;                    // string length
    emit_string_constant(s, str); // ascii string
    s << ALIGN;                   // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream &s, int stringclasstag) {
    for (List<StringEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s) { s << INTCONST_PREFIX << index; }

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag) {
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                           // label
      << WORD << intclasstag << endl                     // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
      << WORD;

    /***** Add dispatch information for class Int ******/
    emit_disptable_ref(Int, s);

    s << endl;                // dispatch table
    s << WORD << str << endl; // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag) {
    for (List<IntEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s, intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream &s) const { s << BOOLCONST_PREFIX << val; }

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream &s, int boolclasstag) {
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                            // label
      << WORD << boolclasstag << endl                     // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
      << WORD;

    /***** Add dispatch information for class Bool ******/
    emit_disptable_ref(Bool, s);

    s << endl;                // dispatch table
    s << WORD << val << endl; // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data() {
    Symbol main = idtable.lookup_string(MAINNAME);
    Symbol string = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n" << ALIGN;
    //
    // The following global names must be defined first.
    //
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL;
    emit_protobj_ref(main, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(integer, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(string, str);
    str << endl;
    str << GLOBAL;
    falsebool.code_ref(str);
    str << endl;
    str << GLOBAL;
    truebool.code_ref(str);
    str << endl;
    str << GLOBAL << INTTAG << endl;
    str << GLOBAL << BOOLTAG << endl;
    str << GLOBAL << STRINGTAG << endl;

    //
    // We also need to know the tag of the Int, String, and Bool classes
    // during code generation.
    //
    str << INTTAG << LABEL << WORD << intclasstag << endl;
    str << BOOLTAG << LABEL << WORD << boolclasstag << endl;
    str << STRINGTAG << LABEL << WORD << stringclasstag << endl;
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text() {
    str << GLOBAL << HEAP_START << endl << HEAP_START << LABEL << WORD << 0
        << endl << "\t.text" << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Int"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("String"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"), str);
    str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"),
                    str);
    str << endl;
}

void CgenClassTable::code_bools(int boolclasstag) {
    falsebool.code_def(str, boolclasstag);
    truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc() {
    //
    // Generate GC choice constants (pointers to GC functions)
    //
    str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
    str << "_MemMgr_INITIALIZER:" << endl;
    str << WORD << gc_init_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
    str << "_MemMgr_COLLECTOR:" << endl;
    str << WORD << gc_collect_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_TEST" << endl;
    str << "_MemMgr_TEST:" << endl;
    str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants() {
    //
    // Add constants that are required by the code generator.
    //
    stringtable.add_string("");
    inttable.add_string("0");

    stringtable.code_string_table(str, stringclasstag);
    inttable.code_string_table(str, intclasstag);
    code_bools(boolclasstag);
}

void set_tags(CgenNodeP nd) {
    nd->tag = tag_cnt++;
    for (auto c = nd->get_children(); c; c = c->tl()) {
        set_tags(c->hd());
    }
}

CgenClassTable::CgenClassTable(Classes classes, ostream &s)
    : nds(NULL), str(s) {

    enterscope();
    if (cgen_debug)
        cout << "Building CgenClassTable" << endl;
    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();
    set_tags(root());

    stringclasstag = probe(Str)->tag;
    intclasstag = probe(Int)->tag;
    boolclasstag = probe(Bool)->tag;

    code();
    exitscope();
}

void CgenClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    //
    // A few special class names are installed in the lookup table but not
    // the class list.  Thus, these classes exist, but are not part of the
    // inheritance hierarchy.
    // No_class serves as the parent of Object and the other special classes.
    // SELF_TYPE is the self class; it cannot be redefined or inherited.
    // prim_slot is a class known to the code generator.
    //
    addid(No_class,
          new CgenNode(class_(No_class, No_class, nil_Features(), filename),
                       Basic, this));
    addid(SELF_TYPE,
          new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
                       Basic, this));
    addid(prim_slot,
          new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
                       Basic, this));

    //
    // The Object class has no parent class. Its methods are
    //        cool_abort() : Object    aborts the program
    //        type_name() : Str        returns a string representation of class
    // name
    //        copy() : SELF_TYPE       returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.
    //
    install_class(new CgenNode(
        class_(Object, No_class,
               append_Features(
                   append_Features(
                       single_Features(method(cool_abort, nil_Formals(), Object,
                                              no_expr())),
                       single_Features(
                           method(type_name, nil_Formals(), Str, no_expr()))),
                   single_Features(
                       method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
               filename),
        Basic, this));

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE          writes a string to the output
    //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
    //        in_string() : Str                    reads a string from the input
    //        in_int() : Int                         "   an int     "  "     "
    //
    install_class(new CgenNode(
        class_(
            IO, Object,
            append_Features(
                append_Features(
                    append_Features(
                        single_Features(method(out_string,
                                               single_Formals(formal(arg, Str)),
                                               SELF_TYPE, no_expr())),
                        single_Features(method(out_int,
                                               single_Formals(formal(arg, Int)),
                                               SELF_TYPE, no_expr()))),
                    single_Features(
                        method(in_string, nil_Formals(), Str, no_expr()))),
                single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
            filename),
        Basic, this));

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    install_class(new CgenNode(
        class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())),
               filename),
        Basic, this));

    //
    // Bool also has only the "val" slot.
    //
    install_class(new CgenNode(
        class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),
               filename),
        Basic, this));

    //
    // The class Str has a number of slots and operations:
    //       val                                  ???
    //       str_field                            the string itself
    //       length() : Int                       length of the string
    //       concat(arg: Str) : Str               string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring
    //
    install_class(new CgenNode(
        class_(
            Str, Object,
            append_Features(
                append_Features(
                    append_Features(
                        append_Features(
                            single_Features(attr(val, Int, no_expr())),
                            single_Features(
                                attr(str_field, prim_slot, no_expr()))),
                        single_Features(
                            method(length, nil_Formals(), Int, no_expr()))),
                    single_Features(method(concat,
                                           single_Formals(formal(arg, Str)),
                                           Str, no_expr()))),
                single_Features(method(
                    substr, append_Formals(single_Formals(formal(arg, Int)),
                                           single_Formals(formal(arg2, Int))),
                    Str, no_expr()))),
            filename),
        Basic, this));
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd) {
    Symbol name = nd->get_name();

    if (probe(name)) {
        return;
    }

    // the class name is legal, so add it to the list of classes
    // and the symbol table.
    nds = new List<CgenNode>(nd, nds);
    addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs) {
    for (int i = cs->first(); cs->more(i); i = cs->next(i))
        install_class(new CgenNode(cs->nth(i), NotBasic, this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree() {
    for (List<CgenNode> *l = nds; l; l = l->tl())
        set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd) {
    CgenNode *parent_node = probe(nd->get_parent());
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n) {
    children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p) {
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}

int CgenNode::get_meth_offset(Symbol meth_name) {
    // in disptable
    int offset = 0;
    for (auto pr : meths)
        if (pr.first == meth_name)
            return offset;
        else
            ++offset;
    return -1; // not found
}

int CgenNode::get_attr_offset(Symbol attr_name) {
    // in stack
    int offset = 3;
    for (auto attr : attrs)
        if (attr->get_name() == attr_name)
            return offset;
        else
            ++offset;
    return -1; // not found
}

void CgenClassTable::gen_name_tab(CgenNodeP nd) {
    str << WORD;
    stringtable.lookup_string(nd->get_name()->get_string())->code_ref(str);
    str << endl;
    for (auto c = nd->get_children(); c; c = c->tl())
        gen_name_tab(c->hd());
}

void CgenClassTable::gen_obj_tab(CgenNodeP nd) {
    auto name = nd->get_name();
    str << WORD;
    emit_protobj_ref(name, str);
    str << endl;
    str << WORD;
    emit_init_ref(name, str);
    str << endl;
    for (auto c = nd->get_children(); c; c = c->tl())
        gen_obj_tab(c->hd());
}

void CgenClassTable::lookup_features(CgenNodeP nd) {
    if (nd->get_name() != Object) {
        auto pnd = nd->get_parentnd();
        for (auto f : pnd->meths)
            nd->meths.push_back(f); // ugly copy
        for (auto f : pnd->attrs)
            nd->attrs.push_back(f);
    }
    auto fs = nd->get_features();
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
        auto f = fs->nth(i);
        if (f->is_method()) {
            // override?
            bool found = false;
            for (auto &pr : nd->meths)
                if (pr.first == f->get_name()) {
                    pr.second = nd->get_name();
                    found = true;
                }
            if (found == false)
                nd->meths.push_back(
                    std::make_pair(f->get_name(), nd->get_name()));
        } else
            nd->attrs.push_back(f);
    }
    for (auto c = nd->get_children(); c; c = c->tl())
        lookup_features(c->hd());
}

void CgenClassTable::gen_disp_tab(List<CgenNode> *l) {
    if (l == nullptr)
        return;
    gen_disp_tab(l->tl());
    auto nd = l->hd();
    emit_disptable_ref(nd->get_name(), str);
    str << LABEL;
    for (auto pr : nd->meths) {
        str << WORD;
        emit_method_ref(pr.second, pr.first, str);
        str << endl;
    }
}

void CgenClassTable::gen_prot_obj(List<CgenNode> *l) {
    if (l == nullptr)
        return;
    gen_prot_obj(l->tl());
    auto nd = l->hd();
    str << WORD << "-1" << endl; // eye
    emit_protobj_ref(nd->get_name(), str);
    str << LABEL << WORD << nd->tag << endl << WORD << 3 + nd->attrs.size()
        << endl << WORD;
    emit_disptable_ref(nd->get_name(), str);
    str << endl;
    for (auto attr : nd->attrs) {
        str << WORD;
        if (attr->get_type() == Int)
            inttable.lookup_string("0")->code_ref(str);
        else if (attr->get_type() == Str)
            stringtable.lookup_string("")->code_ref(str);
        else if (attr->get_type() == Bool)
            falsebool.code_ref(str);
        else
            str << 0;
        str << endl;
    }
}

void CgenClassTable::gen_init(List<CgenNode> *l) {
    if (l == nullptr)
        return;
    gen_init(l->tl());
    auto nd = l->hd();

    // max tmps for all attrs
    int tmps = 0;
    for (auto attr : nd->attrs)
        tmps = max(tmps, attr->get_expr()->cnt_max_tmps());
    emit_init_ref(nd->get_name(), str);
    str << LABEL;
    emit_func_before(tmps, str);
    // super.init()
    if (nd->get_name() != Object) {
        str << JAL;
        emit_init_ref(nd->get_parentnd()->get_name(), str);
        str << endl;
    }
    // each attr's init
    int offset = 2;
    for (auto attr : nd->attrs) {
        ++offset;
        if (attr->get_expr()->is_no_expr())
            continue;
        auto pnt_attrs = nd->get_parentnd()->attrs;
        if (find(pnt_attrs.begin(), pnt_attrs.end(), attr) != pnt_attrs.end())
            continue;

        cur_nd = nd;
        tmp_table.enterscope();
        tmp_cnt = 0;

        attr->get_expr()->code(str);

        tmp_table.exitscope();

        emit_store(ACC, offset, SELF, str);
    }
    emit_move(ACC, SELF, str);
    emit_func_after(tmps, 0, str);
}

void CgenClassTable::gen_meth(List<CgenNode> *l) {
    if (l == nullptr)
        return;
    gen_meth(l->tl());
    auto nd = l->hd();
    // don't gen method for basic class, they are in library
    if (nd->basic())
        return;

    auto fs = nd->get_features();
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {

        if (fs->nth(i)->is_method() == false)
            continue;
        auto f = dynamic_cast<method_class *>(fs->nth(i));

        int tmps = f->get_expr()->cnt_max_tmps();
        emit_method_ref(nd->get_name(), f->get_name(), str);
        str << LABEL;
        emit_func_before(tmps, str);

        cur_nd = nd;
        tmp_table.enterscope();
        tmp_cnt = 0;

        // build formal offset map
        auto fms = f->get_formals();
        // stack: H[formal1, formal2, tmps, fp, so, ra(new_fp)]L
        int offset = fms->len() + tmps + 2;
        for (int i = fms->first(); fms->more(i); i = fms->next(i))
            tmp_table.addid(fms->nth(i)->get_name(), new int { offset-- });

        f->get_expr()->code(str);

        tmp_table.exitscope();

        emit_func_after(tmps, fms->len(), str);
    }
}

void CgenClassTable::code() {
    if (cgen_debug)
        cout << "coding global data" << endl;
    code_global_data();

    if (cgen_debug)
        cout << "choosing gc" << endl;
    code_select_gc();

    if (cgen_debug)
        cout << "coding constants" << endl;
    code_constants();

    //                 Add your code to emit
    //                   - prototype objects
    //                   - class_nameTab
    //                   - dispatch tables
    //

    str << CLASSNAMETAB << LABEL;
    // gen_name_tab(nds);
    gen_name_tab(root());

    str << CLASSOBJTAB << LABEL;
    gen_obj_tab(root());

    // build meths and attrs recursively for each nd
    lookup_features(root());

    gen_disp_tab(nds);
    gen_prot_obj(nds);

    if (cgen_debug)
        cout << "coding global text" << endl;
    code_global_text();

    //                 Add your code to emit
    //                   - object initializer
    //                   - the class methods
    //                   - etc...

    // global class table for lookup tag or something
    class_table = this;
    gen_init(nds);
    gen_meth(nds);
}

CgenNodeP CgenClassTable::root() { return probe(Object); }

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct)
    : class__class((const class__class &)*nd), parentnd(NULL), children(NULL),
      basic_status(bstatus) {
    stringtable.add_string(
        name->get_string()); // Add class name to string table
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s) {
    expr->code(s);
    auto pr = lookup(name);
    emit_store(ACC, pr.second, pr.first, s);
}

int assign_class::cnt_max_tmps() { return expr->cnt_max_tmps(); }

void static_dispatch_class::code(ostream &s) {
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        actual->nth(i)->code(s);
        emit_push(ACC, s);
    }
    expr->code(s);
    // void?
    int ok = new_label();
    emit_bne(ACC, ZERO, ok, s);
    // call abort
    // filename in a0
    emit_load_string(
        ACC, stringtable.lookup_string(cur_nd->get_filename()->get_string()),
        s);
    // line number in t1
    emit_load_imm(T1, cur_nd->get_line_number(), s);
    emit_jal("_dispatch_abort", s);
    emit_label_def(ok, s);
    // load disptable
    // emit_load(T1, DISPTABLE_OFFSET, ACC, s);
    s << LA << T1 << " ";
    emit_disptable_ref(type_name, s);
    s << endl;
    // lookup method offset in disptable
    int offset =
        class_table->probe(this->type_name)->get_meth_offset(this->name);
    emit_load(T1, offset, T1, s);
    // call it
    emit_jalr(T1, s);
}

int static_dispatch_class::cnt_max_tmps() {
    int cnt = expr->cnt_max_tmps();
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
        cnt = max(cnt, actual->nth(i)->cnt_max_tmps());
    return cnt;
}

void dispatch_class::code(ostream &s) {
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        actual->nth(i)->code(s);
        emit_push(ACC, s);
    }
    expr->code(s);
    // void?
    int ok = new_label();
    emit_bne(ACC, ZERO, ok, s);
    // call abort
    // filename in a0
    emit_load_string(
        ACC, stringtable.lookup_string(cur_nd->get_filename()->get_string()),
        s);
    // line number in t1
    emit_load_imm(T1, cur_nd->get_line_number(), s);
    emit_jal("_dispatch_abort", s);
    emit_label_def(ok, s);
    // load disptable
    emit_load(T1, DISPTABLE_OFFSET, ACC, s);
    // lookup method offset in disptable
    auto tp = expr->get_type();
    tp = tp == SELF_TYPE ? cur_nd->get_name() : tp;
    int offset = class_table->probe(tp)->get_meth_offset(this->name);
    emit_load(T1, offset, T1, s);
    // call it
    emit_jalr(T1, s);
}

int dispatch_class::cnt_max_tmps() {
    int cnt = expr->cnt_max_tmps();
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
        cnt = max(cnt, actual->nth(i)->cnt_max_tmps());
    return cnt;
}

void cond_class::code(ostream &s) {
    int else_lbl = new_label();
    int end = new_label();
    pred->code(s);
    emit_fetch_bool(T1, ACC, s);
    emit_beqz(T1, else_lbl, s);
    then_exp->code(s);
    emit_branch(end, s);
    emit_label_def(else_lbl, s);
    else_exp->code(s);
    emit_label_def(end, s);
}

int cond_class::cnt_max_tmps() {
    return max(pred->cnt_max_tmps(),
               max(then_exp->cnt_max_tmps(), else_exp->cnt_max_tmps()));
}

void loop_class::code(ostream &s) {
    int start = new_label();
    int end = new_label();
    emit_label_def(start, s);
    pred->code(s);
    emit_fetch_bool(T1, ACC, s);
    emit_beq(T1, ZERO, end, s);
    body->code(s);
    emit_branch(start, s);
    emit_label_def(end, s);
    emit_move(ACC, ZERO, s); // void
}

int loop_class::cnt_max_tmps() {
    return max(pred->cnt_max_tmps(), body->cnt_max_tmps());
}

void typcase_class::code(ostream &s) {
    expr->code(s);
    int end = new_label();
    int cur = new_label(), nxt;
    emit_bne(ACC, ZERO, cur, s);
    // filename in a0
    emit_load_string(
        ACC, stringtable.lookup_string(cur_nd->get_filename()->get_string()),
        s);
    // line number in t1
    emit_load_imm(T1, cur_nd->get_line_number(), s);
    // void
    emit_jal("_case_abort2", s);

    // sort branches by decreasing order of tag
    std::vector<int> nths;
    for (int i = cases->first(); cases->more(i); i = cases->next(i))
        nths.push_back(i);
    std::sort(nths.begin(), nths.end(), [&](int i, int j)->bool {
        auto cas1 = dynamic_cast<branch_class *>(cases->nth(i));
        auto cas2 = dynamic_cast<branch_class *>(cases->nth(j));
        int tag1 = class_table->probe(cas1->get_type())->tag;
        int tag2 = class_table->probe(cas2->get_type())->tag;
        return tag1 > tag2;
    });

    // gen each branch
    for (int i : nths) {
        tmp_table.enterscope();
        auto cas = dynamic_cast<branch_class *>(cases->nth(i));
        nxt = new_label();
        emit_label_def(cur, s);
        // load tag
        emit_load(T2, 0, ACC, s);
        // min and max tag
        auto nd = class_table->probe(cas->get_type());
        int min_tag = nd->tag;
        // tag of the left most one
        // while(nd->get_children()) nd = nd->get_children()->hd();
        // tag of the right most one
        while (nd->get_children())
            for (auto c = nd->get_children(); c; c = c->tl())
                nd = c->hd();
        int max_tag = nd->tag;
        emit_blti(T2, min_tag, nxt, s);
        emit_bgti(T2, max_tag, nxt, s);
        // tag match, add into tmp table
        tmp_table.addid(cas->get_name(), new int { new_tmp() });
        auto pr = lookup(cas->get_name());
        emit_store(ACC, pr.second, pr.first, s);
        cas->get_expr()->code(s);
        // jump to the end
        emit_branch(end, s);
        cur = nxt;
        tmp_table.exitscope();
        release_tmp();
    }
    // final case for no match
    emit_label_def(nxt, s);
    emit_jal("_case_abort", s);
    emit_label_def(end, s);
}

int typcase_class::cnt_max_tmps() {
    int cnt = expr->cnt_max_tmps();
    for (int i = cases->first(); cases->more(i); i = cases->next(i))
        cnt = max(cnt, cases->nth(i)->get_expr()->cnt_max_tmps() + 1);
    return cnt;
}

void block_class::code(ostream &s) {
    for (int i = body->first(); body->more(i); i = body->next(i))
        body->nth(i)->code(s);
}

int block_class::cnt_max_tmps() {
    int cnt = 0;
    for (int i = body->first(); body->more(i); i = body->next(i))
        cnt = max(cnt, body->nth(i)->cnt_max_tmps());
    return cnt;
}

void let_class::code(ostream &s) {
    if (init->is_no_expr()) {
        if (type_decl == Int)
            emit_load_int(ACC, inttable.lookup_string("0"), s);
        else if (type_decl == Bool)
            emit_load_bool(ACC, falsebool, s);
        else if (type_decl == Str)
            emit_load_string(ACC, stringtable.lookup_string(""), s);
        else
            emit_move(ACC, ZERO, s);
    } else
        init->code(s);
    tmp_table.enterscope();
    tmp_table.addid(identifier, new int { new_tmp() });
    auto pr = lookup(identifier);
    emit_store(ACC, pr.second, pr.first, s);
    body->code(s);
    tmp_table.exitscope();
    release_tmp();
}

int let_class::cnt_max_tmps() {
    return max(init->cnt_max_tmps(), 1 + body->cnt_max_tmps());
}

void plus_class::code(ostream &s) {
    e1->code(s);
    emit_push(ACC, s);
    e2->code(s);
    emit_method_call(Object, ::copy, s);
    emit_pop(T1, s);
    emit_fetch_int(T1, T1, s);
    emit_fetch_int(T2, ACC, s);
    emit_add(T1, T1, T2, s);
    emit_store_int(T1, ACC, s);
}

int plus_class::cnt_max_tmps() {
    return max(e1->cnt_max_tmps(), 1 + e2->cnt_max_tmps());
}

void sub_class::code(ostream &s) {
    e1->code(s);
    emit_push(ACC, s);
    e2->code(s);
    emit_method_call(Object, ::copy, s);
    emit_pop(T1, s);
    emit_fetch_int(T1, T1, s);
    emit_fetch_int(T2, ACC, s);
    emit_sub(T1, T1, T2, s);
    emit_store_int(T1, ACC, s);
}

int sub_class::cnt_max_tmps() {
    return max(e1->cnt_max_tmps(), 1 + e2->cnt_max_tmps());
}

void mul_class::code(ostream &s) {
    e1->code(s);
    emit_push(ACC, s);
    e2->code(s);
    emit_method_call(Object, ::copy, s);
    emit_pop(T1, s);
    emit_fetch_int(T1, T1, s);
    emit_fetch_int(T2, ACC, s);
    emit_mul(T1, T1, T2, s);
    emit_store_int(T1, ACC, s);
}

int mul_class::cnt_max_tmps() {
    return max(e1->cnt_max_tmps(), 1 + e2->cnt_max_tmps());
}

void divide_class::code(ostream &s) {
    e1->code(s);
    emit_push(ACC, s);
    e2->code(s);
    emit_method_call(Object, ::copy, s);
    emit_pop(T1, s);
    emit_fetch_int(T1, T1, s);
    emit_fetch_int(T2, ACC, s);
    emit_div(T1, T1, T2, s);
    emit_store_int(T1, ACC, s);
}

int divide_class::cnt_max_tmps() {
    return max(e1->cnt_max_tmps(), 1 + e2->cnt_max_tmps());
}

void neg_class::code(ostream &s) {
    e1->code(s);
    emit_method_call(Object, ::copy, s);
    emit_fetch_int(T1, ACC, s);
    emit_neg(T1, T1, s);
    emit_store_int(T1, ACC, s);
}

int neg_class::cnt_max_tmps() { return e1->cnt_max_tmps(); }

void lt_class::code(ostream &s) {
    e1->code(s);
    emit_push(ACC, s);
    e2->code(s);
    emit_pop(T1, s);
    emit_fetch_int(T1, T1, s);
    emit_fetch_int(T2, ACC, s);
    int lbl = new_label();
    emit_load_bool(ACC, truebool, s);
    emit_blt(T1, T2, lbl, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(lbl, s);
}

int lt_class::cnt_max_tmps() {
    return max(e1->cnt_max_tmps(), 1 + e2->cnt_max_tmps());
}

void eq_class::code(ostream &s) {
    e1->code(s);
    emit_push(ACC, s);
    e2->code(s);
    emit_pop(T1, s);
    emit_move(T2, ACC, s);
    int ok = new_label();
    emit_load_bool(A1, falsebool, s);
    emit_load_bool(ACC, truebool, s);
    emit_beq(T1, T2, ok, s);
    emit_jal("equality_test", s);
    emit_label_def(ok, s);
}

int eq_class::cnt_max_tmps() {
    return max(e1->cnt_max_tmps(), 1 + e2->cnt_max_tmps());
}

void leq_class::code(ostream &s) {
    e1->code(s);
    emit_push(ACC, s);
    e2->code(s);
    emit_pop(T1, s);
    emit_fetch_int(T1, T1, s);
    emit_fetch_int(T2, ACC, s);
    int lbl = new_label();
    emit_load_bool(ACC, truebool, s);
    emit_bleq(T1, T2, lbl, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(lbl, s);
}

int leq_class::cnt_max_tmps() {
    return max(e1->cnt_max_tmps(), 1 + e2->cnt_max_tmps());
}

void comp_class::code(ostream &s) {
    e1->code(s);
    emit_fetch_bool(T1, ACC, s);
    emit_load_bool(ACC, truebool, s);
    int lbl = new_label();
    emit_beqz(T1, lbl, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(lbl, s);
}

int comp_class::cnt_max_tmps() { return e1->cnt_max_tmps(); }

void int_const_class::code(ostream &s) {
    //
    // Need to be sure we have an IntEntry *, not an arbitrary Symbol
    //
    emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

int int_const_class::cnt_max_tmps() { return 0; }

void string_const_class::code(ostream &s) {
    emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

int string_const_class::cnt_max_tmps() { return 0; }

void bool_const_class::code(ostream &s) {
    emit_load_bool(ACC, BoolConst(val), s);
}

int bool_const_class::cnt_max_tmps() { return 0; }

void new__class::code(ostream &s) {
    if (type_name == SELF_TYPE) {
        emit_load_address(T1, CLASSOBJTAB, s);
        emit_load(T2, 0, SELF, s); // put tag into t2
        emit_sll(T2, T2, 3, s);    // tag = tag * 8
        emit_addu(T1, T1, T2, s);  // the addr of probtobj
        emit_move("$s1", T1, s);
        emit_load(ACC, 0, T1, s);
        emit_method_call(Object, ::copy, s);
        emit_load(T1, 1, "$s1", s); // addr of init
        emit_jalr(T1, s);
    } else if (type_name == Bool)
        emit_load_bool(ACC, falsebool, s);
    else {
        // la $a0 X_protObj
        s << LA << ACC << " ";
        emit_protobj_ref(type_name, s);
        s << endl;
        // jal Object.copy
        emit_method_call(Object, ::copy, s);
        // jal X_init
        s << JAL;
        emit_init_ref(type_name, s);
        s << endl;
    }
}

int new__class::cnt_max_tmps() { return 1; }

void isvoid_class::code(ostream &s) {
    e1->code(s);
    emit_move(T1, ACC, s);
    emit_load_bool(ACC, truebool, s);
    int lbl = new_label();
    emit_beqz(T1, lbl, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(lbl, s);
}

int isvoid_class::cnt_max_tmps() { return e1->cnt_max_tmps(); }

void no_expr_class::code(ostream &s) {
    // nothing to do
}

int no_expr_class::cnt_max_tmps() { return 0; }

void object_class::code(ostream &s) {
    // s << "# OBJECT " << name << endl;
    if (name == self)
        emit_move(ACC, "$s0", s);
    else {
        auto pr = lookup(name);
        // s << "#        " << pr.first << " " << pr.second << endl;
        emit_load(ACC, pr.second, pr.first, s);
    }
}

int object_class::cnt_max_tmps() { return 0; }
