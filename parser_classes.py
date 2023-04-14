import sys

import parser
from decaf import sym_table
from symbol_table import SymbolEntry
from code_gen import emit

semantic_err = 0

"""
All node types inherited from base node. Some nodes have special params i.e. node_printstmt
"""

with open(sys.argv[1]) as file:
    raw_lines = file.readlines()


class ASTNode:
    node_type = 0
    row = 0
    col_start = -1
    col_end = -1
    name = ''
    err = False
    node_children = []  # list of ASTNode subclasses (stack???)
    val = None

    def __init__(self):
        self.node_children = []
        self.err = False

    def code_visit(self):
        p=1

    def type_check(self):
        p=1

    def construct_symbols(self, parent_table=None):
        p=1

    def check(self):
        """
        Shadow for inheritors to use to check semantics (here for recursion)
        :return:
        """
        p=1


class Node_Terminal(ASTNode):
    """
    name: type of terminal i.e. identifier, op, constant, etc.
    val: value of terminal
    """
    node_type = 30
    name = ""
    val = None

    def check(self):
        return self.type_check()

    def type_check(self):
        if self.name == 'IntConstant':
            return 'int'
        elif self.name == 'DoubleConstant':
            return 'double'
        elif self.name == 'BoolConstant':
            return 'bool'
        elif self.name == 'StringConstant':
            return 'string'
        elif self.name == 'Null':
            return 'null'
        elif self.name != 'Operator':  # identifier
            return sym_table.lookup_type(self.name)
        else:  # should never hit
            return 'ERR_TYPE'


class Node_Program(ASTNode):
    """
    Program ::= Decl+

    node_children = List of vardecls / funcdecls
    """
    node_type = 1
    name = 'Program'

    var_table = {}  # TODO: COMBINE SINCE CANT BE SAME NAME?
    #func_table = {}

    def __init__(self):
        super().__init__()
        self.var_table = {}  # string: SymbolEntry
        #self.func_table = {}  # string: SymbolEntry

    def construct_symbols(self, parent_table=None):
        for node in self.node_children:
            #if node.name == 'VarDecl':
            node.construct_symbols(self.var_table, scope=1, block=1)
            #else:
            #    node.construct_symbols(self.var_table)

    def check(self):
        sym_table.active_nodes.append(self.name)
        self.type_check()
        sym_table.active_nodes.pop()

    def type_check(self):
        # sym_table.stack.append(['program', self.var_table])
        sym_table.stack.append(self.var_table)

        for node in self.node_children:
            node.check()

        sym_table.stack.pop()



class Node_Variable(ASTNode):
    """
    Variable ::= type ident

    node_children = type, ident
    """
    node_type = 4
    name = 'VarDecl'  # still variable, but confusing output.

    def construct_symbols(self, parent_table=None, scope=2, block=1):  # should never be none
        type1 = self.node_children[0].val
        ident = self.node_children[1].val  # type = Type, val = int, double, bool, string, void

        if ident in parent_table.keys():
            self.err = True
            print('\n*** Error line {}.\n  {}\n{}{}\n*** Duplicate key in scope: {}\n'.format(
                self.node_children[1].row, raw_lines[self.node_children[1].row - 1].strip('\n').strip(' '),
                ' ' * (self.node_children[1].col_start + 1),
                '^' * (self.node_children[1].col_end - self.node_children[1].col_start), ident))
        else:  # maintaining old entry: throw new out. CHANGE BY REMOVING ELSE AND HAVING FOLLOW IF TO OVERWRITE IN SAME SCOPE
            parent_table[ident] = SymbolEntry(in_ident=ident, in_type=type1, in_scope=scope, in_block=block, in_ref_type='var')

    def check(self):
        sym_table.active_nodes.append(self.name)
        ret_type = self.type_check()  # returning type for type checking in exprs/calls
        sym_table.active_nodes.pop()
        return ret_type

    def type_check(self):
        type1 = sym_table.lookup_type(self.node_children[1].val)
        if type1 == 'void':  # weird edge case??
            self.err = True
            print('\n*** Error line {}.\n  {}\n{}{}\n*** Duplicate key in scope: {}\n'.format(
                self.node_children[1].row, raw_lines[self.row - 1].strip('\n').strip(' '),
                ' ' * (self.node_children[1].col_start - 1),
                '^' * (self.node_children[1].col_end - self.node_children[1].col_start), type1))
        if type1 == 'ERR_TYPE':

            # here sym fix
            self.err = True
            sym_table.inc_err_cnt()
            print('\n*** Error line {}.\n  {}\n{}{}\n*** No declaration found for variable \'{}\'\n'.format(
                self.row, raw_lines[self.row - 1].strip('\n').strip(' '),
                ' ' * (self.node_children[1].col_start - 1),
                '^' * (self.node_children[1].col_end - self.node_children[1].col_start),
                self.node_children[1].val))

        func_or_var = sym_table.var_or_func(self.node_children[1].val)
        if func_or_var == 'func':  # found in table but function instead of variable
            # here sym fix
            self.err = True
            sym_table.inc_err_cnt()
            print('\n*** Error line {}.\n  {}\n{}{}\n*** No declaration found for variable \'{}\'\n'.format(
                self.row, raw_lines[self.row - 1].strip('\n').strip(' '),
                ' ' * (self.node_children[1].col_start - 1),
                '^' * (self.node_children[1].col_end - self.node_children[1].col_start),
                self.node_children[1].val))
            type1 = 'ERR_TYPE'

        return type1


class Node_Formals(ASTNode):
    """
    Formals ::= Variable+, | ϵ

    node_children =
    """
    node_type = 7
    name = 'Formals'


class Node_FnDecl(ASTNode):
    """
    FunctionDecl ::= Type ident ( Formals ) StmtBlock |
                     void ident ( Formals ) StmtBlock
    node_children = type, ident, formals, stmtblock
    """
    node_type = 6
    name = 'FnDecl'
    formal_table = {}  # string: SymbolEntry

    def __init__(self):
        super().__init__()
        self.formal_table = {}  # string: SymbolEntry

    def construct_symbols(self, parent_table=None, scope=1, block=1):
        formal_table = {}  # self.node_children[2]  TODO: REPLACES SELF.FORMAL_TABLE??
        for node in self.node_children[2].node_children:
            node.construct_symbols(self.formal_table, scope=2, block=2)  # formals (parameters)

        #print('FORMAL TABLE:' + str(self.formal_table))

        type1 = self.node_children[0].val
        ident = self.node_children[1].val  # type = Type, val = int, double, bool, string, void
        if ident in parent_table.keys():
            print('\n*** Error line {}.\n  {}\n{}{}\n*** Duplicate key in scope: {}\n'.format(
                self.node_children[1].row, raw_lines[self.node_children[1].row - 1].strip('\n').strip(' '),
                ' ' * (self.node_children[1].col_start + 1),
                '^' * (self.node_children[1].col_end - self.node_children[1].col_start), ident))
            self.err = True
        else:  # maintaining old entry: throw new out. CHANGE BY REMOVING ELSE AND HAVING FOLLOW IF TO OVERWRITE IN SAME SCOPE
            parent_table[ident] = SymbolEntry(in_ident=ident, in_type=type1, in_scope=1, in_block=1, in_formals=self.formal_table, in_ref_type='func')
        # global table insert since functions can only be global scope

        self.node_children[3].construct_symbols(in_type='fndecl')  # stmtblock

    def check(self):
        sym_table.active_nodes.append(self.name + ' ' + self.node_children[1].val)

        if len(sym_table.stack) > 1:  # nested function  (shouldn't trigger, caught by parsing)
            print('\n*** Error line {}.\n  {}\n{}{}\n*** Nested Function\n'.format(
                self.row, raw_lines[self.row - 1].strip('\n').strip(' '),
                ' ' * (self.col_start - 1),
                '^' * (self.col_end - self.col_start)))
            self.err = True

        # TODO: ELSE TO PREVENT FURTHER INTERNAL CHECKING?
        self.type_check()  # add formal table to stack, go into stmtblocks

        sym_table.active_nodes.pop()

    def type_check(self):
        # sym_table.stack.append(['fn {}'.format(self.node_children[1]), self.formal_table])
        sym_table.stack.append(self.formal_table)

        for node in self.node_children:
            node.check()

        sym_table.stack.pop()


class Node_StmtBlock(ASTNode):
    """
    StmtBlock ::= { VariableDecl∗ Stmt∗ }

    """
    node_type = 8
    name = 'StmtBlock'

    semantic_stmt_type = ''
    var_table = {}  # string: SymbolEntry

    def __init__(self):
        super().__init__()
        self.var_table = {}

    def construct_symbols(self, parent_table=None, in_type=None):
        if in_type is not None:
            self.semantic_stmt_type = in_type
        #    print(in_type)
        for node in self.node_children:
            if node is not None:
                if node.name == 'VarDecl':
                    #print('T1' + str(node.name))
                    node.construct_symbols(self.var_table, scope=2, block=2)  # self.var_table? ask in office hours
                else:  # stmt
                    #print('T2' + str(node.name))
                    node.construct_symbols()  # call with empty argument to build nested scopes
                    # TODO: SEE ABOUT SCOPE IN OFFICE HOURS

    def check(self):
        sym_table.active_nodes.append(self.name)
        self.type_check()
        sym_table.active_nodes.pop()

    def type_check(self):
        sym_table.stack.append(self.var_table)

        for node in self.node_children:
            if node is not None:
                node.check()

        sym_table.stack.pop()


class Node_Stmt(ASTNode):
    node_type = 9
    name = 'Stmt'


class Node_IfStmt(ASTNode):
    node_type = 10
    name = 'IfStmt'

    def construct_symbols(self, parent_table=None):
        for node in self.node_children:
            if node is not None:
                if node.name == 'StmtBlock':
                    #print('T1' + str(node.name))
                    node.construct_symbols(in_type='ifstmt')

    def check(self):
        sym_table.active_nodes.append(self.name)
        self.type_check()
        sym_table.active_nodes.pop()

    def type_check(self):
        expr_type = self.node_children[0].type_check()
        if expr_type not in ['bool', 'ERR_TYPE']:

            # here sym fix
            self.err = True
            sym_table.inc_err_cnt()
            print('\n*** Error line {}.\n  {}\n{}{}\n*** Test expression must have boolean type\n'.format(
                self.row, raw_lines[self.row - 1].strip('\n').strip(' '),
                ' ' * (self.node_children[0].col_start - 2),
                '^' * (self.node_children[0].col_end - self.node_children[0].col_start)))

        if self.node_children[1] is not None:  # ; following
            self.node_children[1].check()  # stmt check
        if len(self.node_children) == 3:  # else stmt present
            self.node_children[2].check()


class Node_WhileStmt(ASTNode):
    node_type = 11
    name = 'WhileStmt'

    def construct_symbols(self, parent_table=None):
        for node in self.node_children:
            if node is not None:
                if node.name == 'StmtBlock':
                    #print('T1' + str(node.name))
                    node.construct_symbols(in_type='whilestmt')  # self.var_table? ask in office hours

    def check(self):
        sym_table.active_nodes.append(self.name)
        self.type_check()
        sym_table.active_nodes.pop()

    def type_check(self):
        expr_type = self.node_children[0].check()
        if expr_type not in ['bool', 'ERR_TYPE']:

            # here sym fix
            self.err = True
            sym_table.inc_err_cnt()
            print('\n*** Error line {}.\n  {}\n{}{}\n*** Test expression must have boolean type\n'.format(
                self.row, raw_lines[self.row - 1].strip('\n').strip(' '),
                ' ' * (self.node_children[0].col_start - 2),
                '^' * (self.node_children[0].col_end - self.node_children[0].col_start)))
        if self.node_children[1] is not None:  # ; following
            self.node_children[1].check()


class Node_ForStmt(ASTNode):
    node_type = 12
    name = 'ForStmt'

    def construct_symbols(self, parent_table=None):
        for node in self.node_children:
            if node is not None:
                if node.name == 'StmtBlock':
                    #print('T1' + str(node.name))
                    node.construct_symbols(in_type='forstmt')  # self.var_table? ask in office hours

    def check(self):
        sym_table.active_nodes.append(self.name)
        init = self.node_children[0]
        test = self.node_children[1]
        inc = self.node_children[2]
        stmt = self.node_children[3]
        if init is not None:
            init_type = init.check()
        if test is not None:
            test_type = test.type_check()
            if test_type not in ['bool', 'ERR_TYPE']:

                # here sym fix
                self.err = True
                sym_table.inc_err_cnt()
                print('\n*** Error line {}.\n  {}\n{}{}\n*** Test expression must have boolean type\n'.format(
                    self.row, raw_lines[self.row - 1].strip('\n').strip(' '),
                    ' ' * (self.node_children[1].col_start - 1),
                    '^' * (self.node_children[1].col_end - self.node_children[1].col_start)))
        if inc is not None:
            inc_type = inc.check()
        if stmt is not None:
            stmt_type = stmt.check()

        sym_table.active_nodes.pop()


class Node_ReturnStmt(ASTNode):
    node_type = 13
    name = 'ReturnStmt'

    def check(self):
        sym_table.active_nodes.append(self.name)
        if 'FnDecl' not in "".join(sym_table.active_nodes):
            print('*** return only allowed inside a function.')  # will never hit (syntax error from parsing)
            self.err += 1

        func_type = sym_table.get_func_ret()
        ret_type = self.node_children[0].type_check()



        if ret_type != func_type:
            # here sym fix
            self.err = True
            sym_table.inc_err_cnt()
            print('\n*** Error line {}.\n  {}\n{}{}\n*** Incompatible return: {} given, {} expected\n'.format(
                self.row, raw_lines[self.row - 1].strip('\n').strip(' '),
                ' ' * (self.node_children[0].col_start - 1),
                '^' * (self.node_children[0].col_end - self.node_children[0].col_start),
                ret_type, func_type))

        sym_table.active_nodes.pop()


class Node_BreakStmt(ASTNode):
    node_type = 14
    name = 'BreakStmt'

    def check(self):
        sym_table.active_nodes.append(self.name)
        if 'WhileStmt' not in sym_table.active_nodes or 'ForStmt' not in sym_table.active_nodes:

            # here sym fix
            self.err = True
            sym_table.inc_err_cnt()
            print('\n*** Error line {}.\n  {}\n{}{}\n*** break is only allowed inside a loop.\n'.format(
                self.row, raw_lines[self.row - 1].strip('\n').strip(' '),
                ' ' * (self.col_start - 3),  # TODO: CHECK THIS SPACING IS RIGHT
                '^' * 5))

        sym_table.active_nodes.pop()


class Node_PrintStmt(ASTNode):
    node_type = 15
    print_exprs = []
    name = 'PrintStmt'

    def __init__(self):
        super().__init__()
        self.node_children = []
        self.print_exprs = []

    def check(self):
        sym_table.active_nodes.append(self.name)
        self.type_check()
        sym_table.active_nodes.pop()

    def type_check(self):
        for i in range(len(self.node_children)):
            type1 = self.node_children[i].type_check()
            if type1 not in ['int', 'bool', 'string', 'ERR_TYPE']:

                # here sym fix
                self.err = True
                sym_table.inc_err_cnt()
                print('\n*** Error line {}.\n  {}\n{}{}\n*** Incompatible argument {}: {} given, int/bool/string expected\n'.format(
                    self.node_children[i].row, raw_lines[self.node_children[i].row - 1].strip('\n').strip(' '),
                    ' ' * (self.node_children[i].col_start - 2),  # TODO: FIND RIGHT AMOUNT
                    '^' * (self.node_children[i].col_end - self.node_children[i].col_start),
                    i + 1, type1))


class Node_Expr(ASTNode):
    node_type = 16
    name = 'Expr'

    def check(self):
        return self.type_check()

    def type_check(self):
        sym_table.active_nodes.append(self.name)
        if self.name == 'ReadIntegerExpr':
            sym_table.active_nodes.pop()
            return 'int'
        elif self.name == 'ReadLineExpr':
            sym_table.active_nodes.pop()
            return 'string'

        if len(self.node_children) == 2:  # unary operator
            op = parser.key_op_reversed[self.node_children[0].val]  # convert back to english from lexer rep
            rvalue_type = self.node_children[1].type_check()
            if op == '-':
                if rvalue_type not in ['int', 'double', 'ERR_TYPE']:

                    # here sym fix
                    self.err = True
                    sym_table.inc_err_cnt()
                    print('\n*** Error line {}.\n  {}\n{}{}\n*** Incompatible operands: {} {}\n'.format(
                        self.node_children[0].row, raw_lines[self.node_children[0].row - 1].strip('\n').strip(' '),
                        ' ' * (self.node_children[0].col_start - 1),
                        '^' * (self.node_children[0].col_end - self.node_children[0].col_start),
                        op, rvalue_type))
                    sym_table.active_nodes.pop()
                    return 'ERR_TYPE'
                else:
                    return rvalue_type
            elif op == '!':
                if rvalue_type not in ['bool', 'ERR_TYPE']:

                    # here sym fix
                    self.err = True
                    sym_table.inc_err_cnt()
                    print('\n*** Error line {}.\n  {}\n{}{}\n*** Incompatible operands: {} {}\n'.format(
                        self.node_children[0].row, raw_lines[self.node_children[0].row - 1].strip('\n').strip(' '),
                        ' ' * (self.node_children[0].col_start - 1),
                        '^' * (self.node_children[0].col_end - self.node_children[0].col_start),
                        op, rvalue_type))
                    sym_table.active_nodes.pop()
                    return 'ERR_TYPE'
                else:
                    sym_table.active_nodes.pop()
                    return 'bool'

        else:  # binary operator
            lvalue_type = self.node_children[0].type_check()
            rvalue_type = self.node_children[2].type_check()
            op = parser.key_op_reversed[self.node_children[1].val]

            if lvalue_type == 'ERR_TYPE' or rvalue_type == 'ERR_TYPE':
                if lvalue_type == 'ERR_TYPE':
                    lvalue_type = rvalue_type
                else:
                    rvalue_type = lvalue_type

            if op == '=':
                if lvalue_type != rvalue_type:

                    # here sym fix
                    self.err = True
                    sym_table.inc_err_cnt()
                    print('\n*** Error line {}.\n  {}\n{}{}\n*** Incompatible operands: {} {} {}\n'.format(
                        self.node_children[1].row, raw_lines[self.node_children[1].row - 1].strip('\n').strip(' '),
                        ' ' * (self.node_children[1].col_start - 1),  # TODO: MAKE SURE - 2 IS RIGHT
                        '^' * (self.node_children[1].col_end - 1 - self.node_children[1].col_start),
                        lvalue_type, op, rvalue_type))

            elif op in ['+', '-', '*', '/', '%']:

                if lvalue_type != rvalue_type or lvalue_type not in ['int', 'double'] or rvalue_type not in ['int', 'double']:

                    # here sym fix
                    self.err = True
                    sym_table.inc_err_cnt()
                    print('\n*** Error line {}.\n  {}\n{}{}\n*** Incompatible operands: {} {} {}\n'.format(
                        self.node_children[1].row, raw_lines[self.node_children[1].row-1].strip('\n').strip(' '),
                        ' '*(self.node_children[1].col_start-1), '^'*(self.node_children[1].col_end-1 - self.node_children[1].col_start),
                        lvalue_type, op, rvalue_type))
                    sym_table.active_nodes.pop()
                    return 'ERR_TYPE'
                else:
                    return lvalue_type

            elif op in ['>', '<', '>=', '<=']:
                if lvalue_type != rvalue_type or lvalue_type not in ['int', 'double'] or rvalue_type not in ['int', 'double']:

                    # here sym fix
                    self.err = True
                    sym_table.inc_err_cnt()
                    print('\n*** Error line {}.\n  {}\n{}{}\n*** Incompatible operands: {} {} {}\n'.format(
                        self.node_children[1].row, raw_lines[self.node_children[1].row - 1].strip('\n').strip(' '),
                        ' ' * (self.node_children[1].col_start - 1),
                        '^' * (self.node_children[1].col_end - 1 - self.node_children[1].col_start),
                        lvalue_type, op, rvalue_type))
                    sym_table.active_nodes.pop()
                    return 'ERR_TYPE'
                else:
                    sym_table.active_nodes.pop()
                    return 'bool'

            elif op in ['==', '!=']:
                if lvalue_type != rvalue_type:

                    # here sym fix
                    self.err = True
                    sym_table.inc_err_cnt()
                    print('\n*** Error line {}.\n  {}\n{}{}\n*** Incompatible operands: {} {} {}\n'.format(
                        self.node_children[1].row, raw_lines[self.node_children[1].row - 1].strip('\n').strip(' '),
                        ' ' * (self.node_children[1].col_start - 1),
                        '^' * (self.node_children[1].col_end - 1 - self.node_children[1].col_start),
                        lvalue_type, op, rvalue_type))
                    sym_table.active_nodes.pop()
                    return 'ERR_TYPE'
                else:
                    sym_table.active_nodes.pop()
                    return 'bool'

            elif op in ['&&', '||']:
                if lvalue_type != rvalue_type or lvalue_type != 'bool' or rvalue_type != 'bool':

                    # here sym fix
                    self.err = True
                    sym_table.inc_err_cnt()
                    print('\n*** Error line {}.\n  {}\n{}{}\n*** Incompatible operands: {} {} {}\n'.format(
                        self.node_children[1].row, raw_lines[self.node_children[1].row - 1].strip('\n').strip(' '),
                        ' ' * (self.node_children[1].col_start - 1),
                        '^' * (self.node_children[1].col_end - 1 - self.node_children[1].col_start),
                        lvalue_type, op, rvalue_type))
                    sym_table.active_nodes.pop()
                    return 'ERR_TYPE'
                else:
                    sym_table.active_nodes.pop()
                    return 'bool'


class Node_FieldAccess(ASTNode):
    """
    node_children: ident
    """
    node_type = 16
    name = 'FieldAccess'

    def check(self):
        sym_table.active_nodes.append(self.name)
        ret_type = self.type_check()
        sym_table.active_nodes.pop()
        return ret_type

    def type_check(self):
        type1 = sym_table.lookup_type(self.node_children[0].val)
        func_or_var = sym_table.var_or_func(self.node_children[0].val)  # see if identifier is func if used as identifer (FA) (TODO: JANKY)
        if func_or_var != 'var':  # TODO: duplicate (remove maybe)

            # here sym fix
            self.err = True
            sym_table.inc_err_cnt()
            print('\n*** Error line {}.\n  {}\n{}{}\n*** No declaration found for variable \'{}\'\n'.format(
                self.row, raw_lines[self.row - 1].strip('\n').strip(' '),
                ' ' * (self.node_children[0].col_start - 2),
                '^' * (self.node_children[0].col_end - self.node_children[0].col_start),
                self.node_children[0].val))
            type1 = 'ERR_TYPE'

        return type1


class Node_Call(ASTNode):
    """
        Call ::= ident ( Actuals )
        node_children = ident, actuals
    """
    node_type = 18
    name = 'Call'

    def check(self):
        sym_table.active_nodes.append(self.name)
        ret_type = self.type_check()
        sym_table.active_nodes.pop()
        return ret_type

    def type_check(self):
        # check if function in global table
        # if not: error undefined
        # check if identifiers in actuals matches type order
        # if not error out
        ident = self.node_children[0].val
        type1 = sym_table.lookup_type(ident)

        if type1 != 'ERR_TYPE':
            formal_table = sym_table.lookup_table(ident)
            formal_idents = list(formal_table.keys())
            try:
                call_actuals = self.node_children[1].node_children  # list of exprs
            except IndexError:
                call_actuals = []
            if len(formal_idents) != len(call_actuals):

                # here sym fix
                self.err = True
                sym_table.inc_err_cnt()
                print('\n*** Error line {}.\n  {}\n{}{}\n*** Function \'{}\' expects {} arguments but {} given\n'.format(
                    self.row, raw_lines[self.row - 1].strip('\n').strip(' '),
                    ' ' * (self.node_children[0].col_start - 1),
                    '^' * (self.node_children[0].col_end - self.node_children[0].col_start),
                    ident, len(formal_idents), len(call_actuals)))

            else:
                for i in range(len(formal_idents)):
                    expr_type = call_actuals[i].type_check()
                    formal_type = formal_table[formal_idents[i]].data_type  # formal_table[ident in order].data_type
                    if expr_type != formal_type:

                        # here sym fix
                        self.err = True
                        sym_table.inc_err_cnt()
                        print('\n*** Error line {}.\n  {}\n{}{}\n*** Incompatible argument {}: {} given, {} expected\n'.format(
                            call_actuals[i].row, raw_lines[call_actuals[i].row - 1].strip('\n').strip(' '),
                            ' ' * (call_actuals[i].col_start - 1),
                            '^' * (call_actuals[i].col_end - call_actuals[i].col_start),
                            i+1, expr_type, formal_type))

        else:

            # here sym fix
            self.err = True
            sym_table.inc_err_cnt()
            print('\n*** Error line {}.\n  {}\n{}{}\n*** No declaration for Function \'{}\' found\n'.format(
                self.row, raw_lines[self.row - 1].strip('\n').strip(' '),
                ' ' * (self.node_children[0].col_start - 1),
                '^' * (self.node_children[0].col_end - self.node_children[0].col_start),
                ident))

        return sym_table.lookup_type(self.node_children[0].val)  # will return type of fndecl extry in global table (unless shadowed)


class Node_Actuals(ASTNode):
    node_type = 19
    name = 'Actuals'
