import sys

import parser
from decaf import sym_table
from symbol_table import SymbolEntry
from code_gen import emit

semantic_err = 0
reg_offsets = [0, 0, 0]
             #t0, t1, t2
global_offsets = [-100, -100, -100, -100, -100, -100, -100, -100, -100]

most_recent_loop_label = []

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
    offset = 0
    location = ""

    def __init__(self):
        self.node_children = []
        self.err = False
        self.offset = 0
        self.location = ""

    def check_global_local(self):
        return 2

    def set_stack_location(self, offset=0):
        """
        for function calls (only stmt blocks and func return stuff)

        :return:
        """
        return 0

    def code_visit(self, offset=0):
        p=1

    def type_check(self):
        p=1

    def construct_symbols(self, parent_table=None, offset=0):
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

    def set_stack_location(self, offset=0):
        if 'Constant' in self.name or 'Null' in self.name:
            self.offset = offset
            #print('{} {} OFFSET: {}'.format(self.name, self.val, self.offset))
            return self.offset - 4
        return offset

    def code_visit(self, offset=0):
        if self.name == 'StringConstant':
            reg = sym_table.get_next_reg()
            label = sym_table.get_next_data_label()
            emit('  # tmp = {}'.format(self.val))
            emit('    .data			# create string constant marked with label')
            emit('    _data{}: .asciiz {}'.format(label, self.val))
            emit('    .text')
            emit('    la $t{}, _data{}'.format(reg, label))
            emit('    sw $t{}, {}($fp)'.format(reg, self.offset))
            reg_offsets[reg] = self.offset
            return reg

        elif self.name == 'IntConstant':
            reg = sym_table.get_next_reg()
            emit('  # tmp = {}'.format(self.val))
            emit('    li $t{}, {}'.format(reg, self.val))
            emit('    sw $t{}, {}($fp)'.format(reg, self.offset))
            reg_offsets[reg] = self.offset
            return reg

        elif self.name == 'DoubleConstant':
            reg = sym_table.get_next_reg()
            emit('  # tmp = {}'.format(self.val))
            emit('    li $t{}, {}'.format(reg, self.val))
            emit('    sw $t{}, {}($fp)'.format(reg, self.offset))
            reg_offsets[reg] = self.offset
            return reg

        elif self.name == 'BoolConstant':
            reg = sym_table.get_next_reg()
            true_false = {'true': 1, 'false': 0}
            emit('  # tmp = {}'.format(self.val))
            emit('    li $t{}, {}'.format(reg, true_false[self.val]))
            emit('    sw $t{}, {}($fp)'.format(reg, self.offset))
            reg_offsets[reg] = self.offset
            return reg

        elif self.name == 'Null':
            reg = sym_table.get_next_reg()
            emit('  # tmp = {}'.format(0))
            emit('    li $t{}, {}'.format(reg, 0))
            emit('    sw $t{}, {}($fp)'.format(reg, self.offset))
            reg_offsets[reg] = self.offset
            return reg

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

    def set_stack_location(self, offset=0):
        sym_table.stack.append(self.var_table)
        g_offset = 0
        for node in self.node_children:
            if node.name == 'VarDecl':
                node.offset = g_offset
                self.var_table[node.node_children[1].val].offset = node.offset
                #node.location = '{}($gp)'.format(g_offset)
                g_offset += 4
            else:
                node.set_stack_location(offset=0)  # local sp offsets initialized here
        sym_table.stack.pop()

    def code_visit(self, offset=0):
        sym_table.stack.append(self.var_table)
        emit('# standard Decaf preamble ')
        emit('    .text')
        emit('    .align 2')
        emit('    .globl main')
        for node in self.node_children:
            node.code_visit(offset=0)

        sym_table.stack.pop()

    def construct_symbols(self, parent_table=None, offset=0):
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

    def set_stack_location(self, offset=0):
        p=1

    def code_visit(self, offset=0):
        p=1

    def construct_symbols(self, parent_table=None, scope=2, block=1, offset=0):  # should never be none
        type1 = self.node_children[0].val
        ident = self.node_children[1].val  # type = Type, val = int, double, bool, string, void

        if ident in parent_table.keys():
            self.err = True
            print('\n*** Error line {}.\n  {}\n{}{}\n*** Duplicate key in scope: {}\n'.format(
                self.node_children[1].row, raw_lines[self.node_children[1].row - 1].strip('\n').strip(' '),
                ' ' * (self.node_children[1].col_start + 1),
                '^' * (self.node_children[1].col_end - self.node_children[1].col_start), ident))
        else:  # maintaining old entry: throw new out. CHANGE BY REMOVING ELSE AND HAVING FOLLOW IF TO OVERWRITE IN SAME SCOPE
            parent_table[ident] = SymbolEntry(in_ident=ident, in_type=type1, in_scope=scope, in_block=block, in_ref_type='var', in_offset=offset)

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

    def set_stack_location(self, offset=0):
        formal_offset = 4
        sym_table.stack.append(self.formal_table)
        for node in self.node_children[2].node_children:  # formals
            node.offset = formal_offset
            self.formal_table[node.node_children[1].val].offset = node.offset
            #print('{} OFFSET: {}'.format(node.node_children[1].val, node.offset))
            formal_offset += 4

        stmtblock = self.node_children[3]

        self.offset = (stmtblock.set_stack_location(offset=-8)+8)*-1  # -8 to make room for ra/fp
        #print('\t\tFN {} FRAME SIZE: {}'.format(self.node_children[1].val, self.offset))
        sym_table.stack.pop()

    def code_visit(self, offset=0):
        sym_table.stack.append(self.formal_table)
        if self.node_children[1].val == 'main':
            emit('{}:'.format(self.node_children[1].val))
        else:
            emit('_{}:'.format(self.node_children[1].val))
        emit('  # beginFunc {}'.format(self.offset))
        emit('    subu $sp, $sp, 8	# decrement sp to make space to save ra, fp')
        emit('    sw $fp, 8($sp)	# save fp')
        emit('    sw $ra, 4($sp)	# save ra')
        emit('    addiu $fp, $sp, 8	# set up new fp')
        emit('    subu $sp, $sp, {}	# decrement sp to make space for locals/temps'.format(self.offset))

        for node in self.node_children:
            node.code_visit()

        emit('  # EndFunc')
        emit('  # (below handles reaching end of fn body with no explicit return)')
        emit('    move $sp, $fp  # pop callee frame off stack')
        emit('    lw $ra, -4($fp)  # restore saved ra')
        emit('    lw $fp, 0($fp)  # restore saved fp')
        emit('    jr $ra  # return from function')

        sym_table.stack.pop()


    def construct_symbols(self, parent_table=None, scope=1, block=1, offset=0):
        formal_table = {}  # self.node_children[2]  TODO: REPLACES SELF.FORMAL_TABLE??
        offset=4
        for node in self.node_children[2].node_children:
            node.construct_symbols(self.formal_table, scope=2, block=2, offset=offset)  # formals (parameters)  # TODO: OFFSET MAY BE REDUNdANT NOW ( LOCATION )
            offset+=4

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

        self.node_children[3].construct_symbols(in_type='fndecl', offset=offset)  # stmtblock

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

    def set_stack_location(self, offset=0):
        sym_table.stack.append(self.var_table)
        t_offset = offset
        for node in self.node_children:
            if node.name == 'VarDecl':
                node.offset = t_offset
                self.var_table[node.node_children[1].val].offset = node.offset
                #print('{} OFFSET: {}'.format(node.node_children[1].val, node.offset))
                t_offset -= 4
            else:  # for exprs/stmts
                t_offset = node.set_stack_location(offset=t_offset)
                if t_offset < self.offset:
                    self.offset = t_offset
        sym_table.stack.pop()

        return self.offset

    def code_visit(self, offset=0):
        sym_table.stack.append(self.var_table)

        for node in self.node_children:
            #print('NODE {}: CHILDREN {}'.format(node.name,  node.node_children))
            node.code_visit()


        sym_table.stack.pop()

    def construct_symbols(self, parent_table=None, in_type=None, offset=0):
        if in_type is not None:
            self.semantic_stmt_type = in_type
        #    print(in_type)
        for node in self.node_children:
            if node is not None:
                if node.name == 'VarDecl':
                    #print('T1' + str(node.name))
                    node.construct_symbols(self.var_table, scope=2, block=2, offset=offset)  # self.var_table? ask in office hours
                    offset += 4
                else:  # stmt
                    #print('T2' + str(node.name))
                    node.construct_symbols(parent_table=parent_table, offset=offset)  # call with empty argument to build nested scopes
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

    def set_stack_location(self, offset=0):
        t_offset = offset
        for node in self.node_children:
            if node is not None:
                t_offset = node.set_stack_location(offset=t_offset)
                if t_offset < self.offset:
                    self.offset = t_offset
        return self.offset



class Node_IfStmt(Node_Stmt):
    node_type = 10
    name = 'IfStmt'

    def code_visit(self, offset=0):
        fail_branch = sym_table.get_next_func_label()
        rexpr = self.node_children[0].code_visit()
        emit('  # IfZ expr goto _L{}'.format(fail_branch))
        emit('    beqz $t{} _L{}'.format(rexpr, fail_branch))
        if_succeed = self.node_children[1].code_visit()

        if len(self.node_children) > 2:  # attached else
            end_branch = sym_table.get_next_func_label()
            emit('  # Goto _L{}'.format(end_branch))
            emit('    b _L{}	# unconditional branch'.format(end_branch))

            emit('  _L{}:'.format(fail_branch))
            if_fail = self.node_children[2].code_visit()
            emit('_L{}:'.format(end_branch))

        else:  # no else
            emit('  _L{}:'.format(fail_branch))

    def construct_symbols(self, parent_table=None, offset=0):
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


class Node_WhileStmt(Node_Stmt):
    node_type = 11
    name = 'WhileStmt'

    def code_visit(self, offset=0):
        test_expr = self.node_children[0]
        start_label = sym_table.get_next_func_label()

        emit('_L{}:'.format(start_label))
        test_reg = test_expr.code_visit()

        end_label = sym_table.get_next_func_label()
        global most_recent_loop_label
        most_recent_loop_label.append(end_label)
        emit('  # IfZ expr goto _L{}'.format(end_label))
        emit('    beqz $t{}, _L{}'.format(test_reg, end_label))

        while_stmt = self.node_children[1].code_visit()
        emit('    b _L{}	# unconditional branch'.format(start_label))
        emit('_L{}:'.format(end_label))

    def construct_symbols(self, parent_table=None, offset=0):
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


class Node_ForStmt(Node_Stmt):
    node_type = 12
    name = 'ForStmt'

    def code_visit(self, offset=0):
        init = self.node_children[0]
        test = self.node_children[1]
        incr = self.node_children[2]
        body = self.node_children[3]
        init_reg = -1
        test_reg = -1
        incr_reg = -1
        body_reg = -1
        if init is not None:
            init_reg = init.code_visit()
        test_label = sym_table.get_next_func_label()
        end_label = sym_table.get_next_func_label()
        global most_recent_loop_label
        most_recent_loop_label.append(end_label)
        emit('_L{}:'.format(test_label))

        test_reg = test.code_visit()
        emit('  # IfZ _tmp{} Goto _L{}'.format(test_reg, end_label))
        if test.check_global_local() != 2:
            emit('    lw $t{}, {}($gp)'.format(test_reg, global_offsets[test_reg]))
        else:
            emit('    lw $t{}, {}($fp)'.format(test_reg, reg_offsets[test_reg]))
        emit('    beqz $t{}, _L{}'.format(test_reg, end_label))

        if body is not None:
            body_reg = body.code_visit()
        if incr is not None:
            incr_reg = incr.code_visit()

        emit('  # Goto _L{}'.format(test_label))
        emit('    b _L{}  # unconditional branch'.format(test_label))
        emit('_L{}:'.format(end_label))

    def construct_symbols(self, parent_table=None, offset=0):
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


class Node_ReturnStmt(Node_Stmt):
    node_type = 13
    name = 'ReturnStmt'

    def code_visit(self, offset=0):
        if len(self.node_children) > 0:
            node = self.node_children[0]
            ret_reg = node.code_visit(offset=offset)
            emit('  # Return tmp')
            emit('    lw $t{}, {}($fp)	# fill tmp to $t{} from $fp{}'.format(ret_reg, reg_offsets[ret_reg], ret_reg, reg_offsets[ret_reg]))
            emit('    move $v0, $t{}		# assign return value into $v0'.format(ret_reg))
            emit('    move $sp, $fp		# pop callee frame off stack')
            emit('    lw $ra, -4($fp)	# restore saved ra')
            emit('    lw $fp, 0($fp)	# restore saved fp')
            emit('    jr $ra		# return from function')

    def check(self):
        sym_table.active_nodes.append(self.name)
        if 'FnDecl' not in "".join(sym_table.active_nodes):
            print('*** return only allowed inside a function.')  # will never hit (syntax error from parsing)
            self.err += 1

        func_type = sym_table.get_func_ret()
        if len(self.node_children) > 0:
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


class Node_BreakStmt(Node_Stmt):
    node_type = 14
    name = 'BreakStmt'

    def code_visit(self, offset=0):
        emit('  # break to most recent end label (for/while)')
        emit('    b _L{}	# unconditional branch'.format(most_recent_loop_label[-1]))
        most_recent_loop_label.pop()


    def check(self):
        sym_table.active_nodes.append(self.name)
        if 'WhileStmt' not in sym_table.active_nodes and 'ForStmt' not in sym_table.active_nodes:

            # here sym fix
            self.err = True
            sym_table.inc_err_cnt()
            print('\n*** Error line {}.\n  {}\n{}{}\n*** break is only allowed inside a loop.\n'.format(
                self.row, raw_lines[self.row - 1].strip('\n').strip(' '),
                ' ' * (self.col_start - 3),  # TODO: CHECK THIS SPACING IS RIGHT
                '^' * 5))

        sym_table.active_nodes.pop()


class Node_PrintStmt(Node_Stmt):
    node_type = 15
    name = 'PrintStmt'

    def __init__(self):
        super().__init__()
        self.node_children = []

    def code_visit(self, offset=0):
        for node in self.node_children:
            reg = node.code_visit()
            emit('  # PushParam {}'.format(node.name))
            emit('    subu $sp, $sp, 4	# decrement sp to make space for param')

            emit('    sw $t{}, 4($sp)	# copy param value to stack'.format(reg))
            type1 = node.type_check()
            if type1 == 'string':
                emit('  # LCall _PrintString')
                emit('    jal _PrintString   	# jump to function')
            elif type1 == 'bool':
                emit('  # LCall _PrintBool')
                emit('    jal _PrintBool   	# jump to function')
            else:
                emit('  # LCall _PrintInt')
                emit('    jal _PrintInt   	# jump to function')
            emit('  # PopParams 4')
            emit('    add $sp, $sp, 4  # pop params off stack')

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

    def set_stack_location(self, offset=0):
        #for node in self.node_children:
            #self.offset = node.set_stack_location(offset=offset)
        t_offset = offset
        for node in self.node_children:
            t_offset = node.set_stack_location(offset=t_offset)  # gets decremented when used

        if self.name != 'AssignExpr':  # assignexpr has offset from id it's assigning to, no need for tmp
            self.offset = t_offset
            # TODO: >=, <= need 12 byte offset (1 for ==, 1 for >/<, 1 for or to get if either are true)
            if len(self.node_children) == 3:  # binary op
                op = parser.key_op_reversed[self.node_children[1].val]  # convert back to english from lexer rep
                if op in ['>=', '<=']:  # relationalexpr needing 3 temp registers
                    t_offset -= 4
                else:                   # all other exprs
                    t_offset -= 4
            elif len(self.node_children) == 2:  # unary op
                op = parser.key_op_reversed[self.node_children[0].val]  # convert back to english from lexer rep
                if op == '!':  # need 8 for seq 0 part ( if same (0, 0) -> 1, if different (1, 0) -> 0)
                    t_offset -= 8
                else:
                    t_offset -= 4
            else:  # readexpr
                t_offset -= 4

        #t_offset-=4
        # self.offset = t_offset  # TODO: maybe remove?

        #print('OFFSET {} TMP: {}'.format(self.name, self.offset))
        return t_offset

    def code_visit(self, offset=0):

        if self.name == 'ReadIntegerExpr':
            ret_reg = sym_table.get_next_reg()
            emit('  # tmp = readint')
            emit('    li $v0, 5  # readinteger')
            emit('    syscall')
            emit('    move $t{}, $v0'.format(ret_reg))
            emit('    sw $t{}, {}($fp)'.format(ret_reg, self.offset))
            reg_offsets[ret_reg] = self.offset
            return ret_reg
        elif self.name == 'ReadLineExpr':
            ret_reg = sym_table.get_next_reg()
            emit('  # tmp = readline')

            emit('    li $a1, 40')
            emit('    la $a0, SPACE')
            emit('    li $v0, 8  # readline')
            emit('    syscall')

            emit('    la $t{}, SPACE'.format(ret_reg))
            emit('  bloop5:')
            emit('    lb $t5, ($t{})'.format(ret_reg))
            emit('    beqz $t5, eloop5')
            emit('    addi $t{}, 1'.format(ret_reg))
            emit('    b bloop5')

            emit('  eloop5:')
            emit('    addi $t{}, -1'.format(ret_reg))
            emit('    li $t6, 0')
            emit('    sb $t6, ($t{})'.format(ret_reg))
            emit('    la $v0, SPACE')

            emit('    move $t{}, $v0'.format(ret_reg))
            emit('    sw $t{}, {}($fp)'.format(ret_reg, self.offset))

            reg_offsets[ret_reg] = self.offset
            return ret_reg

        if len(self.node_children) == 2:  # unary operator
            op = parser.key_op_reversed[self.node_children[0].val]
            r1 = self.node_children[1].code_visit(offset=offset)
            if op == '!':  # result stored in middle
                r2 = sym_table.get_next_reg()
                ret_reg = sym_table.get_next_reg()
                emit('  # tmp = 0')
                emit('    li $t{}, 0'.format(r2))
                emit('    sw $t{}, {}($fp)'.format(r2, self.offset-4))
                reg_offsets[r2] = self.offset-4
                emit('  # tmp == tmp')
                emit('    lw $t{}, {}($fp)'.format(r1, reg_offsets[r1]))
                emit('    lw $t{}, {}($fp)'.format(r2, reg_offsets[r2]))
                emit('    seq $t{}, $t{}, $t{}'.format(ret_reg, r1, r2))
                emit('    sw $t{}, {}($fp)'.format(ret_reg, self.offset))
            else:
                ret_reg = sym_table.get_next_reg()
                emit('    neg $t{}, $t{}  # negate'.format(ret_reg, r1))
                emit('    sw $t{}, {}($fp)'.format(ret_reg, self.offset))
                reg_offsets[ret_reg] = self.offset
            return ret_reg

        else:  # binary operator
            op = parser.key_op_reversed[self.node_children[1].val]  # convert back to english from lexer rep
            if op == '=':
                left = self.node_children[0]
                right = self.node_children[2]
                if right.name != 'FieldAccess':
                    r2 = right.code_visit()
                else:
                    r2 = sym_table.get_next_reg()
                emit('  # {} = tmp'.format(left.node_children[0].val))

                #lreg = left.code_visit()
                if right.check_global_local() != 2:
                    emit('    lw $t{}, {}($gp)  # assignexpr'.format(r2, right.offset))
                else:
                    emit('    lw $t{}, {}($fp)  # assignexpr'.format(r2, reg_offsets[r2]))

                if left.check_global_local() != 2:
                    emit('    sw $t{}, {}($gp)  # assignexpr'.format(r2, left.offset))
                else:
                    emit('    sw $t{}, {}($fp)  # assignexpr'.format(r2, left.offset))

                return r2

            else:

                left = self.node_children[0]
                right = self.node_children[2]
                if right.name != 'FieldAccess':
                    r2 = right.code_visit() #if right.name != 'FieldAccess' else sym_table.get_next_reg()
                else:
                    r2 = sym_table.get_next_reg()
                if left.name != 'FieldAccess':
                    r1 = left.code_visit() #if right.name != 'FieldAccess' else sym_table.get_next_reg()
                else:
                    r1 = sym_table.get_next_reg()

                #r1 = left.code_visit() if left.name != 'FieldAccess' else sym_table.get_next_reg()

                ret_reg = sym_table.get_next_reg()

                emit('  # tmp {} tmp'.format(op))
                if left.check_global_local() != 2:
                    emit('    lw $t{}, {}($gp)'.format(r1, left.offset))
                else:
                    emit('    lw $t{}, {}($fp)'.format(r1, left.offset))
                if right.check_global_local() != 2:
                    emit('    lw $t{}, {}($gp)'.format(r2, right.offset))
                else:
                    emit('    lw $t{}, {}($fp)'.format(r2, right.offset))

                if op == '+':
                    emit('    add $t{}, $t{}, $t{}  # addexpr'.format(ret_reg, r1, r2))
                elif op == '-':
                    emit('    sub $t{}, $t{}, $t{}  # subexpr'.format(ret_reg, r1, r2))
                elif op == '*':
                    emit('    mul $t{}, $t{}, $t{}  # mulexpr'.format(ret_reg, r1, r2))
                elif op == '/':
                    emit('    div $t{}, $t{}, $t{}  # divexpr'.format(ret_reg, r1, r2))
                elif op == '%':
                    emit('    rem $t{}, $t{}, $t{}  # modexpr'.format(ret_reg, r1, r2))
                elif op == '&&':
                    emit('    and $t{}, $t{}, $t{}  # andexpr'.format(ret_reg, r1, r2))
                elif op == '||':
                    emit('    or $t{}, $t{}, $t{}  # orexpr'.format(ret_reg, r1, r2))

                elif op == '==':
                    emit('    seq $t{}, $t{}, $t{}  # eqexpr'.format(ret_reg, r1, r2))
                elif op == '!=':
                    emit('    sne $t{}, $t{}, $t{}  # noteqexpr'.format(ret_reg, r1, r2))
                elif op == '>=':
                    emit('    sge $t{}, $t{}, $t{}  # gteexpr'.format(ret_reg, r1, r2))
                elif op == '<=':
                    emit('    sle $t{}, $t{}, $t{}  # lteexpr'.format(ret_reg, r1, r2))
                elif op == '>':
                    emit('    sgt $t{}, $t{}, $t{}  # ltexpr'.format(ret_reg, r1, r2))
                elif op == '<':
                    emit('    slt $t{}, $t{}, $t{}  # gtexpr'.format(ret_reg, r1, r2))

                emit('    sw $t{}, {}($fp)'.format(ret_reg, self.offset))
                reg_offsets[ret_reg] = self.offset

                return ret_reg


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

    def set_stack_location(self, offset=0):
        self.offset = sym_table.lookup_entry(self.node_children[0].val).offset
        return offset

    def code_visit(self, offset=0):
        id = sym_table.lookup_entry(self.node_children[0].val)
        scope = self.check_global_local()
        r1 = sym_table.get_next_reg()
        if scope != 2:
            emit('    lw $t{}, {}($gp)  # fieldacc'.format(r1, id.offset))
            global_offsets[r1] = id.offset
        else:
            emit('    lw $t{}, {}($fp)  # fieldacc'.format(r1, id.offset))
            reg_offsets[r1] = id.offset

        return r1


    def check_global_local(self):
        id = sym_table.lookup_entry(self.node_children[0].val)
        return id.scope

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

    def set_stack_location(self, offset=0):
        t_offset = offset
        for node in self.node_children[1].node_children:
            t_offset = node.set_stack_location(offset=t_offset)

        self.offset = t_offset
        #print('CALL {} OFFSET: {}'.format(self.node_children[0].val, self.offset))
        return self.offset - 4

    def code_visit(self, offset=0):
        for node in self.node_children[1].node_children:
            if node.name != 'FieldAccess':  # to avoid unnecessary load
                node.code_visit()
        num_args = len(self.node_children[1].node_children)
        if num_args > 0:
            reg = sym_table.get_next_reg()
            param_num = num_args
            for node in reversed(self.node_children[1].node_children):
                emit('  # pushparam tmp{}'.format(param_num))
                emit('    subu $sp, $sp, 4	# decrement sp to make space for param')
                if node.name == 'FieldAccess':
                    global_local = node.check_global_local()
                    if global_local != 2:
                        emit('    lw $t{}, {}($gp)	# fill _tmp{} to $t{} from $gp{}'.format(reg, node.offset, param_num, reg, node.offset))
                    else:
                        emit('    lw $t{}, {}($fp)	# fill _tmp{} to $t{} from $fp{}'.format(reg, node.offset, param_num, reg, node.offset))
                else:
                    reg_offsets[reg] = node.offset
                    emit('    lw $t{}, {}($fp)	# fill _tmp{} to $t{} from $fp-{}'.format(reg, node.offset, param_num, reg, node.offset))
                emit('    sw $t{}, 4($sp)	# copy param value to stack'.format(reg))
                param_num-=1

        emit('  # tmp{} = LCall _{}'.format(num_args+1, self.node_children[0].val))
        emit('    jal _{}          	# jump to function'.format(self.node_children[0].val))
        reg = sym_table.get_next_reg()
        emit('    move $t{}, $v0		# copy function return value from $v0'.format(reg))
        emit('    sw $t{}, {}($fp)	# spill _tmp3 from $t{} to $fp{}'.format(reg, self.offset, reg, self.offset))
        reg_offsets[reg] = self.offset

        emit('  # PopParams {}'.format(num_args * 4))
        emit('    add $sp, $sp, {}  # pop params off stack'.format(num_args*4))

        return reg

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
                    if expr_type != formal_type and expr_type != 'ERR_TYPE':

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
