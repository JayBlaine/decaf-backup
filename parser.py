from decaf import token_list
from parser_classes import *
import sys

tok_iter = 0

key_op_reversed = {
    'T_Void': 'void',
    'T_Int': 'int',
    'T_Double': 'double',
    'T_Bool': 'bool',
    'T_String': 'string',
    'T_While': 'While',
    'T_If': 'If',
    'T_For': 'For',
    'T_Else': 'Else',
    'T_Return': 'Return',
    'T_Break': 'Break',
    'T_Print': 'Print',
    'T_ReadLine': 'ReadLine',
    'T_ReadInteger': 'ReadInteger',
    'T_Null': 'null',
    "'+'": '+',
    "'/'": '/',
    "'.'": '.',
    "','": ',',
    "'%'": '%',
    "'!'": '!',
    "';'": ';',
    "'<'": '<',
    "'*'": '*',
    "'-'": '-',
    "'>'": '>',
    "'('": '(',
    "')'": ')',
    "'{'": '{',
    "'}'": '}',
    "'='": '=',
    'T_Equal': "==",
    'T_NotEqual': "!=",
    'T_GreaterEqual': ">=",
    'T_LessEqual': "<=",
    'T_Or': '||',
    'T_And': '&&',
}


def reportSyntaxErr(i=-1):
    """
    9/10 fed from match_token mismatch from what's expected
    :param i: tok_iter where iter reports error
    :return:
    """
    with open(sys.argv[1]) as file:
        lines = file.readlines()

    if token_list[i].tok_val[0] == "'" and token_list[i].tok_val[-1] == "'":
        token_list[i].tok_val = token_list[i].tok_val.strip("'")  # extra quote from tokenizer correction
    token_list[i].tok_col-=1

    if token_list[i].tok_val in key_op_reversed.keys():
        token_list[i].tok_val = key_op_reversed[token_list[i].tok_val]

    print('\n*** Error line {}.\n  {}\n  {}{}\n*** syntax error\n'.format(token_list[i].tok_row,
                                                                lines[token_list[i].tok_row-1].strip('\n'),
                                                                ' '*token_list[i].tok_col,
                                                                '^'*len(token_list[i].tok_val.strip("'"))))

    exit()


def match_token(tok = ''):
    """
    Checks if next token matches some terminal. If yes, update global iterator to move on.
    :param tok:
    :param tok_iter:
    :return:
    """
    global tok_iter
    try:
        tok_iter += 1 if tok == token_list[tok_iter].tok_val else reportSyntaxErr(tok_iter)
    except IndexError:
        reportSyntaxErr(len(token_list)-1)


def func_program():
    """
    Program ::= Decl+
    :return:
    """
    prog = Node_Program()
    if len(token_list) == 0 or token_list is None:
        #print('\nEmpty program is syntactically incorrect.')
        exit()
    while tok_iter < len(token_list):
        prog.node_children.append(func_decl())

    return prog


def func_decl():
    """
    Decl ::= VariableDecl | FunctionDecl
    :return:
    """
    if token_list[tok_iter].tok_val in ('T_Int', 'T_Double', 'T_Bool', 'T_String', 'T_Void'):
        if token_list[tok_iter+1].tok_type == 'Identifier':  # identifier
            if token_list[tok_iter+2].tok_val == "'('":  # lr2 lookahead
                return func_funcdecl()
            else:
                return func_vardecl()
        else:
            reportSyntaxErr(tok_iter+1)
    else:
        reportSyntaxErr(tok_iter)


def func_vardecl():
    """
    VariableDecl ::= Variable ;
    :return:
    """
    if token_list[tok_iter].tok_val not in ('T_Int', 'T_Double', 'T_Bool', 'T_String'):
        reportSyntaxErr(tok_iter)
    if token_list[tok_iter+1].tok_type != 'Identifier':
        reportSyntaxErr(tok_iter+1)
    if token_list[tok_iter+2].tok_val != "';'":
        reportSyntaxErr(tok_iter+2)
    varDecl = func_variable()  # TODO: MIGHT THROW ERRORS SINCE CONFUSING IN BNF (TMP FIX: Name both vardecl)
    match_token("';'")
    return varDecl


def func_variable():
    """
    Variable ::= Type ident
    :return:
    """
    var1 = Node_Variable()
    var1.row = token_list[tok_iter].tok_row
    var1.col_start = token_list[tok_iter].tok_col

    if token_list[tok_iter].tok_val not in ('T_Int', 'T_Double', 'T_Bool', 'T_String'):
        reportSyntaxErr(tok_iter)
    if token_list[tok_iter+1].tok_type != 'Identifier':
        reportSyntaxErr(tok_iter+1)
    var1.node_children.append(func_type())  # type terminal
    var1.node_children.append(func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type))  # identifier terminal
    var1.col_end = token_list[tok_iter].tok_col
    return var1


def func_formals():
    """
    Formals ::= Variable+, | ϵ
    :return:
    """
    formals = Node_Formals()
    formals.row = token_list[tok_iter].tok_row
    formals.col_start = token_list[tok_iter].tok_col

    if token_list[tok_iter].tok_val == "')'":
        return formals
    formals.node_children.append(func_variable())
    while token_list[tok_iter].tok_val == "','":
        match_token("','")
        formals.node_children.append(func_variable())

    formals.col_end = token_list[tok_iter].tok_col
    return formals


def func_funcdecl():
    """
    FunctionDecl ::= Type ident ( Formals ) StmtBlock |
                     void ident ( Formals ) StmtBlock
    :return:
    """
    funcDecl = Node_FnDecl()
    funcDecl.row = token_list[tok_iter].tok_row
    funcDecl.col_start = token_list[tok_iter].tok_col

    if token_list[tok_iter].tok_val not in ('T_Int', 'T_Double', 'T_Bool', 'T_String', 'T_Void'):
        reportSyntaxErr(tok_iter)
    if token_list[tok_iter+1].tok_type != 'Identifier':  # identifier
        reportSyntaxErr(tok_iter+1)
    funcDecl.node_children.append(func_type())  # type
    funcDecl.node_children.append(func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type))  # identifier terminal
    match_token("'('")
    funcDecl.node_children.append(func_formals())
    match_token("')'")
    funcDecl.node_children.append(func_stmtBlock())

    try:  # end of file index error
        funcDecl.col_end = token_list[tok_iter].tok_col
    except IndexError:
        funcDecl.col_end = len(token_list)-1

    return funcDecl
    # TODO REMEMBER RETURN FOR FUNCDECL IS FIRST 1 FOR PRINTING


def func_stmtBlock():
    """
    StmtBlock ::= { VariableDecl∗ Stmt∗ }
    :return:
    """
    stmtBlock = Node_StmtBlock()
    stmtBlock.row = token_list[tok_iter].tok_row
    stmtBlock.col_start = token_list[tok_iter].tok_col

    match_token("'{'")
    while token_list[tok_iter].tok_val in ('T_Int', 'T_Double', 'T_Bool', 'T_String'):
        stmtBlock.node_children.append(func_vardecl())
    try:
        while token_list[tok_iter].tok_val != "'}'":
            stmtBlock.node_children.append(func_stmt())
        match_token("'}'")
        try:  # end of file index error
            stmtBlock.col_end = token_list[tok_iter].tok_col
        except IndexError:
            stmtBlock.col_end = len(token_list)-1
    except IndexError:
        match_token("'}'")

    return stmtBlock


def func_stmt():
    """
    Stmt ::= <Expr>; | IfStmt | WhileStmt | ForStmt | BreakStmt |
             ReturnStmt | PrintStmt | StmtBlock
    :return:
    """
    if token_list[tok_iter].tok_val == "T_If":
        return func_ifstmt()
    if token_list[tok_iter].tok_val == "T_While":
        return func_whilestmt()
    if token_list[tok_iter].tok_val == "T_For":
        return func_forstmt()
    if token_list[tok_iter].tok_val == "T_Return":
        return func_returnstmt()
    if token_list[tok_iter].tok_val == "T_Break":
        return func_breakstmt()
    if token_list[tok_iter].tok_val == "T_Print":
        return func_printstmt()
    if token_list[tok_iter].tok_val == "'{'":
        return func_stmtBlock()
    if token_list[tok_iter].tok_val == "';'":  # empty expr
        match_token("';'")
        #return None
    else:  # expr
        expr = func_expr()
        match_token("';'")
        # expr.col_end = token_list[tok_iter].tok_col  # TODO: CHECK IF NEEDED
        return expr


def func_ifstmt():
    """
    IfStmt ::= if ( Expr ) Stmt <else Stmt>
    :return:
    """
    ifstmt = Node_IfStmt()
    ifstmt.row = token_list[tok_iter].tok_row
    ifstmt.col_start = token_list[tok_iter].tok_col

    match_token("T_If")
    match_token("'('")
    ifstmt.node_children.append(func_expr())  # test part of if statement
    match_token("')'")
    ifstmt.node_children.append(func_stmt())  # then part of if statement


    if token_list[tok_iter].tok_val == 'T_Else':
        match_token('T_Else')
        ifstmt.node_children.append(func_stmt())  # else part of if statement
    #else:
    #    ifstmt.node_children.append(None)
    ifstmt.col_end = token_list[tok_iter].tok_col

    return ifstmt


def func_whilestmt():
    """
    WhileStmt ::= while ( Expr ) Stmt
    :return:
    """
    whilestmt = Node_WhileStmt()
    whilestmt.row = token_list[tok_iter].tok_row
    whilestmt.col_start = token_list[tok_iter].tok_col

    match_token("T_While")
    match_token("'('")
    whilestmt.node_children.append(func_expr())  # test part of while statement
    match_token("')'")
    whilestmt.node_children.append(func_stmt())  # body part of while statement

    whilestmt.col_end = token_list[tok_iter].tok_col

    return whilestmt


def func_forstmt():
    """
    ForStmt ::= for ( <Expr>; Expr ; <Expr>) Stmt
    :return:
    """
    forstmt = Node_ForStmt()
    forstmt.row = token_list[tok_iter].tok_row
    forstmt.col_start = token_list[tok_iter].tok_col
    match_token("T_For")
    match_token("'('")
    if token_list[tok_iter].tok_val != "';'":  # test for init part of for statement
        forstmt.node_children.append(func_expr())
    else:
        forstmt.node_children.append(None)

    match_token("';'")
    forstmt.node_children.append(func_expr())  # test part of for statement
    match_token("';'")

    if token_list[tok_iter].tok_val != "')'":  # test for step part of for statment
        forstmt.node_children.append(func_expr())
    else:
        forstmt.node_children.append(None)

    match_token("')'")
    forstmt.node_children.append(func_stmt())  # body part of for loop
    forstmt.col_end = token_list[tok_iter].tok_col

    return forstmt
    # TODO


def func_returnstmt():
    """
    ReturnStmt ::= return < Expr > ;
    :return:
    """
    retstmt = Node_ReturnStmt()
    retstmt.row = token_list[tok_iter].tok_row
    retstmt.col_start = token_list[tok_iter].tok_col
    match_token("T_Return")
    if token_list[tok_iter].tok_val != "';'":
        retstmt.node_children.append(func_expr())
    match_token("';'")  # BROKE CODE

    retstmt.col_end = token_list[tok_iter].tok_col
    #else:
    #    retstmt.node_children.append(None)

    return retstmt


def func_breakstmt():
    """
    BreakStmt ::= break ;
    :return:
    """
    breakstmt = Node_BreakStmt()
    breakstmt.row = token_list[tok_iter].tok_row
    breakstmt.col_start = token_list[tok_iter].tok_col
    match_token('T_Break')
    match_token("';'")
    breakstmt.col_end = token_list[tok_iter].tok_col
    return breakstmt


def func_printstmt():
    """
    PrintStmt ::= Print ( Expr+, ) ;
    :return:
    """
    printstmt = Node_PrintStmt()  # list of exprs
    printstmt.row = token_list[tok_iter].tok_row
    printstmt.col_start = token_list[tok_iter].tok_col

    match_token('T_Print')
    match_token("'('")
    printstmt.node_children.append(func_expr())
    while token_list[tok_iter].tok_val == "','":
        match_token("','")
        printstmt.node_children.append(func_expr())

    match_token("')'")
    match_token("';'")
    printstmt.col_end = token_list[tok_iter].tok_col
    return printstmt


def func_expr():
    """
    Goes down to factor to establish precendence
    Expr := LValue = RightExpr | RightExpr
    :return:
    """
    if token_list[tok_iter].tok_type == 'Identifier' and token_list[tok_iter+1].tok_val == "'='":
        assignexpr = Node_Expr()
        assignexpr.name = 'AssignExpr'
        assignexpr.row = token_list[tok_iter].tok_row
        assignexpr.col_start = token_list[tok_iter].tok_col
        # TODO: FIELD ACCESS SINCE IDENTIFIER

        assignexpr.node_children.append(func_fieldaccess())
        # identifier
        assignexpr.node_children.append(func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type))
        # Operator
        assignexpr.node_children.append(func_rightexpr())

        assignexpr.col_end = token_list[tok_iter].tok_col
        return assignexpr
    else:
        return func_rightexpr()

        # func_terminal will match token

    # order: assign > constants > call > ( expr ) > +- > */ > modular > neg > relational > logical


def func_fieldaccess():
    """
    fieldaccess := ident
    nested ident for printing (lvalue in BNF i think?)
    :return:
    """
    fieldacc = Node_FieldAccess()
    fieldacc.row = token_list[tok_iter].tok_row
    fieldacc.col_start = token_list[tok_iter].tok_col
    fieldacc.node_children.append(func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type))  # only ident in there
    fieldacc.col_end = token_list[tok_iter].tok_col
    return fieldacc


def func_rightexpr():
    """
    RightExpr := AndExpr+, ||
    :return:
    """
    expr = func_andexpr()
    # expr.col_start = token_list[tok_iter].tok_col
    while token_list[tok_iter].tok_val == "T_Or":
        op = func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type)  # T_Or
        right = func_andexpr()

        old_expr = expr
        expr = Node_Expr()
        expr.name = "LogicalExpr"
        expr.row = token_list[tok_iter].tok_row
        expr.col_start = old_expr.col_start

        expr.node_children.append(old_expr)
        expr.node_children.append(op)
        expr.node_children.append(right)

    if expr is not None:
        expr.col_end = token_list[tok_iter].tok_col
    #print(expr.col_start)
    #print(expr.col_end)
    return expr


def func_andexpr():
    """
    AndExpr := EqExpr+, &&
    :return:
    """
    expr = func_equalityexpr()
    # expr.col_start = token_list[tok_iter].tok_col
    while token_list[tok_iter].tok_val == "T_And":
        op = func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type)  # T_And
        right = func_equalityexpr()

        old_expr = expr
        expr = Node_Expr()
        expr.name = "LogicalExpr"
        expr.row = token_list[tok_iter].tok_row
        expr.col_start = old_expr.col_start

        expr.node_children.append(old_expr)
        expr.node_children.append(op)
        expr.node_children.append(right)

    if expr is not None:
        expr.col_end = token_list[tok_iter].tok_col
    return expr


def func_equalityexpr():
    """
    EqExpr := RelExpr eop RelExpr (eop:= ==|!=)
    :return:
    """
    # EqExpr := RelExpr eop RelExpr (eop:= ==|!=)
    left = func_relationalexpr()
    # left.col_start = token_list[tok_iter].tok_col
    if token_list[tok_iter].tok_val in ("T_Equal", "T_NotEqual"):
        op = func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type)
        right = func_relationalexpr()

        expr = Node_Expr()
        expr.name = 'EqualityExpr'
        expr.row = token_list[tok_iter].tok_row
        expr.col_start = token_list[tok_iter].tok_col
        expr.col_start = left.col_start

        expr.node_children.append(left)
        expr.node_children.append(op)
        expr.node_children.append(right)

        expr.col_end = token_list[tok_iter].tok_col
        return expr

    if left is not None:
        left.col_end = token_list[tok_iter].tok_col
    return left


def func_relationalexpr():
    """
    RelExpr := ArithExpr rop ArithExpr (rop:= >=|<=|>|<)
    :return:
    """
    # RelExpr := ArithExpr rop ArithExpr (rop:= >=|<=|>|<)
    left = func_arithexpr()
    # left.col_start = token_list[tok_iter].tok_col
    if token_list[tok_iter].tok_val in ("T_GreaterEqual", "T_LessEqual", "'>'", "'<'"):
        op = func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type)  # (rop:= >=|<=|>|<)
        right = func_arithexpr()

        expr = Node_Expr()
        expr.name = 'RelationalExpr'
        expr.row = token_list[tok_iter].tok_row
        expr.col_start = left.col_start

        expr.node_children.append(left)
        expr.node_children.append(op)
        expr.node_children.append(right)

        expr.col_end = token_list[tok_iter].tok_col
        return expr
    if left is not None:
        left.col_end = token_list[tok_iter].tok_col
    return left


def func_arithexpr():
    """
    ArithExpr := Term+, +|-
    :return:
    """
    expr = func_term()
    # expr.col_start = token_list[tok_iter].tok_col
    while token_list[tok_iter].tok_val in ("'+'", "'-'"):
        #expr.row = token_list[tok_iter].tok_row
        op = func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type)  # +|-
        right = func_term()
        old_expr = expr
        expr = Node_Expr()
        expr.name = "ArithmeticExpr"
        expr.row = token_list[tok_iter].tok_row
        expr.col_start = old_expr.col_start
        #expr.col_start = token_list[tok_iter].tok_col
        expr.node_children.append(old_expr)
        expr.node_children.append(op)
        expr.node_children.append(right)

    if expr is not None:
        expr.col_end = token_list[tok_iter].tok_col
    return expr


def func_term():
    """
    Term := Factor+, *|/|%
    :return:
    """
    expr = func_factor()
    # expr.col_start = token_list[tok_iter].tok_col
    while token_list[tok_iter].tok_val in ("'*'", "'/'", "'%'"):
        op = func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type)  # *|/|%
        right = func_factor()
        old_expr = expr
        expr = Node_Expr()
        expr.name = "ArithmeticExpr"
        expr.row = token_list[tok_iter].tok_row
        expr.col_start = old_expr.col_start
        #expr.col_start = token_list[tok_iter].tok_col

        expr.node_children.append(old_expr)
        expr.node_children.append(op)
        expr.node_children.append(right)

    if expr is not None:
        expr.col_end = token_list[tok_iter].tok_col
    return expr


def func_factor():
    """
    Factor := !Factor | -Factor | ident | Constant | (Expr)| Call | ReadInteger()| Readline()
    :return:
    """
    if token_list[tok_iter].tok_val == "'!'":
        ret_expr = Node_Expr()
        ret_expr.name = 'LogicalExpr'
        ret_expr.row = token_list[tok_iter].tok_row
        ret_expr.node_children.append(func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type))
        ret_expr.node_children.append(func_factor())
        return ret_expr

    elif token_list[tok_iter].tok_val == "'-'":  # TODO: May need to redo this for next phase
        ret_expr = Node_Expr()
        ret_expr.name = 'ArithmeticExpr'
        ret_expr.row = token_list[tok_iter].tok_row
        ret_expr.col_start = token_list[tok_iter].tok_col

        ret_expr.node_children.append(func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type))  # -
        ret_expr.node_children.append(func_factor())  # factor
        ret_expr.col_end = token_list[tok_iter].tok_col
        return ret_expr

    elif token_list[tok_iter].tok_val == 'T_ReadLine':
        readlineexpr = Node_Expr()
        readlineexpr.name = 'ReadLineExpr'
        readlineexpr.row = token_list[tok_iter].tok_row
        readlineexpr.col_start = token_list[tok_iter].tok_col

        match_token(token_list[tok_iter].tok_val)  # T_ReadLine
        match_token("'('")
        match_token("')'")

        readlineexpr.col_end = token_list[tok_iter].tok_col
        return readlineexpr

    elif token_list[tok_iter].tok_val == 'T_ReadInteger':
        readintexpr = Node_Expr()
        readintexpr.name = 'ReadIntegerExpr'
        readintexpr.row = token_list[tok_iter].tok_row
        readintexpr.col_start = token_list[tok_iter].tok_col

        match_token(token_list[tok_iter].tok_val)  # T_ReadInteger
        match_token("'('")
        match_token("')'")
        readintexpr.col_end = token_list[tok_iter].tok_col
        return readintexpr

    elif token_list[tok_iter].tok_type == 'Identifier' and token_list[tok_iter+1].tok_val == "'('":  # call
        return func_call()

    elif token_list[tok_iter].tok_type == 'Identifier':  # lvalue
        return func_fieldaccess()

    elif token_list[tok_iter].tok_val == "'('":  # nested expr
        match_token("'('")
        ret_expr = func_expr()
        match_token("')'")
        return ret_expr

    elif token_list[tok_iter].tok_type in ("IntConstant", "DoubleConstant", "BoolConstant", "StringConstant"):
        return func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type)

    elif token_list[tok_iter].tok_val == 'T_Null':
        return func_terminal(val='T_Null', name='Null')

    #else:
        #print(token_list[tok_iter].tok_val)


def func_call():
    """
    Call ::= ident ( Actuals )
    :return:
    """
    call = Node_Call()
    call.row = token_list[tok_iter].tok_row
    call.col_start = token_list[tok_iter].tok_col
    call.node_children.append(func_terminal(val=token_list[tok_iter].tok_val, name=token_list[tok_iter].tok_type))  # identifier terminal
    match_token("'('")
    if token_list[tok_iter].tok_val != "')'":  # if actuals and not empty
        call.node_children.append(func_actuals())  # list of exprs
    match_token("')'")

    call.col_end = token_list[tok_iter].tok_col
    return call


def func_actuals():
    """
    Actuals ::= Expr+, | ϵ
    :return:
    """
    actuals = Node_Actuals()
    actuals.row = token_list[tok_iter].tok_row
    actuals.col_start = token_list[tok_iter].tok_col
    actuals.node_children.append(func_expr())
    while token_list[tok_iter].tok_val == "','":
        match_token("','")
        actuals.node_children.append(func_expr())
    actuals.col_end = token_list[tok_iter].tok_col
    return actuals


def func_type():
    """
    Type ::= int | double | bool | string
    consumes type terminal
    :return:
    """
    node_type = Node_Terminal()
    node_type.row = token_list[tok_iter].tok_row
    node_type.col_start = token_list[tok_iter].tok_col
    node_type.name = 'Type'
    if token_list[tok_iter].tok_val in ('T_Int', 'T_Double', 'T_Bool', 'T_String', 'T_Void'):
        if token_list[tok_iter].tok_val == 'T_Int':
            node_type.val = 'int'
        elif token_list[tok_iter].tok_val == 'T_Double':
            node_type.val = 'double'
        elif token_list[tok_iter].tok_val == 'T_Bool':
            node_type.val = 'bool'
        elif token_list[tok_iter].tok_val == 'T_String':
            node_type.val = 'string'
        elif token_list[tok_iter].tok_val == 'T_Void':
            node_type.val = 'void'
        #node_type.row = token_list[tok_iter].tok_row  # TODO: types dont have rows??
        # stopgap to avoid putting in every if. Should be ok thanks to checks before term
        match_token(token_list[tok_iter].tok_val)
        node_type.col_end = token_list[tok_iter].tok_col
        return node_type
    else:
        reportSyntaxErr(tok_iter)


def func_terminal(name = None, val = None):
    """
    Populate for terminals for tree, consumes terminal in list
    :param name: Name of object i.e. identifier, constants, ops, etc.
    :param val:
    :return:
    """
    t1 = Node_Terminal()
    t1.name = name
    t1.val = val
    t1.row = token_list[tok_iter].tok_row
    t1.col_start = token_list[tok_iter].tok_col
    match_token(t1.val)
    t1.col_end = token_list[tok_iter].tok_col

    return t1
