import parser


def print_ast(node, nest=0):
    """
    goes depth first into child list of each node and printing with whitespace determined by an iterating nest value.
    If node has special print structure i.e. call args/stmt decorators, are handled and returned in a special statement.
    :param node:
    :param nest:
    :return:
    """
    if node is None:
        return
    if 'Program' in node.name:
        print('\n   Program: ')

    elif 'Type' in node.name:
        print('{}{}: {}'.format(' ' * 3 * (nest+1), node.name, node.val))

    elif node.node_type == 30:  # terminal node (ops, constants, idents)
        if node.val in parser.key_op_reversed.keys():  # go from terminal id to printing value to match diff
            node.val = parser.key_op_reversed[node.val]
        print('{:>3s}{}{}: {}'.format(str(node.row), ' '*3*nest, node.name, node.val))

    elif 'VarDecl' in node.name:
        print('{:>3s}{}{}: '.format(str(node.row), ' '*3*nest, node.name))

    elif 'FnDecl' in node.name:
        print('{:>3s}{}{}: '.format(str(node.row), ' '*3*nest, node.name))
        ret = node.node_children[0]
        print('{}(return type) {}: {}'.format(' '*3*(nest+2), ret.name, ret.val))  # ret type
        node.node_children = node.node_children[1:]  # remove from list for recurse after printing
        id = node.node_children[0]  # fncdecl identifier
        print('{:>3s}{}{}: {}'.format(str(id.row), ' '*3*(nest+1), id.name, id.val))
        node.node_children = node.node_children[1:]  # remove from list for recurse after printing
        formal_list = node.node_children[0].node_children  # list of formals
        for i in formal_list:
            print('{:>3s}{}(formals) {}: '.format(str(i.row), ' '*3*(nest+1),i.name))
            for j in i.node_children:
                print_ast(j, nest=nest+2)
        node.node_children = node.node_children[1:]

        body = node.node_children[0]
        print('{}(body) {}: '.format(' ' * 3 * (nest + 2), body.name))
        for i in body.node_children:
            print_ast(i, nest=nest + 2)
        return

    elif 'PrintStmt' in node.name:  # TODO: WILL NEED DEBUGGING
        print('{}PrintStmt: '.format(' '*3*(nest+1)))  # + 1 because no line #
        for i in node.node_children:  # iterate through list of exprs
            i.name = '(args) ' + i.name
            print_ast(i, nest=nest+1)
        return  # since recursing in loop, need to return to not hit recurse case at end of func

    elif 'BreakStmt' in node.name:
        print('{}{}: '.format(' '*3*(nest+1), node.name))  # + 1 because no line #

    elif 'WhileStmt' in node.name:
        print('{}{}: '.format(' ' * 3 * (nest + 1), node.name))  # + 1 because no line #
        test = node.node_children[0]
        print('{:>3s}{}(test) {}: '.format(str(test.row), ' ' * 3 * (nest + 1), test.name))
        for i in test.node_children:
            print_ast(i, nest=nest + 2)

        if len(node.node_children) > 1:
            if node.node_children[1] is None:  # empty stmt
                print('{}(body) Empty: '.format(' ' * 3 * (nest + 2)))
            else:
                stmt = node.node_children[1]
                print('{}(body) {}: '.format(' ' * 3 * (nest + 2), stmt.name))
                for i in stmt.node_children:
                    print_ast(i, nest=nest + 2)
        return  # since recursing in loop, need to return to not hit recurse case at end of func

    elif 'IfStmt' in node.name:
        print('{}{}: '.format(' ' * 3 * (nest + 1), node.name))  # + 1 because no line #
        test = node.node_children[0]
        print('{:>3s}{}(test) {}: '.format(str(test.row), ' '*3*(nest+1), test.name))
        for i in test.node_children:
            print_ast(i, nest=nest+2)

        if node.node_children[1] is None:  # empty stmt
            print('{:>3s}{}(then) Empty: '.format(str(test.row), ' ' * 3 * (nest + 1)))
        else:
            then = node.node_children[1]
            print('{:>3s}{}(then) {}: '.format(str(then.row), ' ' * 3 * (nest + 1), then.name))
            for i in then.node_children:
                print_ast(i, nest=nest + 2)

        if len(node.node_children) > 2:
            elsestmt = node.node_children[2]
            print('{:>3s}{}(else) {}: '.format(str(elsestmt.row), ' ' * 3 * (nest + 1), elsestmt.name))
            for i in elsestmt.node_children:
                print_ast(i, nest=nest + 2)
        return  # since recursing in loop, need to return to not hit recurse case at end of func

    elif 'ReturnStmt' in node.name:
        print('{:>3s}{}{}: '.format(str(node.row), ' '*3*(nest), node.name))  # + 1 because no line #
        if len(node.node_children) == 0:
            print('{}Empty: '.format(' ' * 3 * (nest+2)))  # no return

    elif 'ForStmt' in node.name:
        print('{}{}: '.format(' ' *3*(nest+1), node.name))  # + 1 because no line #
        # None gets added and checked rather than skipping. Since there can be empty init and/or step, we need a placeholder
        if node.node_children[0] is None:  # init can be empty
            print('{}(init) Empty: '.format(' '*3*(nest+2)))
        else:
            init = node.node_children[0]
            print('{:>3s}{}(init) {}: '.format(str(init.row), ' ' * 3 * (nest + 1), init.name))
            for i in init.node_children:
                print_ast(i, nest=nest + 2)

        test = node.node_children[1]
        print('{:>3s}{}(test) {}: '.format(str(test.row), ' ' * 3 * (nest + 1), test.name))
        for i in test.node_children:
            print_ast(i, nest=nest + 2)

        if node.node_children[2] is None:  # step can be empty
            print('{}(step) Empty: '.format(' '*3*(nest+2)))
        else:
            step = node.node_children[2]
            print('{:>3s}{}(step) {}: '.format(str(step.row), ' ' * 3 * (nest + 1), step.name))
            for i in step.node_children:
                print_ast(i, nest=nest + 2)

        if node.node_children[3] is None:  # body can be empty statement
            print('{}(body) Empty: '.format(' ' * 3 * (nest + 2)))
        else:
            body = node.node_children[3]
            print('{}(body) {}: '.format(' ' * 3 * (nest + 2), body.name))
            for i in body.node_children:
                print_ast(i, nest=nest + 2)
        return  # since recursing in loop, need to return to not hit recurse case at end of func

    elif 'FieldAccess' in node.name:
        print('{:>3s}{}{}: '.format(str(node.row), ' '*3*(nest), node.name))

    elif 'ArithmeticExpr' in node.name:
        print('{:>3s}{}{}: '.format(str(node.row), ' '*3*(nest), node.name))

    elif 'AssignExpr' in node.name:
        print('{:>3s}{}{}: '.format(str(node.row), ' '*3*(nest), node.name))

    elif 'EqualityExpr' in node.name:
        print('{:>3s}{}{}: '.format(str(node.row), ' '*3*(nest), node.name))

    elif 'LogicalExpr' in node.name:
        print('{:>3s}{}{}: '.format(str(node.row), ' '*3*(nest), node.name))

    elif 'RelationalExpr' in node.name:
        print('{:>3s}{}{}: '.format(str(node.row), ' '*3*(nest), node.name))

    elif 'ReadIntegerExpr' in node.name:
        print('{:>3s}{}{}: '.format(str(node.row), ' '*3*(nest), node.name))

    elif 'ReadLineExpr' in node.name:
        print('{:>3s}{}{}: '.format(str(node.row), ' '*3*(nest), node.name))

    elif 'Call' in node.name:
        # Call ::= ident ( Actuals )
        print('{:>3s}{}{}: '.format(str(node.row), ' '*3*(nest), node.name))  # call print
        id = node.node_children[0]
        print('{:>3s}{}Identifier: {}'.format(str(id.row), ' ' * 3 * (nest+1), id.val))  # id print

        if len(node.node_children) > 1:  # Will trigger if actuals in call, can be empty
            for i in node.node_children[1].node_children:  # actuals
                print('{:>3s}{}(actuals) {}: '.format(str(i.row), ' ' * 3 * (nest + 1), i.name), end='')
                print(i.val) if i.val is not None else print()
                for j in i.node_children:  # children of actuals
                    print_ast(j, nest + 2)
        return  # since recursing in loop, need to return to not hit recurse case at end of func

    for nodes in node.node_children:  # main recursive definition if not handled and returned by special nodes.
        print_ast(nodes, nest=nest+1)
