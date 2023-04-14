

def test_sym_table(node=None):
    if node is not None:
        if node.name == 'Program':
            print('GLOBAL SCOPE: {}'.format(node.var_table))
        elif node.name == 'FnDecl':
            print('FORMAL SCOPE: {}'.format(node.formal_table))
        elif node.name == 'StmtBlock':
            print('STMTBLOCK SCOPE: {}'.format(node.var_table))

        for p in node.node_children:
            test_sym_table(p)


def check_semantic_errs(node=None):
    if node is not None:
        if node.err:
            exit()
        for child in node.node_children:
            check_semantic_errs(child)


def semantic_analyze(ast_tree=None):
    ast_tree.construct_symbols()

    #test_sym_table(ast_tree)

    ast_tree.check()
    check_semantic_errs(ast_tree)



    # TODO: GO THROUGH TREE AND CHECK EXPR TYPES


