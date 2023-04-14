from decaf import sym_table

def check_link_err(ast_tree=None):
    if 'main' not in ast_tree.var_table.keys() or ast_tree.var_table['main'].ref_type != 'func':
        print('\n*** Error.\n*** Linker: function \'main\' not defined\n')
        exit()


def build_code(ast_tree=None):
    ast_tree.code_visit()


def emit(instr=None, label=None, type=0):
    p=1
