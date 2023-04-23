

class SymbolEntry:
    ident = ""
    data_type = ""
    block_level = 0  # 1 = global, 2 = local/parameter
    ref_type = ""  # var or func
    scope = 0  # global, local, parameter
    offset = -1  # no clue
    formal_table = {}  # table of formals for fnDecl table entries (method checking for calls)

    register = 0

    def __init__(self, in_ident=None, in_type=None, in_block=None, in_scope=None, in_offset=None, in_formals=None, in_ref_type=None):
        self.ident = in_ident if in_ident is not None else ""
        self.data_type = in_type if in_type is not None else ""
        self.block_level = in_block if in_block is not None else 0
        self.scope = in_scope if in_scope is not None else 0
        self.offset = in_offset if in_offset is not None else 0
        self.formal_table = in_formals if in_formals is not None else {}
        self.ref_type = in_ref_type if in_ref_type is not None else ""
        self.register = 0
        self.offset = in_offset if in_offset is not None else -1


class SymbolTable:
    stack = []  # append/pop, list of dicts{ident (str) : entry (str)}
    active_nodes = []
    semantic_error_str = ""
    semantic_err = 0

    current_reg = -1
    func_label = 0  # jumps for funcs/stmts
    data_label = 0  # strings, etc.

    def __init__(self):
        self.stack = []  # list of dicts. first is global
        self.active_nodes = []
        self.semantic_error_str = ""
        self.semantic_err = 0
        self.current_reg = -1
        self.func_label = 0
        self.data_label = 0

    def get_next_reg(self):
        self.current_reg += 1
        return self.current_reg % 3  # t0, t1, t2

    def get_num_locals(self):
        num_locals = 0
        for table in reversed(self.stack[:-1]):
            num_locals += table.keys()
        return num_locals

    def get_next_func_label(self):
        self.func_label += 1
        return self.func_label

    def get_next_data_label(self):
        self.data_label += 1
        return self.data_label

    def inc_err_cnt(self):
        self.semantic_err += 1

    def add_error_msg(self, err):
        self.semantic_error_str += err

    def check_err(self):
        print(self.semantic_error_str)

    def lookup_entry(self, ident=""):
        """
        Gets most local symbolentry of ident

        :param ident: string identifier of var/func
        :return: SymbolEntry object
        """
        for table in reversed(self.stack):
            if ident in table.keys():
                return table[ident]
        return None

    def get_func_ret(self):
        for node in reversed(self.active_nodes):  # reversed so that most recent function for return shows up first
            if 'FnDecl' in node:
                ident = node.split()[1]
                return self.stack[0][ident].data_type
        return 'ERR_TYPE'

    def var_or_func(self, ident=""):
        for table in reversed(self.stack):
            if ident in table.keys():
                return table[ident].ref_type
        return 'ERR_TYPE'

    def lookup_table(self, ident=""):
        for table in reversed(self.stack):
            if ident in table.keys():
                return table[ident].formal_table
        return 'TABLE_NOT_FOUND_ERR'

    def lookup_type(self, ident=""):
        for table in reversed(self.stack):
            if ident in table.keys():
                return table[ident].data_type
        return 'ERR_TYPE'
