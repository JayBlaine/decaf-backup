import sys
import copy
import parser
from parser_print import print_ast
from symbol_table import SymbolTable
sym_table = SymbolTable()
token_list = []  # populate with tokens from lexer in order of appearance
semantic_error_str = ""
# Before lexer import to avoid circular import
from semantic import semantic_analyze
from lexer import tokenize
from code_gen import check_link_err, build_code


if __name__ == "__main__":
    if len(sys.argv) != 2:
        raise IOError('USAGE: python3 <init> <file>')

    tokenize()
    prog = parser.func_program()
    #prog2 = copy.deepcopy(prog)
    #print_ast(prog2, nest=0)

    semantic_analyze(ast_tree=prog)

    check_link_err(ast_tree=prog)
    build_code(ast_tree=prog)



