import sys
import re

from decaf import token_list


class Token:
    """
    token object stored in list by lexer to be read by parser
    state/path key
    -1: error
    1: string_constant: "*" (spaces allowed)
    2: num_constant: [0-9], 0x[0-9a-fA-F]{1,}
    3: int_constant
    4: double constant
    5: bool constant
    10: identifier: [a-zA-Z][a-zA-Z0-9special]{0,32} (or some other num)
    20: keyword keywords check (if buf in keywords) (while in ident path (state != 1)
        if yes: loop until found, return
    30: ops ops check (if buf in ops)
    40: comment
    """
    tok_val = ""
    tok_type = ""  # change based on final token printing in lexer
    tok_row = 0
    tok_col = 0

    def __init__(self, tok_val=None, tok_type=None, tok_row=None, tok_col=None):
        self.tok_val = tok_val if tok_val is not None else ""
        self.tok_type = tok_type if tok_type is not None else ""
        self.tok_row = tok_row if tok_row is not None else 0
        self.tok_col = tok_col if tok_col is not None else 0


keywords = ['void', 'int', 'double', 'bool', 'string', 'null', 'for', 'while',
            'if', 'else', 'return', 'break', 'Print', 'ReadInteger', 'ReadLine']
bool_constants = ['true', 'false']

keyword_dict = {
    'void': 'T_Void',
    'int': 'T_Int',
    'double': 'T_Double',
    'bool': 'T_Bool',
    'string': 'T_String',
    'while': 'T_While',
    'if': 'T_If',
    'for': 'T_For',
    'else': 'T_Else',
    'return': 'T_Return',
    'break': 'T_Break',
    'Print': 'T_Print',
    'ReadLine': 'T_ReadLine',
    'ReadInteger': 'T_ReadInteger',
    'null': 'T_Null'
}
ops_dict = {
    '+': "'+'",
    '-': "'-'",
    '*': "'*'",
    '/': "'/'",
    '.': "'.'",
    ',': "','",
    '%': "'%'",
    '!': "'!'",
    ';': "';'",
    '<': "'<'",
    '>': "'>'",
    '(': "'('",
    ')': "')'",
    '{': "'{'",
    '}': "'}'",
    '=': "'='",
    '==': "T_Equal",
    '!=': "T_NotEqual",
    '>=': "T_GreaterEqual",
    '<=': "T_LessEqual",
    '||': 'T_Or',
    '&&': 'T_And'
}
"""
state/path key
-1: error
1: string_constant: "*" (spaces allowed)
2: num_constant: [0-9], 0x[0-9a-fA-F]{1,}
3: int_constant
4: double constant
5: bool constant
10: identifier: [a-zA-Z][a-zA-Z0-9special]{0,32} (or some other num)
20: keyword keywords check (if buf in keywords) (while in ident path (state != 1)
    if yes: loop until found, return
30: ops ops check (if buf in ops)
40: comment
"""


def term_token(buf: str, col: int, row: int, state: int = -1):
    """
    :param buf: Buffer of chars read in before regex match
    :param col: Last column of char read in for token
    :param row:
    :param state: Optional argument for code for tokenization if regex match
    :return:
    """
    if state == 1:  # string constant
        #print("{:<12} line {} cols {}-{} is T_StringConstant (value = {})".format(buf, row, col, col+len(buf)-1, buf))
        new_tok = Token(tok_val=buf, tok_row=row, tok_col=col, tok_type='StringConstant')
        token_list.append(new_tok)
    elif state == 3:  # int constant
        #print("{:<12} line {} cols {}-{} is T_IntConstant (value = {})".format(buf, row, col, col+len(buf)-1, int(buf)))
        new_tok = Token(tok_val=buf, tok_row=row, tok_col=col, tok_type='IntConstant')
        token_list.append(new_tok)
        # For some reason cols not lining up
    elif state == 4:  # double constant
        num = int(float(buf)) if float(buf) % 1 == 0 else float(buf)
        #print("{:<12} line {} cols {}-{} is T_DoubleConstant (value = {})".format(buf, row, col, col+len(buf)-1, num))
        new_tok = Token(tok_val=num, tok_row=row, tok_col=col, tok_type='DoubleConstant')
        token_list.append(new_tok)
    elif state == 5:  # bool constant
        #print("{:<12} line {} cols {}-{} is T_BoolConstant (value = {})".format(buf, row, col, col+len(buf)-1, buf))
        new_tok = Token(tok_val=buf, tok_row=row, tok_col=col, tok_type='BoolConstant')
        token_list.append(new_tok)
    elif state == 10:  # identifier
        if len(buf) > 31:
            # trunc = buf[:31]
            #print("{:<12} line {} cols {}-{} is T_Identifier (truncated to {})".format(buf, row, col, col+len(buf)-1, buf[:31]))
            new_tok = Token(tok_val=buf[:31], tok_row=row, tok_col=col, tok_type='Identifier')
            token_list.append(new_tok)
            # HARD CODING COL RN, TODO
        else:
            #print("{:<12} line {} cols {}-{} is T_Identifier ".format(buf, row, col, col+len(buf)-1))
            new_tok = Token(tok_val=buf, tok_row=row, tok_col=col, tok_type='Identifier')
            token_list.append(new_tok)
    elif state == 20:  # keyword
        #print("{:<12} line {} cols {}-{} is {} ".format(buf, row, col, col+len(buf)-1, keyword_dict[buf]))
        new_tok = Token(tok_val=keyword_dict[buf], tok_row=row, tok_col=col, tok_type='Keyword')
        token_list.append(new_tok)
    elif state == 30:  # operator
        #print("{:<12} line {} cols {}-{} is {} ".format(buf, row, col, col+len(buf)-1, ops_dict[buf]))
        new_tok = Token(tok_val=ops_dict[buf], tok_row=row, tok_col=col, tok_type='Operator')
        token_list.append(new_tok)


def token_match(buf: str, row, col):  # TODO
    """
    :param buf:
    :param col:
    :param row:
    :return:
    """
    col = (col - len(buf))  # get col back to beginning, iterates up with len(buf) each loop
    col = 1 if col == 0 else col
    while len(buf) > 0:
        if re.match(r"^\"[^\n]+\"", buf):  # string constant
            matched = re.search(r"^\"[^\n]+\"", buf).group()
            buf = buf[len(str(matched)):]
            term_token(str(matched), col=col, row=row, state=1)

        elif re.match(r"^[0-9]+\.[0-9]*([Ee][+-])?[0-9]*", buf):  # double constant
            matched = re.search(r"^[0-9]+\.[0-9]*([Ee][+-][0-9])?[0-9]*", buf).group()
            buf = buf[len(str(matched)):]
            term_token(str(matched), col=col, row=row, state=4)

        elif re.match(r"^[0-9]+", buf) or re.match(r"^0[xX][0-9A-Fa-f]+", buf):  # int constant
            matched = re.search(r"^[0-9]+", buf).group()
            if matched is None:
                matched = re.search(r"^0[xX][0-9A-Fa-f]+", buf).group()
            buf = buf[len(str(matched)):]
            term_token(str(matched), col=col, row=row, state=3)

        elif re.match(r"^true", buf) or re.match(r"^false", buf):  # bool constant
            try:
                matched = re.search(r"^true", buf).group()
            except AttributeError:
                matched = re.search(r"^false", buf).group()

            buf = buf[len(str(matched)):]
            term_token(str(matched), col=col, row=row, state=5)

        elif re.match(r"^[a-zA-Z][a-zA-Z0-9_]*", buf):  # identifier
            matched = re.search(r"^[a-zA-Z][a-zA-Z0-9_]*", buf).group()
            if len(matched) > 31:
                print('\n*** Error line {}.'.format(str(row)))
                print('*** Identifier too long: "{}"\n'.format(matched))
            buf = buf[len(str(matched)):]

            if matched in keywords:
                term_token(str(matched), col=col, row=row, state=20)  # keyword
            else:
                # TODO: PASS MATCHED TRUNCATED matched[:31]
                term_token(str(matched), col=col, row=row, state=10)  # identifier

        elif re.match(r"^[<>!=]?=", buf):  # equals ops
            matched = re.search(r"^[<>!=]?=", buf).group()
            buf = buf[len(str(matched)):]
            term_token(str(matched), col=col, row=row, state=30)

        elif re.match(r"^[+*/%<>!;,.(){}\-]", buf):  # single char ops
            matched = re.search(r"^[+*/%<>!;,.(){}\-]", buf).group()
            buf = buf[len(str(matched)):]
            term_token(str(matched), col=col, row=row, state=30)

        elif re.match(r"^&&", buf) or re.match(r"^\|\|", buf):  # double char ops
            try:
                matched = re.search(r"^&&", buf).group()
            except AttributeError:
                matched = re.search(r"^\|\|", buf).group()

            buf = buf[len(str(matched)):]
            term_token(str(matched), col=col, row=row, state=30)
        else:  # no matches, error in token -> send buf to error check, recheck tokens until buf empty
            return -1, buf

        col += len(matched)
    return 0, buf


def error_check(buf, row, col):
    if re.match(r"^\"[^\n]*", buf):  # unclosed string
        matched = re.search(r"^\"[^\n]*", buf).group()
        buf = buf[len(str(matched)):]
        print('\n*** Error line {}.'.format(str(row)))
        print('*** Unterminated string constant: {}\n'.format(matched))
    elif re.match(r"^[^\n]*\"", buf):  # unclosed string opp side (NEVER HITS, REDUNDANT)
        matched = re.search(r"^[^\n]*\"", buf).group()
        buf = buf[len(str(matched)):]
        print('\n*** Error line {}.'.format(str(row)))
        print('*** Unterminated string constant: {}\n'.format(matched))
    elif re.match(r"^[^a-zA-Z0-9+\-*/%<>=!;,.{}() ]", buf):  # Unrecognized char and not in string (invalid identifier
        # ^[$@^&`~?':_]
        matched = re.search(r"^[^a-zA-Z0-9+\-*/%<>=!;,.{}() ]", buf).group()
        buf = buf[len(str(matched)):]
        print("\n*** Error line {}.".format(str(row)))
        print("*** Unrecognized char: '{}'\n".format(matched))
        return buf
    else:
        return buf
    return buf


def tokenize():
    with open(sys.argv[1]) as file:
        row = 0
        lines = file.readlines()
        buf = ""  # maintain buffer until we've expanded as far as possible for token
        string_switch = 0  # 0 = non string buffer, 1 = string buffer
        comment_switch = 0  # 0 = no comment, 1 = multiline comment, 2 = single line comment

        for line in lines:
            col = 0
            row += 1
            buf = "" if comment_switch == 0 else buf  # reset buffer if not in multiline comment
            for char in line:
                """
                SEPERATOR CHECKS: DONE
                APPEND CHAR TO BUF IF NOT SEPERATOR
                IF SEP SEND TO TOKEN MATCHING FOR TERMINATION
                """
                col += 1
                if len(buf) >= 1 and buf[-1] == '/' and (char == '*' or char == '/') and comment_switch == 0 and string_switch == 0:
                    # Not in comment or switch mode, / at end of line and getting char that would create a comment with stuff in buffer
                    # Handle edge case where comment starts right after token i.e -- int testing/*this is a comment*/
                    t_buf = buf[-1]

                    buf = buf[:-1]
                    comment_switch = 1 if char == '*' else 2
                    t_state, buf = token_match(buf, row=row, col=col-1)  # subtract one due to comment start
                    # if full tokenization from buffer -> t_state = 0, buf empty else t_state = -1 buf = leftover
                    while t_state == -1:
                        # loops through until no errors and buf empty (token_match kills buffer unless error found)
                        # if error found, loops around again and repeats above
                        buf = error_check(buf, row=row, col=col)
                        t_state, buf = token_match(buf, row=row, col=col)
                    buf = t_buf+char
                    #print('BUF: '+buf)
                    continue

                if char in [' ', '\t'] and (string_switch == 0 and comment_switch == 0):
                    # seperator and not in string/comment mode: END OF TOKEN
                    if len(buf) > 0:
                        t_state, buf = token_match(buf, row=row, col=col)
                        # if full tokenization from buffer -> t_state = 0, buf empty else t_state = -1 buf = leftover
                        while t_state == -1:
                            # loops through until no errors and buf empty (token_match kills buffer unless error found)
                            # if error found, loops around again and repeats above
                            buf = error_check(buf, row=row, col=col)
                            t_state, buf = token_match(buf, row=row, col=col)
                        buf = ""
                    continue
                elif char == '\n' and comment_switch == 0:
                    # line seperator and not in comment mode: END OF TOKEN NO MATTER WHAT
                    if len(buf) > 0:
                        if string_switch == 1:  # in string and end of line (error)
                            buf = error_check(buf, row=row, col=col)  # no need to loop (will hit since buf is ".*
                            string_switch = 0
                        else:
                            t_state, buf = token_match(buf, row=row, col=col)
                            while t_state == -1:
                                # loops through until no errors and buf empty (token_match kills buffer unless error found)
                                # if error found, loops around again and repeats above
                                buf = error_check(buf, row=row, col=col)
                                t_state, buf = token_match(buf, row=row, col=col)
                        buf = ""
                    continue
                elif char == '"' and comment_switch == 0 and string_switch == 0:  # start of string and not in any conflicting modes
                    string_switch = 1
                    if len(buf) > 0:
                        t_state, buf = token_match(buf, row=row, col=col)
                        while t_state == -1:
                            # loops through until no errors and buf empty (token_match kills buffer unless error found)
                            # if error found, loops around again and repeats above
                            buf = error_check(buf, row=row, col=col)
                            t_state, buf = token_match(buf, row=row, col=col)

                buf += char
                """
                SEPERATOR CHECKS
                """

                """
                COMMENT HANDLING: DONE
                Functionally independent of regular token looping except comment switch checks
                """
                if len(buf) >= 2:
                    if buf[0] == '/' and buf[1] == '*':  # in multiline comment, no need to do rest of checks
                        comment_switch = 1  # multiline comment
                        if len(buf) >= 4:
                            # checking for end of comment
                            if buf[-2] == '*' and buf[-1] == '/':  # END OF COMMENT
                                # clear buffer, turn off comment switch, go back to regular mode
                                buf = ""
                                comment_switch = 0
                                continue
                            else:
                                continue
                        else:
                            continue
                    elif buf[0] == '/' and buf[1] == '/':
                        comment_switch = 2  # single line comment
                        if char == '\n':  # END OF COMMENT
                            # clear buffer, turn off comment switch, go back to regular mode
                            buf = ""
                            comment_switch = 0
                            continue
                        else:
                            # still in comment mode, already appended char, skip string check and continue until end of comment
                            continue
                """
                COMMENT HANDLING: DONE
                """

                """
                STRING CONSTANT HANDLING: DONE
                """
                if len(buf) >= 1:
                    if buf[0] == '"':
                        string_switch = 1
                        if re.fullmatch(r"^\"[^\n]*\"", buf):  # string constant
                            term_token(buf, col, 1)
                            string_switch = 0
                            temp_col = (col - len(buf)) + 1  # getting col of start of string
                            term_token(str(buf), col=temp_col, row=row, state=1)
                            buf = ""
                        else:
                            continue
                        continue
                """
                STRING CONSTANT HANDLING:
                """

        # OUTSIDE LINE LOOP (EOF CHECK)
        if len(buf) > 0:
            # NO newline before EOF, still stuff in buffer
            if comment_switch == 0:
                t_state, buf = token_match(buf, row=row, col=col)
                while t_state == -1:
                    buf = error_check(buf, row=row, col=col)
                    t_state, buf = token_match(buf, row=row, col=col)
                buf = error_check(buf, row=row, col=col)
