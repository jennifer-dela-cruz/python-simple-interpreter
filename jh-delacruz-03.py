from enum import Enum
import math
import re
from tabulate import tabulate
# Install Tabulate [pip install tabulate] or go to this link [https://pypi.org/project/tabulate/]

# Token types
# EOF (end-of-file) token is used to indicate that
# there is no more input left for lexical analysis
ID      = 'ID'
CREATE  = 'CREATE'
RUPTURE = 'RUPTURE'
DINT    = 'DINT'
DSTR    = 'DSTR'
WITH    = 'WITH'
STORE   = 'STORE'
IN      = 'IN'
INPUT    = 'INPUT'
OUTPUT   = 'OUTPUT'
OUTPUT_WITH_LINE = 'OUTPUT_WITH_LINE'
PLUS    = 'PLUS'
MINUS   = 'MINUS'
TIMES   = 'TIMES'
DIVBY   = 'DIVBY'
MODU    = 'MODU'
RAISE   = 'RAISE'
ROOT    = 'ROOT'
MEAN    = 'MEAN'
DIST    = 'DIST'
AND     = 'AND'
POSITIVE    = 'POSITIVE'
NEGATIVE   = 'NEGATIVE'
LSBRACKET = 'LSBRACKET'
RSBRACKET = 'RSBRACKET'
NUMBER   = 'NUMBER' # Integer value
STRING   = 'STRING' # String literal
VAR      = 'IDENTIFIER' # Variable name
EOS      = 'EOS' # End of statement
EOF      = 'EOF' # End of file no more input
ERR      = 'ERROR' # Error token not found
INTEGER   = 'INTEGER'


# Error codes and description
class ErrorCode(Enum):
    INVALID_CHARACTER               = 'Invalid character'
    INVALID_SYNTAX                  = 'Syntax is invalid'
    VARIABLE_IS_NOT_DECLARED        = 'A variable must be declared before use'
    INVALID_ARITHMETIC_OPERATION    = 'General error for'
    INVALID_EXPRESSION              = 'Invalid expression'
    INVALID_DATA_TYPE               = 'Invalid data type'
    DUPLICATE_VARIABLE_DECLARATION  = 'Variable name has been declared before'
    INCOMPATIBLE_DATA_TYPE          = 'Incompatible data types'
    INVALID_DATA_TYPE_INPUT         = 'Data type inputted is invalid'
    INVALID_EOF                     = 'There is no RUPTURE'
    FILE_IS_EMPTY                   = 'File is empty'
    FILE_NOT_FOUND                  = 'File does not exist'
    INVALID_FILE                    = 'Input file does not end with .ipol'


class Error(Exception):
    def __init__(self, error_code=None, lineno=None, token_list=None, message=None):
        """
        Sample: Invalid character at line number [ 2 ] ----> DSTR stringmo WITH "
        ErrorCode.Code [ lineno ] ----> <lineno tokens>
        """
        self.error_ = error_code
        self.lineno = lineno
        self.token_list = token_list
        # add exception class name before the message
        # JHD Must display the tokens in the whole line
        self.message = f'{message}'


class Token(object):
    def __init__(self, type, name, value):
        # token type: ADD
        self.type = type
        # token name (long description): BASIC_OPERATOR_ADD
        self.name = name
        # token value: non-negative integer value, '+', '-', '*', '/', or None
        self.value = value

    def __str__(self):
        """String representation of the class instance.
        Examples:
            Token(NUMBER, 3)
            Token(ADD, '+')
            Token(MUL, '*')
        """
        return 'Token({type}, {name}, {value})'.format(
            type=self.type,
            name=self.name,
            value=repr(self.value)
        )

    def __repr__(self):
        return self.__str__()

# JHD The type and the value are the same as it is a reserved keyword
# JHD Token (type, name, value)
RESERVED_KEYWORDS = {
    'CREATE':       Token('CREATE', 'PROGRAM_CREATE', 'CREATE'),
    'RUPTURE':      Token('RUPTURE', 'PROGRAM_RUPTURE', 'RUPTURE'),
    'DSTR':         Token('DSTR', 'DECLARATION_STRING', 'DSTR'),
    'DINT':         Token('DINT', 'DECLARATION_INT', 'DINT'),
    'INPUT':      Token('INPUT', 'INPUT', 'GIVEME?'),
    'OUTPUT':     Token('OUTPUT', 'OUTPUT', 'GIVEYOU!'),
    'OUTPUT_WITH_LINE':    Token('OUTPUT_WITH_LINE', 'OUTPUT_WITH_LINE', 'GIVEYOU!!'),
    'WITH':         Token('WITH', 'DECLARATION_ASSIGN_WITH_KEY', 'WITH'),
    'STORE':        Token('STORE', 'ASSIGN_KEY', 'STORE'),
    'IN':           Token('IN', 'ASSIGN_VAR_KEY', 'IN'),
    'PLUS':         Token('PLUS', 'BASIC_OPERATOR_ADD', 'PLUS'),
    'MINUS':        Token('MINUS', 'BASIC_OPERATOR_SUB', 'MINUS'),
    'TIMES':        Token('TIMES', 'BASIC_OPERATOR_MUL', 'TIMES'),
    'DIVBY':        Token('DIVBY', 'BASIC_OPERATOR_DIV', 'DIVBY'),
    'MODU':         Token('MODU', 'BASIC_OPERATOR_MOD', 'MODU'),
    'RAISE':        Token('RAISE', 'ADVANCED_OPERATOR_EXP', 'RAISE'),
    'ROOT':         Token('ROOT', 'ADVANCED_OPERATOR_ROOT', 'ROOT'),
    'MEAN':         Token('MEAN', 'ADVANCED_OPERATOR_AVE', 'MEAN'),
    'DIST':         Token('DIST', 'ADVANCED_OPERATOR_DIST', 'DIST'),
    'AND':          Token('AND', 'DISTANCE_SEPARATOR', 'AND')
}

class Lexer(object):
    def __init__(self, text):
        # client string input, e.g. "3 * 5", "12 / 3 * 4", etc
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.lineno = 0
        self.current_char = self.text[self.pos]
        self.previous_char = self.text[self.pos-1]

    def error(self, error_code):
        """
        Sample: Invalid character at line number [ 2 ] ----> DSTR stringmo WITH "
        ErrorCode.Code [ lineno ] ----> <lineno tokens>
        """
        #token_list = self.current_char
        token_list = "{0} {1}".format(self.peek_previous_until_EOS(),self.current_char)

        raise Error(
            error_code=error_code,
            lineno=self.lineno,
            token_list=token_list,
            message=f'{error_code.value} at line [ {self.lineno} ] ----> {token_list}',
        )



    def advance(self):
        """Advance the `pos` pointer and set the `current_char` variable."""
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None  # Indicates end of input
        else:
            if self.previous_char == '\n':
                self.lineno += 1
            self.previous_char = self.current_char
            self.current_char = self.text[self.pos]

    def peek(self):
        """
        Checks the next character without actual advance
        """
        peek_pos = self.pos + 1
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def peek_previous_until_EOS(self):
        """
        Checks the previous characters until EOS
        """
        text = ''
        peek_pos = self.pos - 1
        while self.text[peek_pos] != '\n':
            text = text + self.text[peek_pos]
            peek_pos = peek_pos - 1
        text = text[::-1]

        return text

    def skip_whitespace(self):
        """
        Skips whitespace chars
        """
        while self.current_char is not None and self.current_char.isspace() and self.current_char != '\n':
            self.advance()

    def skip_comment(self):
        """
        Skips comments until EOS
        """
        while self.current_char != '\n':
            self.advance()
        self.advance()  # the EOS \n

    def integer(self):
        """Return a (multidigit) integer consumed from the input."""
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()
        return int(result)

    def _id(self):
        """Handle identifiers and reserved keywords"""
        result = ''

        while self.current_char is not None and self._isalnum(self.current_char):
            result += self.current_char
            self.advance()

        if result == "GIVEME" and self.current_char == '?':
            self.advance()
            token = RESERVED_KEYWORDS.get(INPUT)
        elif result == "GIVEYOU" and self.current_char == '!' and self.peek() == '!':
            self.advance()
            self.advance()
            token = RESERVED_KEYWORDS.get(OUTPUT_WITH_LINE)
        elif result == "GIVEYOU" and self.current_char == '!':
            self.advance()
            token = RESERVED_KEYWORDS.get(OUTPUT)
        elif result[0].isalpha() and len(result) < 50:
            # JHD: If it is not a Reserved Word and variable, ID must be returned
            # the 2nd parameter is the Value to be returned in case key does not exist.
            token = RESERVED_KEYWORDS.get(result, Token(ID, 'IDENTIFIER', result))

        return token

    def _string(self):
        """Handle strings"""
        result = ''

        while self.current_char is not None and self.current_char.isascii() and self.current_char != ']':
            result += self.current_char
            self.advance()

        return Token(STRING, STRING, result)

    def _isalnum(self, value):
        """
        For checking if value is with alphanumeric characters
        Returns True if value, otherwise False
        """
        return bool(re.match('^[a-zA-Z0-9]+$', value))

    def get_next_token(self):
        """Lexical analyzer (also known as scanner or tokenizer)
        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        while self.current_char is not None:

            if self.current_char == '\n':
                self.advance()
                return Token(EOS, 'END_OF_STATEMENT', EOS)

            if self.previous_char == '[' and self.current_char.isascii() and self.current_char != ']':
                return self._string()

            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char == '#':
                self.advance()
                self.skip_comment()
                continue

            if self.current_char.isdigit():
                return Token(NUMBER, NUMBER, self.integer())

            if self._isalnum(self.current_char):
                return self._id()

            if self.current_char == '[':
                self.advance()
                return Token(LSBRACKET, LSBRACKET, '[')

            if self.current_char == ']':
                self.advance()
                return Token(RSBRACKET, RSBRACKET, ']')

            if self.current_char == '+':
                self.advance()
                return Token('POSITIVE', 'PLUS UNARY OPERATOR', '+')

            if self.current_char == '-':
                self.advance()
                return Token('NEGATIVE', 'MINUS UNARY OPERATOR', '-')

            self.lineno += 1
            self.error(ErrorCode.INVALID_SYNTAX)


        return Token(EOF, 'END_OF_FILE', EOF)


class SymbolTable(object):
    def __init__(self):
        self._symbols = {}

    def __str__(self):
        """
        This is for the text representation
        """
        s = 'Symbols: VARIABLE NAME    TYPE     VALUE \n'

        # JHD Fix this later. Must be in table format and correct sequence
        for key, value in self._symbols.items():
           s += "{0} {1} {2}\n".format(key, value[0], value[1])

        return s

    __repr__ = __str__

    def define(self, name, type, value):
        """
        This will insert a list [type, value] in the dictionary with name as the key
        """
        if type == NUMBER:
            type = INTEGER
        self._symbols[name] = [type, value]

    def lookup(self, name):
        """
        This will return a list [type, value] based on the key <name> provided
        """
        result = self._symbols.get(name)

        return result

    def get_list(self):
        """
        This is for the list representation
        """
        s = []
        if len(self._symbols) > 0:
            for key, value in self._symbols.items():
                s.append([key, value[0], value[1]])
        else:
            s.append(['','',''])

        return s


class TokensTable(object):
    def __init__(self):
        self.lexemes = []

    def __str__(self):
        """
        Returns all the lexemes in this format -  LINE NO.    TOKENS     LEXEMES
        """
        #JHD update this later
        s = 'Lexemes: LINE NO.    TOKENS     LEXEMES \n'

        # JHD Fix this later. Must be in table format and correct sequence
        for value in self.lexemes:
           s += "{0} {1} {2}\n".format(value[0], value[1], value[2])

        return s

    __repr__ = __str__

    def define(self, lineno, token_name, lexeme):
        """
        This inserts tokens as list
        """
        self.lexemes.append([lineno, token_name, lexeme])

    def lookup(self, lineno):
        """
       Returns all the lexemes per line in this format - LEXEMES <space> LEXEMES
       """
        result = ''
        for value in self.lexemes:
            if value[0] == lineno:
                result += "{0} ".format(value[2])

        return result

    def get_list(self):
        """
        Converts the tokens table into list
        """
        s = []
        if len(self.lexemes) > 0:
            for value in self.lexemes:
                s.append([value[0], value[1], value[2]])
        else:
            s.append(['','',''])

        return s


class Interpreter(object):
    def __init__(self, lexer, _symbols, tokens):
        self.lexer = lexer
        self._symbols = _symbols
        self.tokens = tokens
        # set current token to the first token taken from the input
        self.current_token = self.lexer.get_next_token()

    def error(self, error_code):
        """
        Sample: Invalid character at line number [ 2 ] ----> DSTR stringmo WITH "
        ErrorCode.Code [ lineno ] ----> <lineno tokens>
        """
        tokens = self.tokens.lookup(self.lexer.lineno)
        if not tokens:
            tokens = "{0}{1}".format(self.current_token.value, self.lexer.current_char)
        tokens = tokens.replace("EOS"," ")

        raise Error(
            error_code=error_code,
            lineno=self.lexer.lineno,
            token_list=tokens,
            message=f'{error_code.value} at line [ {self.lexer.lineno} ] ----> {tokens}',
        )


    def eat(self, token_type):
        # compare the current token type with the passed token
        # type and if they match then "eat" the current token
        # and assign the next token to the self.current_token,
        # otherwise raise an exception.
        if self.current_token.type == token_type:
            # JHD Change the line number here later
            if self.current_token.type == EOF:
                self.tokens.define(self.lexer.lineno+1, self.current_token.name, self.current_token.value)
            else:
                self.tokens.define(self.lexer.lineno, self.current_token.name, self.current_token.value)
            if self.current_token.type != EOF:
                self.current_token = self.lexer.get_next_token()
        else:
            self.error(ErrorCode.INVALID_SYNTAX)

    # JHD This is the first checking of the token based on the grammar rules
    def parse(self):
        """
        <program> : BEGIN <statement_list> END
        <statement_list> : <statement> | <statement> EOS  <statement_list>
        <statement> : <assign_statement> |  <get_input>    | <write_output>
        <assignment_statement> : DINT <variable> ( WITH expr()  )
                | DSTR <variable> ( WITH  expr() | [ STRING ]  )
                | STORE <expr> IN <variable>
        <get_input> : GIVEME? ID
        <write_output> : GIVEYOU! <expr>
                | GIVEYOU! ID
                | GIVEYOU! [ STRING ]
                | GIVEYOU!! <expr>  \n
                | GIVEYOU!! ID \n
                | GIVEYOU!! [ STRING ] \n
        <expr> : <factor>
                |PLUS<expr><expr>
                |MINUS<expr><expr>
                |TIMES<expr><expr>
                |DIVBY<expr><expr>
                |MODU<expr><expr>
                |RAISE<expr><factor>
                |ROOT<factor><expr>
                |MEAN<expr>(<expr><expr>)*
                |DIST<expr><expr>AND<expr><expr>
        """
        while self.current_token.type == EOS:
            self.eat(EOS)

        self.program()

        while self.current_token.type == EOS:
            self.eat(EOS)

        if self.current_token.type == EOF:
            self.eat(EOF)
        else:
            self.error(ErrorCode.INVALID_SYNTAX)


    # JHD This is the 1st grammar rule to check
    def program(self):
        """
        program: BEGIN statement_list END
        """
        self.eat(CREATE)
        self.statement_list()
        if self.current_token.type == RUPTURE:
            self.eat(RUPTURE)
        else:
            self.error(ErrorCode.INVALID_SYNTAX)

    def statement_list(self):
        """
        <statement> | <statement> EOS  <statement_list>
        """
        result = ''
        while self.current_token.type == EOS:
            self.eat(EOS)
            if not self.current_token.type == RUPTURE:
                self.statement()

    def statement(self):
        """
       <assign_statement>
            |  <get_input>
            | <write_output>
        """

        # <assign_statement>
        if self.current_token.type == DINT or self.current_token.type == DSTR or self.current_token.type == STORE:
            self.assign_statement()

        # |  <get_input>
        elif self.current_token.type == INPUT:
            self.get_input()

        # | <write_output>
        elif self.current_token.type == OUTPUT or self.current_token.type == OUTPUT_WITH_LINE:
            self.write_output()


    def assign_statement(self):
        """
        DINT <variable> ( WITH expr()  )
            | DSTR <variable> ( WITH  expr() | [ STRING ]  )
            | STORE <expr> IN <variable>
        """
        var_name = ''
        result = ''

        # Integer assignment
        # DINT <variable> ( WITH expr()  )
        if self.current_token.type == DINT:
            self.eat(DINT)
            var_name = self.current_token.value

            # Check if variable is declared already
            var = self._symbols.lookup(var_name)
            self.eat(ID)
            if var is not None:
                self.error(ErrorCode.DUPLICATE_VARIABLE_DECLARATION)

            if self.current_token.type == WITH:
                self.eat(WITH)
                result = self.expr()
            else:
                result = 0
            # Define variables
            self._symbols.define(var_name, NUMBER, result)

        # String assignment
        # | DSTR <variable> ( WITH  expr() | [ STRING ]  )
        elif self.current_token.type == DSTR:
            self.eat(DSTR)
            var_name = self.current_token.value

            # Check if variable is declared already
            var = self._symbols.lookup(var_name)
            self.eat(ID)
            if var is not None:
                self.error(ErrorCode.DUPLICATE_VARIABLE_DECLARATION)

            if self.current_token.type == WITH:
                self.eat(WITH)
                if self.current_token.type == LSBRACKET:
                    self.eat(LSBRACKET)
                    if self.current_token.type != RSBRACKET:
                        result = self.current_token.value
                        self.eat(STRING)
                    else:
                        result = ''
                    self.eat(RSBRACKET)
                else:
                    result = str(self.expr())
            else:
                result = ''
            # Define variables
            self._symbols.define(var_name, STRING, result)

        # Integer assignment
        # | STORE <expr> IN <variable>
        elif self.current_token.type == STORE:
            self.eat(STORE)
            result = self.expr()
            self.eat(IN)
            var_name = self.current_token.value

            # Check if variable is declared already
            var = self._symbols.lookup(var_name)
            self.eat(ID)
            if var is not None:
                # Update the value of the variable
                self._symbols.define(var_name, NUMBER, result)
            else:
                self.error(ErrorCode.VARIABLE_IS_NOT_DECLARED)



    def get_input(self):
        """
        GIVEME? ID
        """
        self.eat(INPUT)
        var_name = self.current_token.value
        self.eat(ID)

        # Check if variable is existing in the symbols table before using
        var = self._symbols.lookup(var_name)
        if var is not None:
            var_type = var[0]
            var_value = input('')
            self._symbols.define(var_name, var_type, str(var_value))
        else:
            self.error(ErrorCode.VARIABLE_IS_NOT_DECLARED)

    def write_output(self):
        """
        GIVEYOU! <expr>
            | GIVEYOU! ID
            | GIVEYOU! [ STRING ]
            | GIVEYOU!! <expr>  \n
            | GIVEYOU!! ID \n
            | GIVEYOU!! [ STRING ] \n
        """
        result = ''

        if self.current_token.type == OUTPUT:
            self.eat(OUTPUT)

            # | GIVEYOU! ID
            if self.current_token.type == ID:
                # Check if variable is existing in the symbols table before using
                var = self._symbols.lookup(self.current_token.value)
                self.eat(ID)
                if var is None:
                    self.error(ErrorCode.VARIABLE_IS_NOT_DECLARED)
                else:
                    result = var[1]

            # | GIVEYOU! [ STRING ]
            elif self.current_token.type == LSBRACKET:
                self.eat(LSBRACKET)
                if self.current_token.type != RSBRACKET:
                    result = self.current_token.value
                    self.eat(STRING)
                self.eat(RSBRACKET)

            # GIVEYOU! <expr>
            else:
                result = self.expr()
            result = "{0}".format(str(result))


        elif self.current_token.type == OUTPUT_WITH_LINE:
            self.eat(OUTPUT_WITH_LINE)

            #| GIVEYOU!! ID \n
            if self.current_token.type == ID:
                # Check if variable is existing in the symbols table before using
                var = self._symbols.lookup(self.current_token.value)
                self.eat(ID)
                if var is None:
                    self.error(ErrorCode.VARIABLE_IS_NOT_DECLARED)
                else:
                    result = var[1]

            #| GIVEYOU!! [ STRING ] \n
            elif self.current_token.type == LSBRACKET:
                self.eat(LSBRACKET)
                if self.current_token.type != RSBRACKET:
                    result = self.current_token.value
                    self.eat(STRING)
                self.eat(RSBRACKET)

            #| GIVEYOU!! <expr>  \n
            else:
                result = self.expr()
            result = "{0}\n".format(str(result))

        print(result,end='',flush=True)

    def expr(self):
        """
        <factor>
           | PLUS <expr> <expr>
           | MINUS <expr> <expr>
           | TIMES <expr> <expr>
           | DIVBY <expr> <expr>
           | MODU <expr> <expr>
            | RAISE <expr> <factor>
            | ROOT <factor> <expr>
            | MEAN <expr> (<expr> <expr>)*
            | DIST <expr><expr> AND <expr><expr>
        """
        if self.current_token.type in (PLUS, MINUS, DIVBY, TIMES, MODU, RAISE, ROOT, MEAN, DIST, NUMBER, ID, POSITIVE, NEGATIVE):
            stack = self.create_stack()
        else:
            self.error(ErrorCode.INVALID_SYNTAX)

        # Scan the stack from the last item to the first item (right to left)
        result = 0
        working_stack = []

        for i in range((len(stack)-1), -1, -1):

            current_stack = stack[i]

            # Get the AND for DIST
            if current_stack == AND:
                working_stack.append(current_stack)
                continue

            # Perform NEGATIVE unary operation
            if current_stack == '-':
                if len(working_stack) >= 1:
                    op1 = working_stack.pop()
                else:
                    self.error(ErrorCode.INVALID_EXPRESSION)

                if self._isint(op1):
                    working_stack.append(-int(op1))
                else:
                    self.error(ErrorCode.INVALID_EXPRESSION)
                continue

            # Perform POSITIVE unary operation
            if current_stack == '+':
                if len(working_stack) >= 1:
                    op1 = working_stack.pop()
                else:
                    self.error(ErrorCode.INVALID_EXPRESSION)

                if self._isint(op1):
                    working_stack.append(int(op1))
                else:
                    self.error(ErrorCode.INVALID_EXPRESSION)


            # Perform operation for PLUS, MINUS, DIVBY, TIMES, MODU, RAISE, ROOT
            if current_stack in (PLUS, MINUS, DIVBY, TIMES, MODU, RAISE, ROOT):
                if len(working_stack) >= 2:
                    op1 = working_stack.pop()
                    op2 = working_stack.pop()
                else:
                    self.error(ErrorCode.INVALID_EXPRESSION)

                if self._isint(op1) and self._isint(op2):
                    op1 = int(op1)
                    op2 = int(op2)
                    # PLUS < expr > < expr >
                    if current_stack == PLUS:
                        result = op1 + op2
                    # MINUS < expr > < expr >
                    elif current_stack == MINUS:
                        result = op1 - op2
                    # TIMES < expr > < expr >
                    elif current_stack == TIMES:
                        result = op1 * op2
                    # DIVBY < expr > < expr >
                    elif current_stack == DIVBY:
                        result = op1 // op2
                    # MODU < expr > < expr >
                    elif current_stack == MODU:
                        result = op1 % op2
                    # RAISE < expr > < factor >
                    elif current_stack == RAISE:
                        result = op1 ** op2
                    # ROOT < factor > < expr >
                    elif current_stack == ROOT:
                        result = int(op2 ** (1/op1))

                    working_stack.append(int(result))
                    continue
                else:
                    self.error(ErrorCode.INVALID_EXPRESSION)

            # Perform operation for DIST
            # DIST <expr><expr> AND <expr><expr>
            elif current_stack == DIST:

                # Check if DIST has 4 parameters
                if len(working_stack) >= 4:
                    op1 = working_stack.pop()
                    op2 = working_stack.pop()
                    op0 = working_stack.pop()
                    op3 = working_stack.pop()
                    op4 = working_stack.pop()
                else:
                    self.error(ErrorCode.INVALID_EXPRESSION)

                if self._isint(op1) and self._isint(op2) and self._isint(op3) and self._isint(op4) and op0 == AND:
                    op1 = int(op1)
                    op2 = int(op2)
                    op3 = int(op3)
                    op4 = int(op4)
                    result = int(math.sqrt(((op1 - op3) ** 2) + ((op2 - op4) ** 2)))
                    working_stack.append(int(result))
                    continue

            # Perform operation for MEAN
            # MEAN < expr > (< expr > < expr >) *
            elif current_stack == MEAN:
                if len(working_stack) > 0:

                    # Get the MEAN parameters
                    mean_stack = []
                    while len(working_stack) > 0:
                        op1 = working_stack.pop()
                        if self._isint(op1):
                            mean_stack.append(int(op1))
                        else:
                            working_stack.append(op1)
                            break

                    result = sum(mean_stack) // len(mean_stack)
                    working_stack.append(int(result))
                    continue

            # Get the operands
            if self._isint(current_stack):
                working_stack.append(int(current_stack))
                continue


        result = working_stack.pop()

        return result

    def create_stack(self):
        # The stack must either have the actual tokens to be used for operation.
        # The variables must be converted to a number.
        # Scan all the tokes until EOS and push it into the stack.
        # Check for invalid expression tokens

        stack = []
        while self.current_token.type != EOS and self.current_token.type != IN:
            stack_item = ''
            if self.current_token.type in (PLUS, MINUS, DIVBY, TIMES, MODU, RAISE, ROOT, MEAN, DIST, AND, NUMBER, ID, POSITIVE, NEGATIVE):

                if self.current_token.type == ID:
                    # Find the value of the ID
                    var = self._symbols.lookup(self.current_token.value)

                    # Check if ID exists
                    if var is not None:
                        stack_item = var[1]

                        # Check if value is number
                        if self._isint(stack_item):
                            self.eat(ID)
                            stack.append(stack_item)
                        else:
                            self.error(ErrorCode.INCOMPATIBLE_DATA_TYPE)

                    else:
                        self.error(ErrorCode.VARIABLE_IS_NOT_DECLARED)
                # if not ID or the rest of the valid tokens
                else:
                    stack_item = self.current_token.value
                    self.eat(self.current_token.type)
                    stack.append(stack_item)
            else:
                self.error(ErrorCode.INVALID_SYNTAX)

        return stack

    def _isint(self, s):
        """
        Checks if value is integer
        """
        try:
            int(s)
            return True
        except ValueError:
            return False

def main():
    try:
        print('========  INTERPOL INTERPRETER STARTED   ========\n')
        filename = input('Enter INTERPOL file (.ipol): ')
        #filename = "a.ipol"
        print('\n================ INTERPOL OUTPUT ================')
        if filename.endswith('.ipol'):
            import os.path
            if os.path.isfile(filename):
                text = open(filename, 'r').read()
                if len(text) > 0:

                    print("\n----------------  OUTPUT START  ---------------->")

                    # Interpreter code
                    lexer = Lexer(text)
                    symbols = SymbolTable()
                    tokens = TokensTable()
                    interpreter = Interpreter(lexer, symbols, tokens)
                    interpreter.parse()

                    print("\n<----------------- OUTPUT END -------------------")

                    print("\n========= INTERPOL LEXEMES/TOKENS TABLE =========")
                    # Print tokens table
                    print("\n{0}".format(tabulate(tokens.get_list(), headers=['LINE NO.', 'TOKENS', 'LEXEMES'])))

                    print("\n================= SYMBOLS TABLE =================")
                    # Print symbols table
                    print("\n{0}".format(tabulate(symbols.get_list(), headers=['VARIABLE NAME', 'TYPE', 'VALUE'])))

                else:
                    print('File is empty')
            else:
                print('File does not exist')
        else:
            print('Input file does not end with .ipol')
        print('\n======== INTERPOL INTERPRETER TERMINATED ========')
    except Error as e:
        # Customized error message
        print("\n{0}".format(e.message))
    except:
        print("\n General error encountered.")


if __name__ == '__main__':
    main()