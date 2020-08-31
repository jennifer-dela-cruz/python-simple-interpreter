from enum import Enum
import math

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
LSBRACKET = 'LSBRACKET'
RSBRACKET = 'RSBRACKET'
NUMBER   = 'NUMBER' # Integer value
STRING   = 'STRING' # String literal
VAR      = 'IDENTIFIER' # Variable name
EOS      = 'EOS' # End of statement
EOF      = 'END_OF_FILE' # End of file no more input
ERR      = 'ERROR' # Error token not found


# Error codes and description
class ErrorCode(Enum):
    UNEXPECTED_TOKEN                = 'Unexpected token'
    ID_NOT_FOUND                    = 'Identifier not found'
    DUPLICATE_ID                    = 'Duplicate id found'
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
        self.message = f'{self.__class__.__name__}: {message}'


class Token(object):
    def __init__(self, type, name, value):
        # JHD Fix this later so that it will also get the description aside from the name
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
        raise Error(
            error_code=error_code,
            lineno=self.lineno,
            token_list=self.previous_char,
            message=f'{error_code.value} at line [ {self.lineno} ] ----> {self.previous_char}'
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
        peek_pos = self.pos + 1
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace() and self.current_char != '\n':
            self.advance()

    def skip_comment(self):
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

        while self.current_char is not None and self.current_char.isalnum():
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
            token = RESERVED_KEYWORDS.get(result, Token(ID, ID, result))

        return token

    def _string(self):
        """Handle strings"""
        result = ''

        while self.current_char is not None and self.current_char.isascii() and self.current_char != ']':
            result += self.current_char
            self.advance()

        return Token(STRING, STRING, result)

    def get_next_token(self):
        """Lexical analyzer (also known as scanner or tokenizer)
        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        while self.current_char is not None:

            if self.current_char == '\n':
                self.advance()
                return Token(EOS, EOS, '\n')

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

            if self.current_char.isalnum():
                return self._id()

            if self.current_char == '[':
                self.advance()
                return Token(LSBRACKET, LSBRACKET, '[')

            if self.current_char == ']':
                self.advance()
                return Token(RSBRACKET, RSBRACKET, ']')

            self.error(ErrorCode.INVALID_CHARACTER)

        return Token(EOF, EOF, None)


class SymbolTable(object):
    def __init__(self):
        self._symbols = {}

    def __str__(self):
        #JHD update this later
        s = 'Symbols: VARIABLE NAME    TYPE     VALUE \n'

        # JHD Fix this later. Must be in table format and correct sequence
        for key, value in self._symbols.items():
           s += "{0} {1} {2}\n".format(key, value[0], value[1])

        return s

    __repr__ = __str__

    def define(self, name, type, value):
        self._symbols[name] = [type, value]

    def lookup(self, name):
        result = self._symbols.get(name)

        return result

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
        if lexeme == '\n':
            lexeme = ''
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
        token_list = self.tokens.lookup(self.lexer.lineno)
        if not token_list:
            token_list = self.lexer.previous_char

        raise Error(
            error_code=error_code,
            lineno=self.lexer.lineno,
            token_list=token_list,
            message=f'{error_code.value} at line [ {self.lexer.lineno} ] ----> {token_list}',
        )


    def eat(self, token_type):
        # compare the current token type with the passed token
        # type and if they match then "eat" the current token
        # and assign the next token to the self.current_token,
        # otherwise raise an exception.
        if self.current_token.type == token_type:
            # JHD Change the line number here later
            self.tokens.define(self.lexer.lineno, self.current_token.name, self.current_token.value)
            self.current_token = self.lexer.get_next_token()
        else:
            self.error(ErrorCode.INVALID_SYNTAX)

    # JHD This is the first checking of the token based on the grammar rules
    def parse(self):
        """ EDIT THIS
        program : compound_statement DOT
        compound_statement : BEGIN statement_list END
        statement_list : statement
                       | statement SEMI statement_list
        statement : compound_statement
                  | assignment_statement
                  | empty
        assignment_statement : variable ASSIGN expr
        empty :
        expr: term ((PLUS | MINUS) term)*
        term: factor ((MUL | DIV) factor)*
        factor : PLUS factor
               | MINUS factor
               | INTEGER
               | LPAREN expr RPAREN
               | variable
        variable: ID
        """
        self.program()
        if self.current_token.type != EOS:
            self.error(ErrorCode.INVALID_SYNTAX)


    # JHD This is the 1st grammar rule to check
    def program(self):
        """
        program: BEGIN statement_list END
        """
        self.eat(CREATE)
        self.statement_list()
        self.eat(RUPTURE)

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
                result = ''
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
                self.error(ErrorCode.DUPLICATE_VARIABLE_DECLARATION)
            # Define variables
            self._symbols.define(var_name, NUMBER, result)


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
                    result = str(self.expr())
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
                    result = str(self.expr())
                self.eat(RSBRACKET)

            #| GIVEYOU!! <expr>  \n
            else:
                result = self.expr()
            result = "{0}\n".format(str(result))

        print(result,end='',flush=True)

    def expr(self):
        """
        <basic>
            | RAISE <expr> <factor>
            | ROOT <factor> <expr>
            | MEAN <expr> (<expr> <expr>)*
            | DIST <expr><expr> AND <expr><expr>
        """

        # JHD Must scan from right to left
        # JHD Check if basic must be inside the while loop
        if self.current_token.type in (RAISE, ROOT, MEAN, DIST):

            # Check how to use while later
            while self.current_token.type in (RAISE, ROOT, MEAN, DIST):
                token = self.current_token

                # RAISE <expression> <exponent>
                if token.type == RAISE:
                    self.eat(RAISE)
                    op1 = self.expr()
                    op2 = self.factor()
                    result = op1 ** op2

                # ROOT <N> <expression>
                elif token.type == ROOT:
                    self.eat(ROOT)
                    op1 = self.factor()
                    op2 = self.expr()
                    result = op2 ** (1//op1)

                # DIST < expr1 > < expr2 > AND < expr3 > < expr4 >
                elif token.type == DIST:
                    self.eat(DIST)
                    op1 = self.expr()
                    op2 = self.expr()
                    self.eat(AND)
                    op3 = self.expr()
                    op4 = self.expr()
                    result = int(math.sqrt(((op1-op3)**2)+((op2-op4)**2)))

                # MEAN <expr1> <expr2> <expr3> … <exprn>
                elif token.type == MEAN:
                    self.eat(MEAN)
                    # WHAT TO DO TO SCAN THE NEXT ITEMS?
                    # self.expr()
                    op_list = []
                    result = sum(op_list)//len(op_list)

        else:
            result = self.basic()

        return result

    def basic(self):
        """
        <term> | ((PLUS | MINUS) <expr> <expr>)*
        """
        # JHD Must scan from right to left

        if self.current_token.type in (PLUS, MINUS):

            while self.current_token.type in (PLUS, MINUS):
                token = self.current_token

                # PLUS <expression1> <expression2>
                if token.type == PLUS:
                    self.eat(PLUS)
                    op1 = self.expr()
                    op2 = self.expr()
                    result = op1 + op2

                # MINUS < expression1 > < expression2 >
                elif token.type == MINUS:
                    self.eat(MINUS)
                    op1 = self.expr()
                    op2 = self.expr()
                    result = op1 - op2
        else:
            result = self.term()

        return result

    def term(self):
        """
        <factor> | ((MUL | DIV | MODU) <factor><factor>)*
        """
        result = ''

        while self.current_token.type in (TIMES, DIVBY, MODU):
            token = self.current_token

            # TIMES <expression1> <expression2>
            if token.type == TIMES:
                self.eat(TIMES)
                op1 = self.expr()
                op2 = self.expr()
                result = op1 * op2

            # DIVBY <expression1> <expression2>
            elif token.type == DIVBY:
                self.eat(DIVBY)
                op1 = self.expr()
                op2 = self.expr()
                result = op1 // op2

            # MODU <expression1> <expression2>
            elif token.type == MODU:
                self.eat(MODU)
                op1 = self.expr()
                op2 = self.expr()
                result = op1 % op2

        if result == '':
            result = self.factor()

        return result

    def factor(self):
        """factor : NUMBER"""
        token = self.current_token
        if token.type == STRING:
            self.eat(STRING)
        elif token.type == NUMBER:
            self.eat(NUMBER)
        else:
            self.error(ErrorCode.INVALID_SYNTAX)

        return token.value

def main():
    try:
        print('========  INTERPOL INTERPRETER STARTED   ========\n')
        #filename = input('Enter INTERPOL file (.ipol): ')
        filename = "a.ipol"
        print('>>> Reading file.')
        if filename.endswith('.ipol'):
            import os.path
            if os.path.isfile(filename):
                text = open(filename, 'r').read()
                if len(text) > 0:

                    # Add the interpreter code here
                    lexer = Lexer(text)
                    symbols = SymbolTable()
                    tokens = TokensTable()
                    interpreter = Interpreter(lexer, symbols, tokens)
                    interpreter.parse()
                    print("\n{0}".format(tokens))
                    print("{0}".format(symbols))

                else:
                    #print('File is empty.')
                    raise Error(error_code=ErrorCode.FILE_IS_EMPTY)

            else:
                #print('File not found.')
                raise Error(error_code=ErrorCode.FILE_NOT_FOUND)
        else:
            #print('Invalid file.')
            raise Error(error_code=ErrorCode.INVALID_FILE)
        print('\n======== INTERPOL INTERPRETER TERMINATED ========')
    except Error as e:
        print(e.message)
        #print("An error occurred. Please try again.")

if __name__ == '__main__':
    main()