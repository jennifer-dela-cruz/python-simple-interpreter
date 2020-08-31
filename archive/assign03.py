#Types of Tokens
START    = 'PROGRAM_CREATE'
END      = 'PROGRAM_RUPTURE'
DECINT   = 'DECLARATION_INT'
DECSTR   = 'DECLARATION_STRING'
WITH     = 'DECLARATION_ASSIGN_WITH_KEY'
STORE    = 'ASSIGN_KEY'
IN       = 'ASSIGN_VAR_KEY'
READ     = 'INPUT'
WRITE    = 'OUTPUT'
WRITELN  = 'OUTPUT_WITH_LINE'
ADD      = 'BASIC_OPERATOR_ADD'
SUB      = 'BASIC_OPERATOR_SUB'
MUL      = 'BASIC_OPERATOR_MUL'
DIV      = 'BASIC_OPERATOR_DIV'
MOD      = 'BASIC_OPERATOR_MOD'
EXP      = 'ADVANCED_OPERATOR_EXP'
ROOT     = 'ADVANCED_OPERATOR_ROOT'
AVE      = 'ADVANCED_OPERATOR_AVE'
DIST     = 'ADVANCED_OPERATOR_DIST'
DISTSEP  = 'DISTANCE_SEPARATOR'
NUMBER   = 'NUMBER' # Integer value
STRING   = 'STRING' # String literal
VAR      = 'IDENTIFIER' # Variable name
EOS      = 'END_OF_STATEMENT' # End of statement
EOF      = 'END_OF_FILE' # End of file no more input
ERR      = 'ERROR' # Error token not found 

#Keywords
KEYWORDS = {
    'CREATE':       START,
    'RUPTURE':      END,
    'DSTR':         DECSTR,
    'DINT':         DECINT,
    'GIVEME?':      READ,
    'GIVEYOU!':     WRITE,
    'GIVEYOU!!':    WRITELN,
    'WITH':         WITH,
    'STORE':        STORE,
    'IN':           IN,
    'PLUS':         ADD,
    'MINUS':        SUB,
    'TIMES':        MUL,
    'DIVBY':        DIV,
    'MODU':         MOD,
    'RAISE':        EXP,
    'ROOT':         ROOT,
    'MEAN':         AVE,
    'DIST':         DIST,
    'AND':          DISTSEP
}

def main():
    print('========  INTERPOL INTERPRETER STARTED   ========\n')
    filename = input('Enter INTERPOL file (.ipol): ')
    print('>>> Reading file.')
    if filename.endswith('.ipol'):
        import os.path
        if os.path.isfile(filename):
            text = open(filename, 'r').read()
            if len(text) > 0:
                #Add the interpreter code here

            else:
                print('File is empty.')
        else:
            print('File not found.')
    else:
        print('Invalid file.')
    print('\n======== INTERPOL INTERPRETER TERMINATED ========')

if __name__ == '__main__':
    main()