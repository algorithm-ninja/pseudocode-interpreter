export function tokens_provider() {
    return {
        // Set defaultToken to invalid to see what you do not tokenize yet
        // defaultToken: 'invalid',

        keywords: [
            'variable', 'function', 'type',
            'while', 'for', 'if', 'else', 'end', 'return', 'do', 'in', 'then'
        ],

        builtinNames: [
            'output', 'min', 'max', 'repeat', 'has_string', 'next_string', 'has_int', 'next_int', 'to_string',
            'true', 'false'
        ],

        typeKeywords: [
            'integer', 'float', 'bool', 'string'
        ],

        operators: [
            '<-', '>', '>=', '<', '<=', '==', '!=', 'and', 'or', '+', '-', '*', '/', 'mod', '...', 'not'
        ],

        symbols: /[=><!:+\-*\/]+/,
        escapes: /\\(?:[nt\\"'])/,

        tokenizer: {
            root: [
                // whitespace
                { include: '@whitespace' },

                // delimiters and operators
                [/[{}()\[\]]/, '@brackets'],
                [/[<>](?!@symbols)/, '@brackets'],
                [/@symbols/, { cases: {
                    '@operators': 'operator',
                    '@default'  : '' }
                }],

                // identifiers and keywords
                [/[a-zA-Z_][\w]*/, { cases: {
                    '@typeKeywords': 'type',
                    '@operators': 'operator',
                    '@builtinNames': 'identifier',
                    '@keywords': 'keyword',
                    '@default': 'identifier' }
                }],

                // numbers
                [/\d+\.\d+/, 'number.float'],
                [/\d+/, 'number'],

                // delimiters
                [/[,.]/, 'delimiter'],

                // strings
                [/"([^"\\]|\\.)*$/, 'string.invalid' ],  // non-teminated string
                [/"/,  { token: 'string.quote', bracket: '@open', next: '@string' } ]
            ],

            string: [
                [/[^\\"]+/,  'string'],
                [/@escapes/, 'string.escape'],
                [/\\./,      'string.escape.invalid'],
                [/"/,        { token: 'string.quote', bracket: '@close', next: '@pop' } ]
            ],

            whitespace: [
                [/^[ \t]*\/\/.*$/, 'comment'],
                // [/[ \t\r\n]+/, 'white'],
            ],
        },
    };
}
