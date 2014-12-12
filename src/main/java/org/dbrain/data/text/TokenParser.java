/*
 * Copyright [2014] [Eric Poitras]
 *
 *     Licensed under the Apache License, Version 2.0 (the "License");
 *     you may not use this file except in compliance with the License.
 *     You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dbrain.data.text;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

/**
 * This class allow sequential parsing of text stream to extract tokens.
 *
 * @author Eric Poitras
 * @version 1.0
 * @see Token for more informations on availables tokens.
 */
public class TokenParser implements AutoCloseable {

    public static String ERR_READER_NULL       = "Reader is null.";
    public static String ERR_INVALID_CHARACTER = "Invalid character found.";
    public static String ERR_UNEXPECTED_EOS    = "Unexpected end of stream.";
    public static String ERR_EXPECTED_NUMERIC  = "Expected numeric litteral.";
    public static String ERR_EXPECTED_TOKEN    = "Expected %s, found %s.";
    public static String ERR_WRONG_QUOTE       = "Invalid quoting on the string litteral.";
    public static String ERR_EXPECTING_EOF     = "Expecting end of stream.";

    // Default values for stream format.
    private static char DOUBLE_QUOTE       = '"';
    private static char SINGLE_QUOTE       = '\'';
    private static char LONG_QUOTE         = '\\';
    private static char SINGLE_CHAR_STRING = '$';


    private Reader reader;
    private boolean parsed = false;
    private char cur;
    private boolean eof = false;
    private Token  curToken;
    private Object curValue;


    /**
     * Create a new Parser.
     *
     * @param aReader The source of the text stream.
     * @since 1.0
     */
    public TokenParser( Reader aReader ) {
        super();
        if ( aReader == null ) throw new IllegalArgumentException( ERR_READER_NULL );
        reader = aReader;
    }

    /**
     * Create a new Parser having a string as it's source.
     *
     * @param s The source of the text stream.
     */
    public TokenParser( String s ) {
        this( new StringReader( s ) );
    }

    /**
     * Throw a ParseException.
     *
     * @param msg The message to include in the error.
     * @since 1.0
     */
    public void raiseError( String msg ) {
        throw new ParseException( msg );
    }

    public void raiseExpectedTokenError( Token foundToken, Token... expectedTokens ) {
        StringBuilder sb = new StringBuilder();
        for ( Token t : expectedTokens ) {
            if ( sb.length() > 0 ) sb.append( " - " );
            sb.append( t.toString() );
        }
        raiseError( String.format( ERR_EXPECTED_TOKEN, sb.toString(), foundToken.toString() ) );
    }


    /**
     * Get the binary getValue of a single digit.<p>
     *
     * @param c The digit to evaluate.
     * @return The getValue of the digit from 0 to 9 or -1 if this is not a valid digit.
     * @since 1.0
     */
    private static int getBinDigitValue( char c ) {
        return ( c >= '0' && c <= '1' ) ? (int) c - (int) '0' : -1;
    }

    /**
     * Get the decimal getValue of a single digit.<p>
     *
     * @param c The digit to evaluate.
     * @return The getValue of the digit from 0 to 9 or -1 if this is not a valid digit.
     * @since 1.0
     */
    private static int getDigitValue( char c ) {
        return ( c >= '0' && c <= '9' ) ? (int) c - (int) '0' : -1;
    }

    /**
     * Verify if a character is a digit.
     *
     * @param c The character to verify.
     * @return True if c is a digit.
     * @since 1.0
     */
    private static boolean isDigit( char c ) {
        return ( c >= '0' && c <= '9' );
    }

    /**
     * Get the hexadecimal getValue of a single digit. Both upper and lower case are accepted.<p>
     *
     * @param c The digit to evaluate.
     * @return The getValue of the digit from 0 to 15 or -1 if this is not a valid digit.
     * @since 1.0
     */
    private static int getHexDigitValue( char c ) {
        int result;
        if ( c >= '0' && c <= '9' ) {
            result = (int) c - (int) '0';
        } else if ( c >= 'a' && c <= 'f' ) {
            result = (int) c - (int) 'a' + 10;
        } else if ( c >= 'A' && c <= 'F' ) {
            result = (int) c - (int) 'A' + 10;
        } else result = -1;

        return result;
    }

    /**
     * Verify if a character is a valid starting character for an unquoted string.
     *
     * @param c The character to verify.
     * @return True if c is a starting identifier character.
     * @since 1.0
     */
    private static boolean isUnquotedStringStart( char c ) {

        // Fast lane for latin standard
        return ( c >= 'a' && c <= 'z' ) || ( c >= 'A' && c <= 'Z' ) || ( c == '_' ) || Character.isLetter( c );

    }


    /**
     * Verify if a character is a valid character for an unquoted string.
     *
     * @param c The character to verify.
     * @return True if c is a valid identifier character.
     * @since 1.0
     */
    private static boolean isUnquotedStringPart( char c ) {
        return ( isUnquotedStringStart( c ) || isDigit( c ) );
    }

    /**
     * Read the next character on the stream. Set mEof to true if no character is fetched.
     *
     * @since 1.0
     */
    private void readNextChar() {
        if ( !eof ) try {

            int i = reader.read();
            eof = ( i < 0 );
            cur = eof ? (char) 0 : (char) i;

        } catch ( IOException e ) {
            raiseError( e.getMessage() );
        }
    }

    /**
     * Read the next character on the stream. Throw an exception if no character is fetched.
     *
     * @since 1.0
     */
    private void readNextCharMandatory() {
        readNextChar();
        if ( eof ) raiseError( ERR_UNEXPECTED_EOS );
    }

    /**
     * @return A long containing the decoded binary getValue
     * @since 1.0
     */
    private long parseBinary() {
        long result = 0;

        int value = getBinDigitValue( cur );
        if ( value < 0 ) raiseError( ERR_EXPECTED_NUMERIC );
        for ( boolean ok = true; ok; ) {
            if ( value >= 0 ) {
                result = result * 2 + value;
                readNextChar();
            } else ok = false;
            value = getBinDigitValue( cur );
        }
        return result;
    }

    /**
     * @return A long containing the decoded decimal getValue. Does not handle signs.
     * @since 1.0
     */
    private long parseDecimal() {
        long result = 0;

        int value = getDigitValue( cur );
        if ( value < 0 ) raiseError( ERR_EXPECTED_NUMERIC );
        for ( boolean ok = true; ok; ) {
            if ( value >= 0 ) {
                result = result * 10 + value;
                readNextChar();
            } else ok = false;
            value = getDigitValue( cur );
        }
        return result;
    }

    /**
     * @return A long containing the decoded hexadecimal getValue. Does not handle signs.
     */
    private long parseHexadecimal() {
        long result = 0;
        int value = getHexDigitValue( cur );
        if ( value < 0 ) raiseError( ERR_EXPECTED_NUMERIC );
        for ( boolean ok = true; ok; ) {
            if ( value >= 0 ) {
                result = result * 16 + value;
                readNextChar();
            } else ok = false;
            value = getHexDigitValue( cur );
        }
        return result;
    }

    /**
     * @return A Number containing the decoded numeric getValue.
     */
    private long parseSignedDecimal() {
        boolean negative = false;

        if ( cur == '-' ) {
            negative = true;
            readNextChar();
        }
        return negative ? -parseDecimal() : parseDecimal();
    }

    /**
     * @param scale The number is multiplied by scale before being returned.
     * @return A Number containing the decoded numeric getValue.
     */
    private Number parseNumeric( long scale ) {
        long integralPart = 0;
        double doublePart = 0.0;
        long exponent = 0;
        boolean checkDouble = false;
        boolean isDouble = false;


        // Verify the first integral part
        if ( cur == '0' ) {
            readNextChar();
            switch ( cur ) {
                case 'x':
                case 'X':
                    readNextChar();
                    integralPart = parseHexadecimal();
                    break;
                case 'b':
                case 'B':
                    readNextChar();
                    integralPart = parseBinary();
                    break;
                case 'd':
                case 'D':
                    readNextChar();
                    integralPart = parseDecimal();
                    break;
                default: {
                    integralPart = isDigit( cur ) ? parseDecimal() : 0;
                    checkDouble = true;
                }
            }
        } else {
            integralPart = cur == '.' ? 0 : parseDecimal();
            checkDouble = true;
        }

        // Could be a double getValue ?
        if ( checkDouble ) {

            if ( cur == '.' ) {
                isDouble = true;
                double factor = .1;

                readNextChar();
                for ( boolean ok = true; ok; ) {
                    int digit = getDigitValue( cur );
                    if ( digit >= 0 ) {
                        doublePart = doublePart + digit * factor;
                        factor = factor / 10;
                        readNextChar();
                    } else ok = false;

                }
            }

            if ( cur == 'E' || cur == 'e' ) {
                isDouble = true;
                readNextChar();
                exponent = parseSignedDecimal();
            }
        }

        if ( isDouble ) {
            return scale * ( integralPart + doublePart ) * Math.pow( 10, exponent );
        } else {
            return scale * integralPart;
        }
    }

    /**
     * Parse an unquoted string. Stop when the character sequence is not
     * suitable to fit in an unquoted string.
     */
    private void parseUnquotedString( StringBuilder sb ) {
        while ( isUnquotedStringPart( cur ) ) {
            sb.append( cur );
            readNextChar();
        }
    }

    /**
     * Parse a string delimited by quoting character. The function must be called
     * with cur pointing to the current quote to use. Two sequantial instance of the quote
     * result in including the quote in the string.
     *
     * @param forbidControlCharacter If true, control characters inside the string will throw
     *                               an error.
     * @return The string without the quotes.
     * @since 1.0
     */
    private void parseQuotedString( StringBuilder sb, boolean forbidControlCharacter ) {
        boolean ok = true;

        char quote = cur;
        readNextCharMandatory();

        while ( ok ) {
            if ( cur == quote ) {
                readNextChar();
                ok = ( cur == quote );
                if ( ok ) {
                    sb.append( quote );
                    readNextCharMandatory();
                }
            } else {
                if ( forbidControlCharacter ) if ( cur < ' ' ) raiseError( ERR_INVALID_CHARACTER );
                sb.append( cur );
                readNextCharMandatory();
            }
        }
    }

    /**
     * Parse a one-character constant having the syntax : $&lt;ascii-code&gt; <p>
     * Examples : <p>
     * <ul>
     * <li>$13 : return.</li>
     * <li>$10 : line feed.</li>
     * </ul>
     *
     * @return the decoded character.
     * @since 1.0
     */
    private char parseSingleCharString() {
        if ( cur != SINGLE_CHAR_STRING ) raiseError( ERR_INVALID_CHARACTER );
        readNextChar();
        if ( !isDigit( cur ) ) raiseError( ERR_INVALID_CHARACTER );
        return (char) parseNumeric( 1 ).byteValue();

    }


    /**
     * Concatenate a sequence of string defined in different format.<P>
     * "first line"$13$10"second line"$13$10
     *
     * @return a string.
     * @since 1.0
     */
    private String parseMultiPartString() {
        StringBuilder result = new StringBuilder( 256 );
        for ( boolean ok = true; ok; ) {
            if ( isUnquotedStringStart( cur ) ) {
                parseUnquotedString( result );
            } else if ( cur == DOUBLE_QUOTE || cur == SINGLE_QUOTE ) {
                parseQuotedString( result, true );
            } else if ( cur == LONG_QUOTE ) {
                parseQuotedString( result, false );
            } else if ( cur == SINGLE_CHAR_STRING ) {
                result.append( parseSingleCharString() );
            } else ok = false;
        }
        return result.toString();
    }

    /**
     * Internally used to set current token getValue after if have been read from
     * the stream.
     *
     * @param aToken
     * @param aValue
     */
    private void setToken( Token aToken, Object aValue ) {
        curToken = aToken;
        curValue = aValue;
        parsed = true;
    }

    /**
     * Internally use to set a single character operator token.
     */
    private void setOpToken( Token aToken ) {
        readNextChar();
        setToken( aToken, null );
    }

    /**
     * Internally use to set a single or a double character operator token.
     */
    private void setOpToken( Token aToken, char op1, Token aToken1 ) {
        readNextChar();
        if ( cur == op1 ) {
            setToken( aToken1, null );
            readNextChar();
        } else setToken( aToken, null );
    }

    /**
     * Internally use to set a single, a double or a tripple character operator token.
     */
    private void setOpToken( Token aToken, char op1, Token aToken1, char op2, Token aToken2 ) {
        readNextChar();
        if ( cur == op1 ) {
            setToken( aToken1, null );
            readNextChar();
        } else if ( cur == op2 ) {
            setToken( aToken2, null );
            readNextChar();
        } else setToken( aToken, null );
    }

    /**
     * Internally used to skip line single comments.
     */
    private void skipSingleLineComment() {
        readNextChar();
        for ( boolean ok = true; ok; ) {
            ok = !( cur == '\r' || eof );
            readNextChar();
        }
    }

    /**
     * Internally used to skip multi-line comments.
     */
    private void skipMultiLineComment() {
        readNextCharMandatory();
        for ( boolean ok = true; ok; ) {
            if ( cur == '*' ) {
                readNextCharMandatory();
                ok = !( cur == '/' );
            }
            readNextCharMandatory();
        }
    }


    /**
     * Get the available token in the stream. This method does *NOT* consume the
     * token.
     *
     * @return the token.
     */
    public Token getToken() {
        while ( !parsed ) {
            if ( isDigit( cur ) ) {
                setToken( Token.NUMERIC, parseNumeric( 1 ) );
            } else if ( isUnquotedStringStart( cur ) ) {
                String identifier = parseMultiPartString();
                setToken( Token.KEYWORD, identifier );
            } else if ( cur == DOUBLE_QUOTE ) {
                setToken( Token.STRING, parseMultiPartString() );
            } else if ( cur == SINGLE_QUOTE ) {
                setToken( Token.IDENTIFIER, parseMultiPartString() );
            } else if ( cur == LONG_QUOTE ) {
                setToken( Token.STRING, parseMultiPartString() );
            } else if ( cur == SINGLE_CHAR_STRING ) {
                setToken( Token.STRING, parseMultiPartString() );
            } else switch ( cur ) {
                case '(':
                    setOpToken( Token.OPEN_PARENTESIS );
                    break;
                case '[':
                    setOpToken( Token.OPEN_BRACKET );
                    break;
                case '{':
                    setOpToken( Token.OPEN_ALINEAS );
                    break;
                case ')':
                    setOpToken( Token.CLOSE_PARENTESIS );
                    break;
                case ']':
                    setOpToken( Token.CLOSE_BRACKET );
                    break;
                case '}':
                    setOpToken( Token.CLOSE_ALINEAS );
                    break;
                case '+':
                    setOpToken( Token.PLUS, '+', Token.INCREMENT, '=', Token.PLUS_EQUAL );
                    break;
                case '-':
                    setOpToken( Token.MINUS, '-', Token.DECREMENT, '=', Token.MINUS_EQUAL );
                    break;
                case '*':
                    setOpToken( Token.MULTIPLY, '=', Token.MULTIPLY_EQUAL );
                    break;
                case '%':
                    setOpToken( Token.MODULUS, '=', Token.MODULUS_EQUAL );
                    break;
                case '<':
                    setOpToken( Token.LESSER, '=', Token.LESSER_EQUAL );
                    break;
                case '>':
                    setOpToken( Token.GREATER, '=', Token.GREATER_EQUAL );
                    break;
                case ':':
                    setOpToken( Token.COLON, '=', Token.COLON_EQUAL );
                    break;
                case '=':
                    setOpToken( Token.EQUAL );
                    break;
                case '@':
                    setOpToken( Token.AT );
                    break;
                case '&':
                    setOpToken( Token.BW_AND, '&', Token.AND );
                    break;
                case '|':
                    setOpToken( Token.BW_OR, '|', Token.OR );
                    break;
                case '^':
                    setOpToken( Token.BW_XOR );
                    break;
                case '~':
                    setOpToken( Token.BW_NOT );
                    break;
                case '!':
                    setOpToken( Token.NOT );
                    break;
                case '.':
                    setOpToken( Token.DOT );
                    break;
                case ',':
                    setOpToken( Token.SEP );
                    break;
                case ';':
                    setOpToken( Token.EOS );
                    break;
                case '/':

                    readNextChar();
                    switch ( cur ) {
                        case '/':
                            skipSingleLineComment();
                            break;        // Single line comment : skip until EOL
                        case '*':
                            skipMultiLineComment();
                            break;         // Multiline comment : Skip until end of comment is reached.
                        case '=':
                            setOpToken( Token.DIVIDE_EQUAL );
                            break; // Divide_Equal
                        default:
                            setToken( Token.DIVIDE, null );
                            break;   // Really was a simple divide
                    }
                    break;

                default:
                    if ( cur <= ' ' ) readNextChar();
                    else raiseError( ERR_INVALID_CHARACTER );
                    break;
            }

            if ( !parsed && eof ) setToken( Token.EOF, null );
        }

        return curToken;
    }

    public boolean isToken( Token validToken1 ) {
        Token retval = getToken();
        return retval == validToken1;
    }

    public boolean isToken( Token validToken1, Token validToken2 ) {
        Token retval = getToken();
        return ( ( retval == validToken1 ) || ( retval == validToken2 ) );
    }

    public boolean isToken( Token validToken1, Token validToken2, Token validToken3 ) {
        Token retval = getToken();
        return ( ( retval == validToken1 ) || ( retval == validToken2 ) || ( retval == validToken3 ) );
    }

    public Token getValidToken( Token expectedToken1 ) {
        if ( !isToken( expectedToken1 ) ) raiseExpectedTokenError( getToken(), expectedToken1 );
        return getToken();
    }

    public Token getValidToken( Token expectedToken1, Token expectedToken2 ) {
        if ( !isToken( expectedToken1, expectedToken2 ) ) raiseExpectedTokenError( getToken(), expectedToken1 );
        return getToken();
    }

    public Token getValidToken( Token expectedToken1, Token expectedToken2, Token expectedToken3 ) {
        if ( !isToken( expectedToken1, expectedToken2, expectedToken3 ) )
            raiseExpectedTokenError( getToken(), expectedToken1 );
        return getToken();
    }


    /**
     * Read the numeric getValue from the parser.
     *
     * @return the current numeric getValue.
     * @throws ParseException if current token is not numeric.
     */
    public Number getNumeric() {
        return (Number) get( Token.NUMERIC );
    }

    /**
     * Read the numeric getValue from the parser.<p>
     * Move the parser on the next token.<p>
     *
     * @return the current numeric getValue.
     * @throws ParseException if current token is not numeric.
     */
    public Number readNumeric() {
        Number result = getNumeric();
        consume();
        return result;
    }

    /**
     * Read the keyword from the parser.<p>
     *
     * @return the current keyword.
     * @throws ParseException if current token is not a keyword.
     */
    public String getKeyword() {
        return (String) get( Token.KEYWORD );
    }

    /**
     * Read the keyword from the parser.<p>
     * Move the parser on the next token.<p>
     *
     * @return the current string getValue.
     * @throws ParseException if current token is not a keyword.
     */
    public String readKeyword() {
        String result = getKeyword();
        consume();
        return result;
    }


    /**
     * Read the identifier getValue from the parser.<p>
     *
     * @return the current identifier getValue.
     * @throws ParseException if current token is not string.
     */
    public String getIdentifier() {
        return (String) get( Token.IDENTIFIER );
    }

    /**
     * Read the identifier getValue from the parser.<p>
     * Move the parser on the next token.<p>
     *
     * @return the current string getValue.
     * @throws ParseException if current token is not string.
     */
    public String readIdentifier() {
        String result = getIdentifier();
        consume();
        return result;
    }

    /**
     * Read the string getValue from the parser.<p>
     * Does not care about the quoting of the string.<p>
     *
     * @return the current string getValue.
     * @throws ParseException if current token is not string.
     */
    public String getString() {
        return (String) get( Token.STRING );
    }

    /**
     * Read the string getValue from the parser.<p>
     * Does not care about the quoting of the string.<p>
     * Move the parser on the next token.<p>
     *
     * @return the current string getValue.
     * @throws ParseException if current token is not string.
     */
    public String readString() {
        String result = getString();
        consume();
        return result;
    }

    public Object get( Token expectedToken1 ) {
        getValidToken( expectedToken1 );
        return curValue;
    }

    public Object get( Token expectedToken1, Token expectedToken2 ) {
        getValidToken( expectedToken1, expectedToken2 );
        return curValue;
    }

    public Object get( Token expectedToken1, Token expectedToken2, Token expectedToken3 ) {
        getValidToken( expectedToken1, expectedToken2, expectedToken3 );
        return curValue;
    }

    public Object read( Token expectedToken1 ) {
        Object result = get( expectedToken1 );
        consume();
        return result;
    }

    public Object read( Token expectedToken1, Token expectedToken2 ) {
        Object result = get( expectedToken1, expectedToken2 );
        consume();
        return result;
    }

    public Object read( Token expectedToken1, Token expectedToken2, Token expectedToken3 ) {
        Object result = get( expectedToken1, expectedToken2, expectedToken3 );
        consume();
        return result;
    }


    /**
     * Used to invalidate current token getValue and set the parsed
     * flag to false, firing the parsing of a new token from the stream.
     */
    public boolean consume() {
        if ( getToken() == Token.EOF ) return false;

        curToken = null;
        curValue = null;
        parsed = false;

        return true;
    }


    /**
     * Close the underlying stream.
     */
    public void close() {
        try {
            reader.close();
        } catch ( IOException e ) {
            raiseError( e.getMessage() );
        }
    }

}