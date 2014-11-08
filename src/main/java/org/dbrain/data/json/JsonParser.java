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

package org.dbrain.data.json;


import org.dbrain.data.text.ReaderCursor;
import org.dbrain.data.text.ParseException;

import java.io.Reader;

/**
 * Created by epoitras on 27/06/14.
 */
public class JsonParser {

    private ReaderCursor cursor;
    private boolean parsed = false;
    private Token  token;
    private Object value;

    public JsonParser( Reader r ) {
        cursor = new ReaderCursor( r );
    }

    public ParseException error( String message ) {
        return cursor.error( message );
    }

    private void skipWhiteSpace() {
        int current = cursor.peek();
        while ( current >= 0 && current <= ' ' || Character.isWhitespace( current ) ) {
            current = cursor.peekNext();
        }
    }

    private void setToken( Token token, Object value ) {
        this.parsed = true;
        this.token = token;
        this.value = value;
    }

    private void setToken( Token token ) {
        setToken( token, null );
    }

    private String parseString() {
        int quote = cursor.peek();
        StringBuilder sb = new StringBuilder();
        for ( int codePoint = cursor.peekNext(); codePoint != quote; codePoint = cursor.peekNext() ) {
            if ( codePoint < 0 || codePoint == '\r' || codePoint == '\n' ) {
                throw cursor.error( "Unterminated string" );
            } else if ( codePoint == '\\' ) {
                codePoint = cursor.peekNext();
                switch ( codePoint ) {
                    case 'b':
                        sb.append( '\b' );
                        break;
                    case 't':
                        sb.append( '\t' );
                        break;
                    case 'n':
                        sb.append( '\n' );
                        break;
                    case 'f':
                        sb.append( '\f' );
                        break;
                    case 'r':
                        sb.append( '\r' );
                        break;
                    case 'u':
                        cursor.read();
                        sb.append( (char) Integer.parseInt( cursor.read( 4 ), 16 ) );
                        break;
                    case '"':
                    case '\'':
                    case '\\':
                    case '/':
                        sb.appendCodePoint( codePoint );
                        break;
                    default:
                        throw cursor.error( "Invalid escape in string" );
                }
            } else if ( codePoint < ' ' ) {
                throw cursor.error( "Illegal character in string" );
            } else {
                sb.appendCodePoint( codePoint );
            }
        }
        cursor.read();
        return sb.toString();
    }

    private Double parseNumber() {
        StringBuilder sb = new StringBuilder();
        int current = cursor.peek();
        if ( current == '-' ) {
            sb.appendCodePoint( current );
            current = cursor.peekNext();
        }

        while ( current >= '0' && current <= '9' ) {
            sb.appendCodePoint( current );
            current = cursor.peekNext();
        }
        if ( current == '.' ) {
            sb.append( '.' );
            current = cursor.peekNext();
            while ( current >= '0' && current <= '9' ) {
                sb.appendCodePoint( current );
                current = cursor.peekNext();
            }
        }
        if ( current == 'e' || current == 'E' ) {
            sb.appendCodePoint( current );
            current = cursor.peekNext();
            if ( current == '+' || current == '-' ) {
                sb.appendCodePoint( current );
                current = cursor.peekNext();
            }
            while ( current >= '0' && current <= '9' ) {
                sb.appendCodePoint( current );
                current = cursor.peekNext();
            }
        }
        if ( Character.isJavaIdentifierStart( current ) ) {
            throw cursor.error( "Illegal number expression" );
        }

        return Double.parseDouble( sb.toString() );
    }


    private String parseIdentifier() {
        StringBuilder sb = new StringBuilder();
        sb.appendCodePoint( cursor.peek() );
        for ( int codePoint = cursor.peekNext(); Character.isJavaIdentifierPart( codePoint ); codePoint = cursor.peekNext() ) {
            sb.appendCodePoint( codePoint );
        }
        return sb.toString();
    }

    public Token getToken() {
        if ( !parsed ) {
            skipWhiteSpace();
            int current = cursor.peek();
            if ( current < 0 ) {
                setToken( null );
            } else if ( current == '{' ) {
                cursor.read();
                setToken( Token.OPEN_OBJECT );
            } else if ( current == '}' ) {
                cursor.read();
                setToken( Token.CLOSE_OBJECT );
            } else if ( current == '[' ) {
                cursor.read();
                setToken( Token.OPEN_ARRAY );
            } else if ( current == ']' ) {
                cursor.read();
                setToken( Token.CLOSE_ARRAY );
            } else if ( current == ',' ) {
                cursor.read();
                setToken( Token.COMMA );
            } else if ( current == ':' ) {
                cursor.read();
                setToken( Token.COLON );
            } else if ( current == '"' || current == '\'' ) {
                setToken( Token.STRING, parseString() );
            } else if ( current >= '0' && current <= '9' || current == '-' ) {
                setToken( Token.DOUBLE, parseNumber() );
            } else if ( Character.isJavaIdentifierStart( current ) ) {
                String s = parseIdentifier();
                if ( "true".equalsIgnoreCase( s ) ) {
                    setToken( Token.BOOLEAN, Boolean.TRUE );
                } else if ( "false".equalsIgnoreCase( s ) ) {
                    setToken( Token.BOOLEAN, Boolean.FALSE );
                } else if ( "null".equalsIgnoreCase( s ) ) {
                    setToken( Token.NULL );
                } else {
                    setToken( Token.STRING, s );
                }
            } else {
                throw cursor.error( "Illegal input" );
            }

        }
        return token;
    }

    public void skip() {
        parsed = false;
    }

    public Token readToken( Token token ) {
        if ( getToken() == token ) {
            skip();
            return token;
        } else {
            throw new IllegalStateException(  );
        }
    }

    public String readString() {
        String result;
        if ( getToken() == Token.STRING ) {
            result = (String) value;
        } else {
            throw new IllegalStateException();
        }
        skip();
        return result;
    }

    public Double readDouble() {
        Double result;
        if ( getToken() == Token.DOUBLE ) {
            result = (Double) value;
        } else {
            throw new IllegalStateException();
        }
        skip();
        return result;
    }

    public Boolean readBoolean() {
        Boolean result;
        if ( getToken() == Token.BOOLEAN ) {
            result = (Boolean) value;
        } else {
            throw new IllegalStateException();
        }
        skip();
        return result;
    }

    public static enum Token {
        OPEN_OBJECT,
        CLOSE_OBJECT,
        OPEN_ARRAY,
        CLOSE_ARRAY,
        COLON,
        COMMA,
        STRING,
        DOUBLE,
        BOOLEAN,
        NULL
    }
}
