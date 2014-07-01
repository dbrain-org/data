package epic.data.json;

import epic.data.ParseException;
import epic.data.parsing.ParseCursor;

import java.io.Reader;
import java.util.EnumSet;

/**
 * Created by epoitras on 27/06/14.
 */
public class JsonParser {

    private ParseCursor cursor;
    private boolean parsed = false;
    private Token  token;
    private Object value;

    public JsonParser( Reader r ) {
        cursor = new ParseCursor( r );
    }

    public ParseException error( String message ) {
        return cursor.error( message );
    }

    private void skipWhiteSpace() {
        int current = cursor.getCurrent();
        while ( current >= 0 && current <= ' ' || Character.isWhitespace( current ) ) {
            current = cursor.read();
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
        int quote = cursor.getCurrent();
        StringBuilder sb = new StringBuilder();
        for ( int codePoint = cursor.read(); codePoint != quote; codePoint = cursor.read() ) {
            if ( codePoint < 0 || codePoint == '\r' || codePoint == '\n' ) {
                throw cursor.error( "Unterminated string" );
            } else if ( codePoint == '\\' ) {
                codePoint = cursor.read();
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
        cursor.consume();
        return sb.toString();
    }

    private Double parseNumber() {
        StringBuilder sb = new StringBuilder();
        int current = cursor.getCurrent();
        if ( current == '-' ) {
            sb.appendCodePoint( current );
            current = cursor.read();
        }

        while ( current >= '0' && current <= '9' ) {
            sb.appendCodePoint( current );
            current = cursor.read();
        }
        if ( current == '.' ) {
            sb.append( '.' );
            current = cursor.read();
            while ( current >= '0' && current <= '9' ) {
                sb.appendCodePoint( current );
                current = cursor.read();
            }
        }
        if ( current == 'e' || current == 'E' ) {
            sb.appendCodePoint( current );
            current = cursor.read();
            if ( current == '+' || current == '-' ) {
                sb.appendCodePoint( current );
                current = cursor.read();
            }
            while ( current >= '0' && current <= '9' ) {
                sb.appendCodePoint( current );
                current = cursor.read();
            }
        }
        if ( Character.isJavaIdentifierStart( current ) ) {
            throw cursor.error( "Illegal number expression" );
        }

        return Double.parseDouble( sb.toString() );
    }


    private String parseIdentifier() {
        StringBuilder sb = new StringBuilder();
        sb.appendCodePoint( cursor.getCurrent() );
        for ( int codePoint = cursor.read(); Character.isJavaIdentifierPart( codePoint ); codePoint = cursor.read() ) {
            sb.appendCodePoint( codePoint );
        }
        return sb.toString();
    }

    public Token getToken() {
        if ( !parsed ) {
            skipWhiteSpace();
            int current = cursor.getCurrent();
            if ( current < 0 ) {
                setToken( null );
            } else if ( current == '{' ) {
                cursor.consume();
                setToken( Token.OPEN_OBJECT );
            } else if ( current == '}' ) {
                cursor.consume();
                setToken( Token.CLOSE_OBJECT );
            } else if ( current == '[' ) {
                cursor.consume();
                setToken( Token.OPEN_ARRAY );
            } else if ( current == ']' ) {
                cursor.consume();
                setToken( Token.CLOSE_ARRAY );
            } else if ( current == ',' ) {
                cursor.consume();
                setToken( Token.COMMA );
            } else if ( current == ':' ) {
                cursor.consume();
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
