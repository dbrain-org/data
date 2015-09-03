/*
 * Copyright [2015] [Eric Poitras]
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

package org.dbrain.data.impl.path;

import org.dbrain.data.Path;
import org.dbrain.data.PathPattern;
import org.dbrain.data.text.ParserUtils;
import org.dbrain.data.text.ReaderCursor;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

/**
 * Implements parsing of Path and Patterns.
 */
public final class PathUtils {

    /**
     * Create a fully qualified name from a ReaderCursor.
     */
    public static Path parsePath( ReaderCursor c ) {

        // Skip whitespace
        skipWhitespace( c );

        // Parse the name
        if ( isPathStart( c.current() ) ) {
            List<Object> nodes = new ArrayList<>();
            nodes.add( readNode( c ) );
            int current = c.current();
            while ( true ) {
                if ( isIndexOpen( current ) ) {
                    nodes.add( readBracketContent( c ) );
                } else if ( isAttributeAccessor( current ) ) {
                    c.next(); // Skip accessor
                    nodes.add( readAttribute( c ) );
                } else {
                    break;
                }
                current = c.current();
            }
            return new PathImpl( nodes );
        }
        return PathImpl.EMPTY_PATH;
    }

    /**
     * Create a new Path from a String. Compatible with toString.
     */
    public static Path parsePath( String path ) {
        if ( path == null ) {
            return PathImpl.EMPTY_PATH;
        }
        ReaderCursor c = new ReaderCursor( new StringReader( path ) );

        // Parse the name
        Path result = parsePath( c );

        // Skip trailing whitespace
        skipWhitespace( c );

        // Expect EOF
        if ( c.current() >= 0 ) {
            throw c.error( "Expecting end of string" );
        }

        return result;
    }

    // Read a wildcard segment
    private static void readWildcardNode( ReaderCursor c, PathPattern.Builder to ) {
        if ( isWildcard( c.next() ) ) {
            to.any();
            c.read();
        } else {
            to.one();
        }
    }


    // Read a segment.
    private static void readPatternNode( ReaderCursor c, PathPattern.Builder to ) {
        int cur = c.current();
        if ( isAttributeStart( cur ) ) {
            to.attr( readAttribute( c ) );
        } else if ( isIndexOpen( cur ) ) {
            readBracketContent( c, to );
        } else if ( isWildcard( cur ) ) {
            readWildcardNode( c, to );
        } else {
            c.error( "Expecting an attribute or an index" );
        }
    }


    /**
     * Create a fully qualified name pattern from a ReaderCursor.
     */
    public static PathPattern parsePathPattern( ReaderCursor c ) {

        // Skip whitespace
        skipWhitespace( c );

        // Parse the name
        if ( isPathPatternNode( c.current() ) ) {
            PathPatternBuilderImpl builder = new PathPatternBuilderImpl();
            readPatternNode( c, builder );
            while ( c.current() == '.' ) {
                c.read();
                readPatternNode( c, builder );
            }
            return builder.build();
        }
        return new PathPatternBuilderImpl().build();

    }

    /**
     * Create a new Fully Qualified Name from a String. Compatible with toString.
     */
    public static PathPattern parsePathPattern( String fqn ) {
        if ( fqn == null ) {
            return PathPatternImpl.EMPTY_PATTERN;
        }
        ReaderCursor c = new ReaderCursor( new StringReader( fqn ) );

        // Parse the name
        PathPattern result = parsePathPattern( c );

        // Skip trailing whitespace
        skipWhitespace( c );

        // Expect EOF
        if ( c.current() >= 0 ) {
            throw c.error( "Expecting end of string" );
        }

        return result;
    }


    // Skip consecutive white spaces
    private static void skipWhitespace( ReaderCursor c ) {
        while ( ParserUtils.isSpace( c.current() ) ) {
            c.read();
        }
    }

    // True if the character is a quote.
    private static boolean isQuote( int cur ) {
        return cur == '\'';
    }

    // True if the character is an openning bracket.
    private static boolean isIndexOpen( int cur ) {
        return cur == '[';
    }

    // True if the character is an closing bracket.
    private static boolean isIndexClose( int cur ) {
        return cur == ']';
    }

    private static boolean isAttributeAccessor( int cur ) {
        return cur == '.';
    }


    // True if the current character is a wildcard
    private static boolean isWildcard( int cur ) {
        return cur == '*';
    }

    // True if the character is a possible path node.
    private static boolean isPathStart( int cur ) {
        return cur >= 0 && ( isAttributeStart( cur ) || isIndexOpen( cur ) );
    }

    // True if the character is a possible fully qualified name start.
    private static boolean isPathPatternNode( int cur ) {
        return isPathStart( cur ) || isWildcard( cur );
    }

    // True if the character is an unreserved attribute character.
    private static boolean isAttributeStart( int cur ) {
        return cur >= 0 && Character.isJavaIdentifierStart( cur );
    }

    // True if the character is an unreserved attribute character.
    private static boolean isAttributePart( int cur ) {
        return cur >= 0 && Character.isJavaIdentifierPart( cur );
    }

    // True if the character is a digit.
    private static boolean isDigit( int cur ) {
        return cur >= '0' && cur <= '9';
    }


    // Read a quoted attribute.
    private static String readQuotedAttribute( ReaderCursor c ) {
        int quote = c.read();
        StringBuilder sb = new StringBuilder();
        do {
            int current = c.read();
            if ( current == quote ) {
                if ( c.current() == quote ) {
                    sb.appendCodePoint( c.read() );
                } else {
                    break;
                }
            } else if ( current < 0 ) {
                throw c.error( "Unexpected eof" );
            } else {
                sb.appendCodePoint( current );
            }
        } while ( true );
        return sb.toString();
    }

    // Read an attribute.
    private static String readAttribute( ReaderCursor c ) {
        // Should be called where current = isAttributeStart
        StringBuilder sb = new StringBuilder();
        for (int cur = c.current(); isAttributePart( cur ); cur = c.next() ) {
            sb.appendCodePoint( cur );
        }
        return sb.toString();
    }

    private static long readLong( ReaderCursor c ) {
        long index = 0;
        for ( int cur = c.current(); isDigit( cur ); cur = c.next() ) {
            index = index * 10 + cur - '0';
        }
        return index;
    }

    // Read an index.
    private static void readBracketContent( ReaderCursor c, Path.Builder to ) {
        c.next(); // Skip Index start

        // skip leading whitespaces
        skipWhitespace( c );

        int cur = c.current();
        if ( isDigit( cur ) ) {
            to.index( readLong( c ) );
        } else if ( isQuote( cur ) ) {
            to.attr( readQuotedAttribute( c ) );
        } else {
            throw c.error( "Expecting quoted string or numerical index" );
        }

        // skip trailing whitespaces
        skipWhitespace( c );

        if ( !isIndexClose( c.read() ) ) {
            throw c.error( "Expecting ]" );
        }
    }

    // Read an index.
    private static void readBracketContent( ReaderCursor c, PathPattern.Builder to ) {
        c.next(); // Skip Index start

        // skip leading whitespaces
        skipWhitespace( c );

        int cur = c.current();
        if ( isDigit( cur ) ) {
            to.index( readLong( c ) );
        } else if ( isQuote( cur ) ) {
            to.attr( readQuotedAttribute( c ) );
        } else {
            throw c.error( "Expecting quoted string or numerical index" );
        }

        // skip trailing whitespaces
        skipWhitespace( c );

        if ( !isIndexClose( c.read() ) ) {
            throw c.error( "Expecting ]" );
        }
    }

    // Read a node.
    private static Object readNode( ReaderCursor c ) {
        int cur = c.current();
        if ( isQuote( cur ) ) {
            return readQuotedAttribute( c );
        } else if ( isAttributeStart( cur ) ) {
            return readAttribute( c );
        } else if ( isIndexOpen( cur ) ) {
            return readBracketContent( c );
        } else {
            throw c.error( "Expected path node" );
        }
    }

    /**
     * Encode a path attribute.
     */
    static String encodeAttribute( String attr ) {
        if ( attr.length() == 0 ) {
            return "''";
        }

        // We will allocate the string builder only if we escape the attribute.
        StringBuilder sb = null;
        for ( int i = 0; i < attr.length(); i++ ) {
            Character c = attr.charAt( i );
            if ( ( i > 0 && isAttributePart( c ) ) || ( i == 0 && isAttributeStart( c ) ) ) {
                if ( sb != null ) {
                    sb.append( c );
                }
            } else {
                if ( sb == null ) {
                    sb = new StringBuilder( attr.length() + 12 );
                    sb.append( "'" );
                    sb.append( attr.substring( 0, i ) );
                }
                if ( c == '\'' ) {
                    sb.append( "''" );
                } else {
                    sb.append( c );
                }
            }
        }

        // Close the string as we escaped it.
        if ( sb != null ) {
            sb.append( "\'" );
            return sb.toString();
        } else {
            return attr;
        }
    }
}
