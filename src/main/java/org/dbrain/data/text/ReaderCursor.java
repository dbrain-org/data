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

/**
 * Helper class to build recursive parsing routines. This class holds the "current" character in a buffer that
 * can be read multiple times without affecting the underlying reader.
 */
public class ReaderCursor implements AutoCloseable {

    private static final String ERR_IO_ERROR           = "IO exception";
    private static final String ERR_SURROGATE_ENCODING = "Surrogate encoding error";

    // Linked reader
    private Reader reader;

    private Integer peeked;

    // Position in the stream.
    private int index  = 1;
    private int line   = 1;
    private int column = 1;

    /**
     * Creates a new instance of ReaderCursor using the underlying Reader as it's
     * datasource.
     */
    public ReaderCursor( Reader r ) {
        this.reader = r;
    }

    private int readCodePoint() {
        try {
            int firstChar = reader.read();
            if ( firstChar >= 0 ) {
                if ( Character.isHighSurrogate( (char) firstChar ) ) {
                    int secondChar = reader.read();
                    if ( secondChar >= 0 ) {
                        if ( Character.isSurrogatePair( (char) firstChar, (char) secondChar ) ) {
                            return Character.toCodePoint( (char) firstChar, (char) secondChar );
                        } else {
                            throw error( ERR_SURROGATE_ENCODING, null );
                        }
                    } else {
                        throw error( ERR_SURROGATE_ENCODING, null );
                    }
                }
            }
            return firstChar;
        } catch ( IOException ie ) {
            throw error( ERR_IO_ERROR, ie );
        }
    }

    /**
     * An exception with the specified message.
     *
     * @param message The message.
     */
    public ParseException error( String message, Throwable e ) {
        return new ParseException( String.format( "%s at %s.", message, position() ), e );
    }

    /**
     * An exception with the specified message.
     *
     * @param message The message.
     */
    public ParseException error( String message ) {
        return error( message, null );
    }

    /**
     * @return A positional string.
     */
    private String position() {
        return String.format( "position %d [%d:%d]", index, line, column );
    }

    /**
     * @return Get at the character at the current cursor position.
     *
     * Note: If the current character is consumed. The next one is loaded from
     * the underlying reader.
     */
    public int get() {
        if ( peeked == null ) {
            peeked = readCodePoint();
        }
        return peeked;
    }

    /**
     * Move cursor forward one position and get the character.
     *
     * @return The next available character or -1 if none.
     */
    public int getNext() {
        read();
        return get();
    }

    /**
     * @return The current character and move the cursor forward.
     */
    public int read() {
        int result = get();
        if ( result >= 0 ) {
            peeked = null;
            index += result <= 0xFFFF ? 1 : 2;
            if ( result == 10 ) {
                line++;
                column = 1;
            } else if ( result == 9 ) {
                column += 4;
            } else if ( result >= ' ' ) {
                column++;
            }
        }
        return result;
    }

    /**
     * @param count
     * @return A string of characters. This method can return more characters than asked since it counts codepoints.
     */
    public String read( int count ) {
        if ( count <= 0 ) {
            return "";
        }
        StringBuilder sb = new StringBuilder( count );
        int c = read();
        for ( int i = 0; i < count && c >= 0; i++ ) {
            sb.appendCodePoint( c );
            c = read();

        }
        return sb.toString();
    }

    /**
     * Close the underlying reader.
     */
    @Override
    public void close() throws Exception {
        reader.close();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder(  );
        sb.append( "ReaderCursor index=" );
        sb.append( Integer.toString( index ) );
        sb.append( " char=[" );
        int peeked = get();
        if ( peeked >= 0 ) {
            sb.appendCodePoint( get() );
        } else {
            sb.append( "eof" );
        }
        sb.append( "]" );

        return sb.toString();
    }
}
