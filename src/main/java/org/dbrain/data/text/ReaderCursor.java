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
import java.util.function.IntPredicate;

/**
 * Helper class to build recursive parsing routines. This class holds the "current" character in a buffer that
 * can be read multiple times without affecting the underlying reader.
 */
public class ReaderCursor implements AutoCloseable {

    private static final String ERR_IO_ERROR = "IO exception";
    private static final String ERR_SURROGATE_ENCODING = "Surrogate encoding error";

    // Linked reader
    private Reader reader;

    // Current byte in buffer if consumed = false
    private int current = -1;

    // Position in the stream.
    private int index = 1;
    private int line = 1;
    private int column = 1;

    // Tell if current is valid or not
    private boolean consumed = true;

    /**
     * Creates a new instance of ReaderCursor using the underlying Reader as it's
     * datasource.
     */
    public ReaderCursor( Reader r ) {
        this.reader = r;
    }

    /**
     * load Current character if needed.
     */
    private void load() {
        try {
            if ( consumed ) {
                current = reader.read();
                index++;
                if ( current >= 0 ) {
                    char highSurrogate = (char) current;
                    if ( Character.isHighSurrogate( highSurrogate ) ) {
                        int next = reader.read();
                        index++;
                        if ( next >= 0 ) {
                            char lowSurrogate = (char)next;
                            if (Character.isSurrogatePair( highSurrogate, lowSurrogate )) {
                                current = Character.toCodePoint( highSurrogate, lowSurrogate );
                            } else {
                                throw error( ERR_SURROGATE_ENCODING, null );
                            }
                        } else {
                            throw error( ERR_SURROGATE_ENCODING, null );
                        }
                    }
                    if ( current == 10 ) {
                        line ++;
                        column = 1;
                    } else if ( current == 9 ) {
                        column += 4;
                    } else if ( current >= ' ') {
                        column ++;
                    }
                }
                consumed = false;
            }
        } catch ( IOException ie ) {
            throw error( ERR_IO_ERROR, ie );
        }
    }

    /**
     * An exception with the specified message.
     * @param message The message.
     */
    public ParseException error( String message, Throwable e ) {
        return new ParseException( String.format( "%s at %s.", message, position() ), e );
    }

    /**
     * An exception with the specified message.
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
     * @return The current character in the cursor.
     * <p>
     * Note: If the current character is consumed. The next one is loaded from
     * the underlying reader.
     */
    public int current() {
        load();
        return current;
    }

    /**
     * return true if the current cursor's position is matching the predicated.
     */
    public boolean is( IntPredicate test ) {
        return test.test( current() );
    }

    /**
     * Consume the current character and return the next available.
     *
     * @return The next available character or -1 if none.
     */
    public int next() {
        discard();
        return current();
    }

    /**
     * @param count
     * @return A string of characters. This method can return more characters than asked since it counts codepoints.
     */
    public String next( int count ) {
        if ( count <= 0 ) {
            return "";
        }
        StringBuilder sb = new StringBuilder( count );
        int c = next();
        for ( int i = 0; i < count && c >= 0; i++ ) {
            sb.appendCodePoint( c );
            c = next();

        }
        return sb.toString();
    }

    /**
     * Invalidate current character without reading the next one.
     */
    public void discard() {
        consumed = true;
    }

    /**
     * Close the underlying reader.
     */
    @Override
    public void close() throws Exception {
        reader.close();
    }

}
