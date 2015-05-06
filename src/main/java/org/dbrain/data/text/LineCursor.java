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

import java.io.Reader;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 26/07/13
 * Time: 6:05 PM
 * To change this template use File | Settings | File Templates.
 */
public class LineCursor {

    private boolean eof = false;
    private ReaderCursor cursor;
    private String       currentLine;

    public LineCursor( Reader r ) {
        cursor = new ReaderCursor( r );
    }

    /**
     * Load a line into the internal buffer.
     */
    private void load() {
        if ( currentLine == null && !eof ) {
            int current = cursor.get();
            if ( current >= 0 ) {
                StringBuilder sb = new StringBuilder();
                while ( current >= 0 && current != 13 && current != 10 ) {
                    sb.append( (char) current );
                    current = cursor.getNext();
                }
                if ( current == 13 ) {
                    if ( cursor.getNext() == 10 ) {
                        cursor.read();
                    }
                }
                if ( current == 10 ) {
                    if ( cursor.getNext() == 13 ) {
                        cursor.read();
                    }
                }
                currentLine = sb.toString();
            } else {
                eof = true;
                currentLine = null;
            }
        }
    }

    /**
     * The the line currently loaded in the cursor. If none is loaded, this will cause a new line to be red from the stream.
     *
     * @return The the line currently loaded in the cursor.
     */
    public String get() {
        load();
        return currentLine;
    }

    /**
     * @return The next line in the file, or null if at end of file.
     */
    public String getNext() {
        read();
        return get();
    }

    /**
     * Read the current line and move to the next.
     */
    public String read() {
        String result = get();
        currentLine = null;
        return result;
    }


}
