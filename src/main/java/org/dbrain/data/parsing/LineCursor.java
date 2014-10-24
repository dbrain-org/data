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

package org.dbrain.data.parsing;

import java.io.Reader;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 26/07/13
 * Time: 6:05 PM
 * To change this template use File | Settings | File Templates.
 */
public class LineCursor {

    private long         lineNumber = 0;
    private CursorStatus status     = CursorStatus.BOF;
    private ParseCursor cursor;
    private String      currentLine;

    public LineCursor( Reader r ) {
        cursor = new ParseCursor( r );
    }

    /**
     * Load a line into the internal buffer.
     */
    private void load() {
        if ( currentLine == null && status == null ) {
            int current = cursor.getCurrent();
            if ( current >= 0 ) {
                StringBuilder sb = new StringBuilder();
                while ( current >= 0 && current != 13 && current != 10 ) {
                    sb.append( (char) current );
                    current = cursor.read();
                }
                if ( current == 13 ) {
                    if ( cursor.read() == 10 ) {
                        cursor.consume();
                    }
                }
                if ( current == 10 ) {
                    if ( cursor.read() == 13 ) {
                        cursor.consume();
                    }
                }
                currentLine = sb.toString();
                status = CursorStatus.LOADED;
                lineNumber++;
            } else {
                status = CursorStatus.EOF;
            }
        }
    }

    /**
     * Unload the current line.
     */
    private void unload() {
        if ( status != CursorStatus.EOF ) {
            status = null;
            currentLine = null;
        }


    }

    public CursorStatus getStatus() {
        load();
        return status;
    }

    /**
     * The the line currently loaded in the cursor. If none is loaded, this will cause a new line to be red from the stream.
     *
     * @return The the line currently loaded in the cursor.
     */
    public String getCurrent() {
        load();
        return currentLine;
    }

    /**
     * Move to the next line and return true if cursor is not at eof.
     */
    public boolean next() {
        unload();
        return getStatus() == CursorStatus.LOADED;
    }

    /**
     * @return The next line in the file, or null if at end of file.
     */
    public String getNext() {
        next();
        return getCurrent();
    }


}
