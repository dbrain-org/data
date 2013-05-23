/*
 * Copyright [2013] [Eric Poitras]
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package pragmatic.data.parsing;

import pragmatic.data.ParseException;

import java.io.IOException;
import java.io.Reader;

/**
 * Helper class to build recursive parsing routines. This class holds the "current" character in a buffer that
 * can be read multiple times without affecting the underlying reader.
 *
 * @author PoitraE
 */
public class ParseCursor implements AutoCloseable {

    // Linked reader
    private Reader reader;

    // Current byte in buffer if consumed = false
    private int current = -1;

    // Tell if current is valid or not
    private boolean consumed = true;

    /**
     * Creates a new instance of ReaderCursor using the underlying Reader as it's
     * datasource.
     */
    public ParseCursor( Reader r ) {
        this.reader = r;
    }

    /**
     * load Current character if needed.
     */
    private void load() {
        try {

            if ( consumed ) current = reader.read();
            consumed = false;

        } catch ( IOException ie ) {
            throw new ParseException( ie.getMessage(), ie );
        }
    }

    /**
     * @return The current character in the cursor.
     *         <p/>
     *         Note: If the current character is consumed. The next one is loaded from
     *         the underlying reader.
     */
    public int getCurrent() {
        load();
        return current;
    }

    public int getCurrent( boolean doConsume ) {
        int retval = getCurrent();
        if ( doConsume ) consume();
        return retval;
    }

    /**
     * Consume the current character and return the next available.
     *
     * @return The next available character or -1 if none.
     */
    public int getNext() {
        consume();
        return getCurrent();
    }

    /**
     * Invalidate current character without reading the next one.
     */
    public void consume() {
        consumed = true;
    }

    /**
     * @return the next current character available or -1 if eof.
     */
    public boolean isConsumed() {
        return consumed;
    }

    /**
     * Close the underlying reader.
     */
    @Override
    public void close() throws Exception {
        reader.close();
    }

}
