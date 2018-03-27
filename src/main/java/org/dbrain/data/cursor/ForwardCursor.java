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

package org.dbrain.data.cursor;

/**
 * Allows to read data in a Forward-only manner.
 */
public interface ForwardCursor {

    /**
     * @return true if the cursor is at beginning-of-file.
     */
    boolean bof();

    /**
     * @return true if the cursor is at end-of-file.
     */
    boolean eof();

    /**
     * Move the cursor to the next row in the resultset.
     *
     * @return true if the cursor is not at end of file.
     */
    boolean next();

}
