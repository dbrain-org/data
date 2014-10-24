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

import junit.framework.Assert;
import org.junit.Test;

import java.io.InputStreamReader;
import java.io.Reader;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 26/07/13
 * Time: 6:35 PM
 * To change this template use File | Settings | File Templates.
 */
public class LineCursorTest {

    @Test
    public void testRead() throws Exception {
        Reader r = new InputStreamReader( getClass().getResourceAsStream( "/TestLineCursor.txt" ), "UTF-8" );

        LineCursor lineCursor = new LineCursor( r );

        String line1 = lineCursor.getNext();
        String line2 = lineCursor.getNext();
        String line3 = lineCursor.getNext();
        String eof = lineCursor.getNext();

        Assert.assertEquals( line1, "Line1");
        Assert.assertEquals( line2, "Line2");
        Assert.assertEquals( line3, "Line3");

        Assert.assertNull( eof );

        r.close();

    }
}
