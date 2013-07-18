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

package epic.data.csv;

import junit.framework.Assert;
import org.junit.Test;

import java.io.InputStreamReader;
import java.io.Reader;

/**
 * Test the CsvCursor class.
 */
public class CsvCursorTest {

    @Test
    public void testCursor() throws Exception {
        Reader r = new InputStreamReader( getClass().getResourceAsStream( "/TestNamedColumn.csv" ), "UTF-8" ) ;

        CsvCursor csvCursor = new CsvCursor( r, ',' );

        Assert.assertTrue( csvCursor.next() );

        Assert.assertEquals( "Eric1", csvCursor.get( 0 ));
        Assert.assertEquals( "Eric1", csvCursor.get( "Col1" ) );
        Assert.assertEquals( csvCursor.get( 1 ), "1" );
        Assert.assertEquals( "1", csvCursor.get( "Col2" ) );
        Assert.assertEquals( csvCursor.get( 2 ), "2012-01-01" );
        Assert.assertEquals( "2012-01-01", csvCursor.get( "Col3" ) );

        Assert.assertTrue( csvCursor.next() );
        Assert.assertEquals( "Eric2", csvCursor.get( 0 ) );
        Assert.assertEquals( "2", csvCursor.get( 1 ) );
        Assert.assertEquals( "2012-01-02", csvCursor.get( 2 ) );

        Assert.assertFalse( csvCursor.next() );

        csvCursor.close();


    }
}
