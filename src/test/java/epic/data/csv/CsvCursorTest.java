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

import epic.data.Casts;
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
        Reader r = new InputStreamReader( getClass().getResourceAsStream( "/TestNamedColumn.csv" ), "UTF-8" );

        CsvCursor csvCursor = new CsvCursor( r, ',' );

        Assert.assertTrue( csvCursor.next() );

        Assert.assertEquals( "Eric1", csvCursor.get( 0 ) );
        Assert.assertEquals( "Eric1", csvCursor.get( "Col1" ) );

        Assert.assertEquals( "1", csvCursor.get( 1 ) );
        Assert.assertEquals( (Byte) ( (byte) 1 ), csvCursor.getByte( 1 ) );
        Assert.assertEquals( (Short) ( (short) 1 ), csvCursor.getShort( 1 ) );
        Assert.assertEquals( (Integer) ( (int) 1 ), csvCursor.getInt( 1 ) );
        Assert.assertEquals( (Long) 1L, csvCursor.getLong( 1 ) );
        Assert.assertEquals( 1f, csvCursor.getFloat( 1 ) );
        Assert.assertEquals( 1d, csvCursor.getDouble( 1 ) );
        Assert.assertEquals( "1", csvCursor.getString( 1 ) );

        Assert.assertEquals( (Object)"1", csvCursor.getAs( 1, Casts::identity ) );
        Assert.assertEquals( (Byte) ( (byte) 1 ), csvCursor.getByteAs( 1, Casts::identity ) );
        Assert.assertEquals( (Short) ( (short) 1 ), csvCursor.getShortAs( 1, Casts::identity ) );
        Assert.assertEquals( (Integer) 1, csvCursor.getIntAs( 1, Casts::identity ) );
        Assert.assertEquals( (Long) 1L, csvCursor.getLongAs( 1, Casts::identity ) );
        Assert.assertEquals( 1f, csvCursor.getFloatAs( 1, Casts::identity ) );
        Assert.assertEquals( 1d, csvCursor.getDoubleAs( 1, Casts::identity ) );
        Assert.assertEquals( "1", csvCursor.getStringAs( 1 , Casts::identity ) );

        Assert.assertEquals( "1", csvCursor.get( "Col2" ) );
        Assert.assertEquals( (Byte) ( (byte) 1 ), csvCursor.getByte( "Col2" ) );
        Assert.assertEquals( (Short) ( (short) 1 ), csvCursor.getShort( "Col2" ) );
        Assert.assertEquals( (Integer) ( (int) 1 ), csvCursor.getInt( "Col2" ) );
        Assert.assertEquals( (Long) 1L, csvCursor.getLong( "Col2" ) );
        Assert.assertEquals( 1f, csvCursor.getFloat( "Col2" ) );
        Assert.assertEquals( 1d, csvCursor.getDouble( "Col2" ) );
        Assert.assertEquals( "1", csvCursor.getString( "Col2" ) );

        Assert.assertEquals( (Object)"1", csvCursor.getStringAs( "Col2", Casts::identity ) );
        Assert.assertEquals( (Byte) ( (byte) 1 ), csvCursor.getByteAs( "Col2", Casts::identity ) );
        Assert.assertEquals( (Short) ( (short) 1 ), csvCursor.getShortAs( "Col2", Casts::identity ) );
        Assert.assertEquals( (Integer) ( (int) 1 ), csvCursor.getIntAs( "Col2", Casts::identity ) );
        Assert.assertEquals( (Long) 1L, csvCursor.getLongAs( "Col2", Casts::identity ) );
        Assert.assertEquals( 1f, csvCursor.getFloatAs( "Col2", Casts::identity ) );
        Assert.assertEquals( 1d, csvCursor.getDoubleAs( "Col2", Casts::identity ) );
        Assert.assertEquals( "1", csvCursor.getStringAs( "Col2", Casts::identity ) );

        Assert.assertEquals( "2012-01-01", csvCursor.get( 2 ) );
        Assert.assertEquals( "2012-01-01", csvCursor.get( "Col3" ) );

        Assert.assertTrue( csvCursor.next() );
        Assert.assertEquals( "Eric2", csvCursor.get( 0 ) );
        Assert.assertEquals( "2", csvCursor.get( 1 ) );
        Assert.assertEquals( "2012-01-02", csvCursor.get( 2 ) );

        Assert.assertFalse( csvCursor.next() );

        csvCursor.close();


    }
}
