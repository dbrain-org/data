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

package org.dbrain.data;

import org.dbrain.data.text.ParseException;
import org.dbrain.data.text.ReaderCursor;
import org.junit.Assert;
import org.junit.Test;

import java.io.Reader;
import java.io.StringReader;

/**
 * Test Fully Qualified Names.
 */
public class Fqn_Test {

    @Test
    public void testEquals() throws Exception {
        Assert.assertTrue( Fqn.of( "" ).equals( Fqn.of( "" ) ) );
        Assert.assertTrue( Fqn.of( "test" ).equals( Fqn.of( "test" ) ) );
        Assert.assertTrue( Fqn.of( "test.test2" ).equals( Fqn.of( "test.test2" ) ) );
        Assert.assertFalse( Fqn.of( "" ).equals( Fqn.of( "test" ) ) );
        Assert.assertFalse( Fqn.of( "test2" ).equals( Fqn.of( "test" ) ) );
        Assert.assertFalse( Fqn.of( "test" ).equals( Fqn.of( "" ) ) );
        Assert.assertFalse( Fqn.of( "test" ).equals( Fqn.of( "test.test2" ) ) );
        Assert.assertFalse( Fqn.of( "test.test2" ).equals( Fqn.of( "test" ) ) );

        Assert.assertEquals( Fqn.of( "" ).hashCode(), Fqn.of( "" ).hashCode() );
        Assert.assertEquals( Fqn.of( "test" ).hashCode(), Fqn.of( "test" ).hashCode() );
        Assert.assertEquals( Fqn.of( "test.test2" ).hashCode(), Fqn.of( "test.test2" ).hashCode() );
    }


    @Test
    public void testOfReader() throws Exception {
        Assert.assertTrue( Fqn.of( new ReaderCursor( "" ) ).equals( Fqn.of( "" ) ) );
        Assert.assertTrue( Fqn.of( new ReaderCursor( "   " ) ).equals( Fqn.of( "" ) ) );
        Assert.assertTrue( Fqn.of( new ReaderCursor( "test.test2" ) ).equals( Fqn.of( "test.test2" ) ) );
        Assert.assertTrue( Fqn.of( new ReaderCursor( " test.test2 " ) ).equals( Fqn.of( "test.test2" ) ) );
    }

    /**
     * Test the parsing method of and toString at the same time.
     */
    @Test
    public void testOfString() throws Exception {
        Assert.assertEquals( "", Fqn.of( "" ).toString() );
        Assert.assertEquals( "", Fqn.of( " " ).toString() );
        Assert.assertEquals( "test", Fqn.of( "test" ).toString() );
        Assert.assertEquals( "test", Fqn.of( " test " ).toString() );
        Assert.assertEquals( "'test*'", Fqn.of( "'test*'" ).toString() );
        Assert.assertEquals( "'test'''", Fqn.of( "'test'''" ).toString() );
        Assert.assertEquals( "'test.'", Fqn.of( "'test.'" ).toString() );
        Assert.assertEquals( "'test.'.''.'''123'.'123'''", Fqn.of( "'test.'.''.'''123'.'123'''" ).toString() );
    }

    /**
     * Test the ofSegment factory method
     */
    @Test
    public void testOfSegment() throws Exception {
        Assert.assertEquals( "", Fqn.ofSegment( null ).toString() );
        Assert.assertEquals( "''", Fqn.ofSegment( "" ).toString() );
        Assert.assertEquals( "test", Fqn.ofSegment( "test" ).toString() );
        Assert.assertEquals( "'test.toto'", Fqn.ofSegment( "test.toto" ).toString() );
    }

    /**
     * Test builder scenarios.
     *
     * @throws Exception
     */
    @Test
    public void testBuilder() throws Exception {
        Assert.assertEquals( Fqn.of( "" ), Fqn.newBuilder().build() );
        Assert.assertEquals( Fqn.of( "test" ), Fqn.newBuilder().segment( "test" ).build() );
        Assert.assertEquals( Fqn.of( "test" ), Fqn.fromSegment( "test" ).build() );
        Assert.assertEquals( Fqn.of( "test.test2" ), Fqn.newBuilder().segment( "test" ).segment( "test2" ).build() );
        Assert.assertEquals( Fqn.of( "test.test2" ), Fqn.fromSegment( "test" ).segment( "test2" ).build() );
        Assert.assertEquals( Fqn.of( "test.test2.test3" ),
                             Fqn.from( Fqn.of( "test.test2" ) ).segment( "test3" ).build() );
        Assert.assertEquals( Fqn.of( "test" ), Fqn.from( null ).segment( "test" ).build() );
        Assert.assertEquals( Fqn.of( "test.test2.test3" ),
                             Fqn.newBuilder().segment( "test" ).append( Fqn.of( "test2.test3" ) ).build() );
        Assert.assertEquals( Fqn.of( "test" ), Fqn.newBuilder().segment( "test" ).append( null ).build() );
    }

    /**
     * Test invalid encoding.
     */
    @Test
    public void testOfString_invalid() throws Exception {
        String[] invalidFqn = { ".", "*", "[", "]", "*", ",", "test. toto" };

        for ( String s : invalidFqn ) {
            try {
                Fqn.of( s );
                throw new IllegalStateException( "Should not encode: " + s );
            } catch ( ParseException e ) {
                // expected.
            }
        }
    }


    /**
     * Test the size method.
     */
    @Test
    public void testSize() throws Exception {
        Assert.assertEquals( 1, Fqn.of( "''" ).size() );
        Assert.assertEquals( 2, Fqn.of( "'test*'.''" ).size() );
        Assert.assertEquals( 3, Fqn.of( "'test'''.'*'.'**'" ).size() );
        Assert.assertEquals( 4, Fqn.of( "1.2.3.4" ).size() );
    }

    /**
     * Test the segment method.
     */
    @Test
    public void testSegment() throws Exception {
        Assert.assertEquals( "test", Fqn.of( "test" ).segment( 0 ) );
        Assert.assertEquals( "", Fqn.of( "''" ).segment( 0 ) );
        Assert.assertEquals( "*", Fqn.of( "test.'*'" ).segment( 1 ) );
        Assert.assertEquals( "", Fqn.of( "test.''" ).segment( 1 ) );
    }

    @Test(expected = IndexOutOfBoundsException.class )
    public void testOutOfBoundSegment1() throws Exception {
        Fqn.of( "test").segment( 1 );
    }

    @Test(expected = IndexOutOfBoundsException.class )
    public void testOutOfBoundSegment2() throws Exception {
        Fqn.of( "" ).segment( 0 );
    }


    @Test
    public void testStartsWith() throws Exception {

        // Those are true
        Assert.assertTrue( Fqn.of( "test.123" ).startsWith( Fqn.of( "test" ) ) );
        Assert.assertTrue( Fqn.of( "test.123" ).startsWith( Fqn.of( "test.123" ) ) );
        Assert.assertTrue( Fqn.of( "test.123.456" ).startsWith( Fqn.of( "test" ) ) );
        Assert.assertTrue( Fqn.of( "test.123.456" ).startsWith( Fqn.of( "test.123" ) ) );
        Assert.assertTrue( Fqn.of( "test.123" ).startsWith( Fqn.of( "" ) ) );
        Assert.assertTrue( Fqn.of( "test.123" ).startsWith( null ) );
        Assert.assertTrue( Fqn.of( "" ).startsWith( Fqn.of( "" ) ) );

        // Those are false
        Assert.assertFalse( Fqn.of( "test.123" ).startsWith( Fqn.of( "other" ) ) );
        Assert.assertFalse( Fqn.of( "test" ).startsWith( Fqn.of( "test.123" ) ) );
        Assert.assertFalse( Fqn.of( "test.123" ).startsWith( Fqn.of( "test.123.456" ) ) );
        Assert.assertFalse( Fqn.of( "" ).startsWith( Fqn.of( "test" ) ) );

    }


}
