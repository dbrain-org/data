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

package org.dbrain.data.json;

import junit.framework.Assert;
import org.junit.Test;

import java.io.StringReader;

/**
 * Created by epoitras on 27/06/14.
 */
public class JsonParserTest {

    @Test
    public void testParseString1() throws Exception {

        String json = "\"test\"";
        JsonParser parser = new JsonParser( new StringReader( json ) );

        Assert.assertEquals( parser.getToken(), JsonParser.Token.STRING );
        Assert.assertEquals( parser.readString(), "test" );

    }

    @Test
    public void testParseString2() throws Exception {

        String json = "'test'";
        JsonParser parser = new JsonParser( new StringReader( json ) );

        Assert.assertEquals( parser.getToken(), JsonParser.Token.STRING );
        Assert.assertEquals( parser.readString(), "test" );

    }

    @Test
    public void testParseString3() throws Exception {

        String json = " test ";
        JsonParser parser = new JsonParser( new StringReader( json ) );

        Assert.assertEquals( parser.getToken(), JsonParser.Token.STRING );
        Assert.assertEquals( parser.readString(), "test" );

    }

    @Test
    public void testParseString4() throws Exception {

        String json = " test[";
        JsonParser parser = new JsonParser( new StringReader( json ) );

        Assert.assertEquals( parser.getToken(), JsonParser.Token.STRING );
        Assert.assertEquals( parser.readString(), "test" );



    }


    @Test
    public void testParseNumber1() throws Exception {

        String json = "123.345e-12";
        JsonParser parser = new JsonParser( new StringReader( json ) );

        Assert.assertEquals( parser.getToken(), JsonParser.Token.DOUBLE );
        Assert.assertEquals( parser.readDouble(), 123.345e-12 );

    }

    @Test
    public void testParseNumber2() throws Exception {

        String json = "-123.345e-12";
        JsonParser parser = new JsonParser( new StringReader( json ) );

        Assert.assertEquals( parser.getToken(), JsonParser.Token.DOUBLE );
        Assert.assertEquals( parser.readDouble(), -123.345e-12 );

    }

    @Test
    public void testParseBoolean1() throws Exception {

        String json = "true false";
        JsonParser parser = new JsonParser( new StringReader( json ) );

        Assert.assertEquals( parser.getToken(), JsonParser.Token.BOOLEAN );
        Assert.assertEquals( parser.readBoolean(), Boolean.TRUE );

        Assert.assertEquals( parser.getToken(), JsonParser.Token.BOOLEAN );
        Assert.assertEquals( parser.readBoolean(), Boolean.FALSE );

        Assert.assertNull( parser.getToken() );


    }

    @Test
    public void testParseNull1() throws Exception {

        String json = "null";
        JsonParser parser = new JsonParser( new StringReader( json ) );

        Assert.assertEquals( parser.getToken(), JsonParser.Token.NULL );
        parser.skip();

        Assert.assertNull( parser.getToken() );


    }

}
