package epic.data.json;

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
