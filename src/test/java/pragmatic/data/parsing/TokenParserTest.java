package pragmatic.data.parsing;

import junit.framework.Assert;
import org.junit.Test;
import pragmatic.data.parsing.TokenParser;

import java.io.Reader;
import java.io.StringReader;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 03/04/13
 * Time: 5:29 PM
 * To change this template use File | Settings | File Templates.
 */
public class TokenParserTest {

    @Test
    public void testTokenParserLitterals() throws Exception {

        Reader r = new StringReader( "10 'identifier' \"string1\" \\string2\\" );

        TokenParser tokenParser = new TokenParser( r );

        Number number = tokenParser.readNumeric();
        String identifier = tokenParser.readIdentifier();
        String string1 = tokenParser.readString();
        String string2 = tokenParser.readString();

        Assert.assertEquals( 10, number.intValue() );
        Assert.assertEquals( identifier, "identifier" );
        Assert.assertEquals( string1, "string1" );
        Assert.assertEquals( string2, "string2" );


    }
}
