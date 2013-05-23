package pragmatic.data.formats;

import junit.framework.Assert;
import org.junit.Test;
import pragmatic.data.ParseException;

import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 17/04/13
 * Time: 4:35 PM
 * To change this template use File | Settings | File Templates.
 */
public class DateFormatTest {

    /**
     * Parse a simple value and check the result.b
     */
    @Test
    public void testParse() throws Exception {
        DateFormat df = new DateFormat( "yyyy-MM-DD", Locale.ENGLISH );

        Date testDate = df.parse( "2012-01-01" );

        Calendar testCalendar = Calendar.getInstance( Locale.ENGLISH );
        testCalendar.set( 2012, Calendar.JANUARY, 01, 0, 0, 0 );
        testCalendar.clear( Calendar.MILLISECOND );

        Assert.assertEquals( testDate, testCalendar.getTime() );

    }

    /**
     * Do not expect the parser to be lenient.
     */
    @Test(expected = ParseException.class )
    public void testParseNonLenient() throws Exception {
        DateFormat df = new DateFormat( "yyyy-MM-DD", Locale.ENGLISH );
        Date testDate = df.parse( "2012-01-32" );
    }

    @Test
    public void testFormatParse() throws Exception {
        DateFormat df = new DateFormat( "yyyy-MM-DD", Locale.ENGLISH );
        Date testDate = df.parse( "2012-01-01" );
        String testDateString = df.format( testDate );

        Assert.assertEquals( testDateString, "2012-01-01" );


    }
}
