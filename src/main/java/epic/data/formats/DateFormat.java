package epic.data.formats;

import epic.data.Format;
import epic.data.FormatException;
import epic.data.ParseException;
import epic.data.util.Strings;

import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 16/04/13
 * Time: 8:09 AM
 * To change this template use File | Settings | File Templates.
 */
public class DateFormat implements Format<Date> {

    private final SimpleDateFormat dateFormat;

    public DateFormat( String pattern, Locale locale ) {
        dateFormat = new SimpleDateFormat( pattern, locale );
        dateFormat.setLenient( false );
    }

    @Override
    public synchronized String format( Date value ) throws FormatException {
        if ( value == null ) {
            return null;
        } else {
            return dateFormat.format( value );
        }
    }

    @Override
    public synchronized Date parse( String value ) throws ParseException {
        if ( Strings.isBlank( value ) ) {
            return null;
        } else {
            String trimmedValue = value.trim();

            ParsePosition pos = new ParsePosition( 0 );
            Date result = dateFormat.parse( trimmedValue, pos );
            if ( pos.getIndex() != trimmedValue.length() ) {
                int index = pos.getErrorIndex() >= 0 ? pos.getErrorIndex() : pos.getIndex();
                throw new ParseException( "Error parsing date [" + trimmedValue + "] at position " + index + "." );
            }
            return result;
        }
    }

}