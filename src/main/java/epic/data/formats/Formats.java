package epic.data.formats;

import epic.data.FormatException;
import epic.data.Formatter;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 10/04/13
 * Time: 9:58 PM
 * To change this template use File | Settings | File Templates.
 */
public class Formats {

    public static Formatter<Object> TO_STRING = new Formatter<Object>() {

        @Override
        public String format( Object value ) throws FormatException {
            return value.toString();
        }
    };

}
