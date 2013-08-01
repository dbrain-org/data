package epic.data.formats;

import epic.data.FormatException;
import epic.data.Formatter;

/**
 * Collection of basic format instances.
 */
public class Formats {

    /**
     * Formatter wrapper over the Object.toString().
     */
    public static Formatter<Object> TO_STRING = new Formatter<Object>() {

        @Override
        public String format( Object value ) throws FormatException {
            return value.toString();
        }
    };

}
