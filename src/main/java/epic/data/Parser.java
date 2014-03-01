package epic.data;

/**
 * Simple contract that allows to convert a string representation to a T value.
 *
 * It is correct for an implementation to return a null value for a representation that makes sense.
 */
public interface Parser<T> {

    /**
     * Parse a string value and return an instance of value T.
     * @return T or null.
     * @throws ParseException If a problem occurs parsing the string value.
     */
    T parse( String value ) throws ParseException;

}
