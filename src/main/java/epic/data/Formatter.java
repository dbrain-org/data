package epic.data;

/**
 * Simple contract that provide a String representation from a T value.
 *
 * Implementation should nicely handle null values and should prefer to return a non-null representation
 * if it make sense.
 */
public interface Formatter<T> {

    /**
     * @return A string representation for the value T.
     * @throws FormatException In case there is a problem with formatting.
     */
    String format( T value ) throws FormatException;

}
