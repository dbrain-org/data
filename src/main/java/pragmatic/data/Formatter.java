package pragmatic.data;

/**
 * Simple contract that provide a String representation from a T value.
 *
 * Implementation should nicely handle null values and should prefer to return a non-null representation
 * if it make sense.
 */
public interface Formatter<T> {

    String format( T value ) throws FormatException;

}
