package epic.data;

/**
 * Simple contract that allows to convert a string representation to a T value.
 *
 * It is correct for an implementation to return a null value for a representation that makes sense.
 */
public interface Parser<T> {

    T parse( String value ) throws ParseException;

}
