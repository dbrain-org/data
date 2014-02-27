package epic.data.type;

import epic.data.Formatter;

import java.util.Comparator;
import java.util.function.Function;

/**
 * Hi-level datatype definition for domain types.
 */
public interface DataType<T> extends Comparator<Object> {

    Formatter<? super T> getDisplayFormatter();

    Comparator<? super T> getComparator();

    Function<Object, ? extends T> getCastFunction();

    boolean equals( Object value1, Object value2 );

    boolean in ( Object valueToFind, Object ... values );

    T cast( Object value );

    String toString( Object value );

}
