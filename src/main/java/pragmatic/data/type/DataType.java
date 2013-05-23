package pragmatic.data.type;

import pragmatic.data.Adapter;
import pragmatic.data.Formatter;

import java.util.Comparator;

/**
 * Hi-level datatype definition for domain types.
 */
public interface DataType<T> extends Comparator<Object> {

    Formatter<? super T> getDisplayFormatter();

    Comparator<? super T> getComparator();

    Adapter<Object, ? extends T> getCastAdapter();

    boolean equals( Object value1, Object value2 );

    boolean in ( Object valueToFind, Object ... values );

    T cast( Object value );

    String toString( Object value );

}
