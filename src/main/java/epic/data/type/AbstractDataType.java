package epic.data.type;

/**
 * Abstract base class to implements data types.
 */
public abstract class AbstractDataType<T> implements DataType<T> {

    @Override
    public final int compare( Object o1, Object o2 ) {
        if ( o1 == o2 ) {
            return 0;
        }
        return getComparator().compare( cast( o1 ), cast( o2 ) );
    }

    @Override
    public final boolean equals( Object value1, Object value2 ) {
        if ( value1 == value2 ) {
            return true;
        }
        return getComparator().compare( cast( value1 ), cast( value2 ) ) == 0;
    }

    @Override
    public final boolean in( Object o1, Object... vn ) {
        T v1 = cast( o1 );
        for ( Object value : vn ) {
            if ( getComparator().compare( v1, cast( value ) ) == 0 ) {
                return true;
            }
        }
        return false;
    }

    @Override
    public final T cast( Object value ) {
        return getCastAdapter().adapt( value );
    }

    @Override
    public String toString( Object value ) {
        return getDisplayFormatter().format( cast( value ) );
    }
}
