package epic.data;

import java.util.function.Function;

/**
 * Denote a container of fields accessible by indexes.
 */
public interface IndexedFields {

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    Object get( int fieldIndex );

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getAs( int fieldIndex, Function<Object, T> function ) {
        return function.apply( get( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Byte getByte( int fieldIndex ) {
        return Casts.toByte( get( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getByteAs( int fieldIndex, Function<? super Byte, T> function ) {
        return function.apply( getByte( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Short getShort( int fieldIndex ) {
        return Casts.toShort( get( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getShortAs( int fieldIndex, Function<? super Short, T> function ) {
        return function.apply( getShort( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Integer getInt( int fieldIndex ) {
        return Casts.toInteger( get( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getIntAs( int fieldIndex, Function<? super Integer, T> function ) {
        return function.apply( getInt( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Long getLong( int fieldIndex ) {
        return Casts.toLong( get( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getLongAs( int fieldIndex, Function<? super Long, T> function ) {
        return function.apply( getLong( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Float getFloat( int fieldIndex ) {
        return Casts.toFloat( get( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getFloatAs( int fieldIndex, Function<? super Float, T> function ) {
        return function.apply( getFloat( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Double getDouble( int fieldIndex ) {
        return Casts.toDouble( get( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getDoubleAs( int fieldIndex, Function<? super Double, T> function ) {
        return function.apply( getDouble( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default Boolean getBoolean( int fieldIndex ) {
        return Casts.toBoolean( get( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getBooleanAs( int fieldIndex, Function<? super Boolean, T> function ) {
        return function.apply( getBoolean( fieldIndex ) );
    }

    /**
     * Read the field at the specified index.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default String getString( int fieldIndex ) {
        return Casts.toString( get( fieldIndex ) );
    }

    /**
     * Read the field at the specified index. Use the adapter to transform the object prior to return.
     *
     * @param fieldIndex 0-based field index.
     * @return The field value.
     * @throws IndexOutOfBoundsException if the index is out of bound.
     */
    default <T> T getStringAs( int fieldIndex, Function<? super String, T> function ) {
        return function.apply( getString( fieldIndex ) );
    }
}
