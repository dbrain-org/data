package epic.data;

import epic.data.util.Objects;

import java.util.function.Function;

/**
 * Denote a container of fields accessible by names.
 */
public interface NamedFields {
    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    Object get( String fieldName );

    default <T> T getAs( String fieldName, Function<Object, T> function ) {
        return function.apply( get( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Byte getByte( String fieldName ) {
        return Objects.toByte( get( fieldName ) );
    }

    default <T> T getByteAs( String fieldName, Function<? super Byte, T> function ) {
        return function.apply( getByte( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Short getShort( String fieldName ) {
        return Objects.toShort( get( fieldName ) );
    }

    default <T> T getShortAs( String fieldName, Function<? super Short, T> function ){
        return function.apply( getShort( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Integer getInt( String fieldName ) {
        return Objects.toInteger( get( fieldName ) );
    }

    default <T> T getIntAs( String fieldName, Function<? super Integer, T> function ) {
        return function.apply( getInt( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Long getLong( String fieldName ) {
        return Objects.toLong( get( fieldName ) );
    }

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getLongAs( String fieldName, Function<? super Long, T> function ) {
        return function.apply( getLong( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Float getFloat( String fieldName ) {
        return Objects.toFloat( get( fieldName ) );
    }

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getFloatAs( String fieldName, Function<? super Float, T> function ) {
        return function.apply( getFloat( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Double getDouble( String fieldName ) {
        return Objects.toDouble( get( fieldName ) );
    }

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getDoubleAs( String fieldName, Function<? super Double, T> function ) {
        return function.apply( getDouble( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default Boolean getBoolean( String fieldName ) {
        return Objects.toBoolean( get( fieldName ) );
    }

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getBooleanAs( String fieldName, Function<? super Boolean, T> function ) {
        return function.apply( getBoolean( fieldName ) );
    }

    /**
     * Read the field having the specified name.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default String getString( String fieldName ) {
        return Objects.toString( get( fieldName ) );
    }

    /**
     * Read the field having the specified name. Use the adapter to transform the object prior to return.
     *
     * @param fieldName The field name, non-null.
     * @return The field value.
     * @throws IllegalArgumentException if the field with the specified name does not exists.
     */
    default <T> T getStringAs( String fieldName, Function<? super String, T> function ) {
        return function.apply( getString( fieldName ) );
    }
}
