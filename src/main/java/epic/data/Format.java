package epic.data;

/**
 * A format can produce a textual representation of an entity and also, create an entity from a textual representation.
 *
 * It is required that the value produces by the method format can be parsed by the parse method.
 *
 */
public interface Format<T> extends Formatter<T>, Parser<T> {



}
