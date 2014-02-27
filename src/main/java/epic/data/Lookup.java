package epic.data;

import java.util.Set;
import java.util.function.Function;

/**
 * Lookup table to transform an element of type FROM to an element of type TO.
 */
public interface Lookup<FROM, TO> extends Function<FROM, TO> {

    /**
     * List possible values this lookup can map.
     */
    public Set<FROM> keySet();

}
