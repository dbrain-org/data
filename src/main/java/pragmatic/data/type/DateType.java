package pragmatic.data.type;

import java.util.Date;

/**
 * Date datatype.
 */
public interface DateType<T> extends DataType<T> {

    Date toDate();

}
