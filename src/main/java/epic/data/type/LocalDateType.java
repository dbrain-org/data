package epic.data.type;

import java.util.function.Function;
import epic.data.Formatter;

import java.util.Comparator;
import java.util.Date;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 16/04/13
 * Time: 8:05 AM
 * To change this template use File | Settings | File Templates.
 */
public class LocalDateType implements DateType<Date> {


    @Override
    public Date toDate() {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public Formatter<? super Date> getDisplayFormatter() {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public Comparator<? super Date> getComparator() {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public Function<Object, ? extends Date> getCastFunction() {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public boolean equals( Object value1, Object value2 ) {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public boolean in( Object valueToFind, Object... values ) {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public Date cast( Object value ) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public String toString( Object value ) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public int compare( Object o1, Object o2 ) {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
