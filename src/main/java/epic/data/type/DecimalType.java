package epic.data.type;

import epic.data.Adapter;
import epic.data.adapters.Adapters;
import epic.data.adapters.NumberAdapters;
import epic.data.adapters.ObjectAdapters;
import epic.data.formats.ObjectFormats;
import epic.data.Formatter;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Comparator;

/**
 * Decimal data type.
 */
public class DecimalType extends AbstractDataType<BigDecimal> {

    private static final Comparator<BigDecimal> NATURAL_ORDER = new Comparator<BigDecimal>() {
        @Override
        public int compare( BigDecimal o1, BigDecimal o2 ) {
            // Handle Null values
            if ( o1 == null ) {
                if ( o2 == null ) return 0;
                else return -1;
            }
            if ( o2 == null ) return 1;

            return o1.compareTo( o2 );
        }
    };
    private final Adapter<Object, BigDecimal> castAdapter;

    /**
     * Create a new numeric type.
     */
    public DecimalType( Integer scale ) {
        this( null, scale, RoundingMode.HALF_UP );

    }

    public DecimalType( Integer precision, Integer scale ) {
        this( precision, scale, RoundingMode.HALF_UP );
    }

    public DecimalType( Integer precision, Integer scale, RoundingMode roundingMode ) {
        castAdapter = Adapters.combine( ObjectAdapters.TO_BIG_DECIMAL, NumberAdapters.setDecimalParameters( precision, scale, roundingMode ) );
    }

    @Override
    public Formatter<? super BigDecimal> getDisplayFormatter() {
        return ObjectFormats.TO_STRING;
    }

    @Override
    public Comparator<? super BigDecimal> getComparator() {
        return NATURAL_ORDER;
    }

    @Override
    public Adapter<Object, ? extends BigDecimal> getCastAdapter() {
        return castAdapter;
    }

}
