package epic.data.type;

import java.util.function.Function;

import epic.data.util.Numbers;
import epic.data.util.Functions;
import epic.data.formats.Formats;
import epic.data.Formatter;
import epic.data.util.Objects;

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
    private final Function<Object, BigDecimal> castFunction;

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
        castFunction = Functions.compose( Objects::toBigDecimal, Numbers.setDecimalParameters( precision, scale, roundingMode ) );
    }

    @Override
    public Formatter<? super BigDecimal> getDisplayFormatter() {
        return Formats.TO_STRING;
    }

    @Override
    public Comparator<? super BigDecimal> getComparator() {
        return NATURAL_ORDER;
    }

    @Override
    public Function<Object, ? extends BigDecimal> getCastFunction() {
        return castFunction;
    }

}
