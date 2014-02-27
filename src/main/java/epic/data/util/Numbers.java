package epic.data.util;

import java.util.function.Function;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 11/04/13
 * Time: 9:23 PM
 * To change this template use File | Settings | File Templates.
 */
public class Numbers {

    public static Function<BigDecimal, BigDecimal> setDecimalParameters( final int precision, final int scale, final RoundingMode rounding ) {
        return new Function<BigDecimal, BigDecimal>() {

            final MathContext mathContext = new MathContext( precision, rounding );

            @Override
            public BigDecimal apply( BigDecimal bigDecimal ) {
                if ( bigDecimal == null ) {
                    return null;
                }
                return bigDecimal.setScale( scale, mathContext.getRoundingMode() );
            }
        };
    }


}
