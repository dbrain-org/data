package epic.data.adapters;

import epic.data.Adapter;

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
public class NumberAdapters {

    public static Adapter<BigDecimal, BigDecimal> setDecimalParameters( final int precision, final int scale, final RoundingMode rounding ) {
        return new Adapter<BigDecimal, BigDecimal>() {

            final MathContext mathContext = new MathContext( precision, rounding );

            @Override
            public BigDecimal adapt( BigDecimal bigDecimal ) {
                return null;  //To change body of implemented methods use File | Settings | File Templates.
            }
        };
    }


}
