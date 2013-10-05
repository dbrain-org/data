/*
 * Copyright [2013] [Eric Poitras]
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package epic.data.adapters;

import epic.data.Adapter;
import epic.data.DataTruncationException;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 20/03/13
 * Time: 5:43 PM
 * To change this template use File | Settings | File Templates.
 */
public class ObjectAdapters {

    /**
     * Adapter that apply Object to BigInteger.
     */
    public static final Adapter<Object, BigInteger> TO_BIG_INTEGER = new Adapter<Object, BigInteger>() {

        @Override
        public BigInteger apply( Object o ) {
            if ( o == null ) return null;
            if ( o instanceof BigInteger ) return (BigInteger) o;
            if ( o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong ) {
                return new BigInteger( o.toString() );
            } else if ( o instanceof Float || o instanceof Double ) {
                return new BigDecimal( ( (Number) o ).doubleValue() ).toBigInteger();
            } else if ( o instanceof BigDecimal ) {
                return ( (BigDecimal) o ).toBigInteger();
            } else if ( o instanceof CharSequence ) {
                return new BigInteger( o.toString().trim() );
            }
            throw new IllegalArgumentException( "Cannot cast " + o + " to BigInteger." );
        }

    };

    /**
     * Created with IntelliJ IDEA.
     * User: epoitras
     * Date: 07/12/12
     * Time: 8:10 AM
     * To change this template use File | Settings | File Templates.
     */
    public static final Adapter<Object, Double> TO_DOUBLE = new Adapter<Object, Double>() {

        @Override
        public Double apply( Object o ) {
            if ( o == null ) return null;
            if ( o instanceof Double ) return (Double) o;
            if ( o instanceof Number ) {
                return ( (Number) o ).doubleValue();
            } else if ( o instanceof CharSequence ) {
                return Double.parseDouble( o.toString().trim() );
            }
            throw new IllegalArgumentException( "Cannot cast " + o + " to Double." );
        }

    };

    /**
     * Created with IntelliJ IDEA.
     * User: epoitras
     * Date: 07/12/12
     * Time: 8:10 AM
     * To change this template use File | Settings | File Templates.
     */
    public static final Adapter<Object, Float> TO_FLOAT = new Adapter<Object, Float>() {

        @Override
        public Float apply( Object o ) {
            if ( o == null ) return null;
            if ( o instanceof Float ) return (Float) o;
            if ( o instanceof Number ) {
                return ( (Number) o ).floatValue();
            } else if ( o instanceof CharSequence ) {
                return Float.parseFloat( o.toString().trim() );
            }
            throw new IllegalArgumentException( "Cannot cast " + o + " to Float." );
        }

    };

    /**
     * Created with IntelliJ IDEA.
     * User: epoitras
     * Date: 07/12/12
     * Time: 8:08 AM
     * To change this template use File | Settings | File Templates.
     */
    public static final Adapter<Object, Long> TO_LONG = new Adapter<Object, Long>() {

        private BigDecimal BD_MIN = new BigDecimal( Long.MIN_VALUE );
        private BigDecimal BD_MAX = new BigDecimal( Long.MAX_VALUE );
        private BigInteger BI_MIN = new BigInteger( Long.toString( Long.MIN_VALUE ) );
        private BigInteger BI_MAX = new BigInteger( Long.toString( Long.MAX_VALUE ) );

        @Override
        public Long apply( Object o ) {
            if ( o == null ) return null;
            if ( o instanceof Long ) return (Long) o;
            if ( o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof AtomicInteger || o instanceof AtomicLong ) {
                return ( (Number) o ).longValue();
            } else if ( o instanceof Float || o instanceof Double ) {
                double doubleValue = ( (Number) o ).doubleValue();
                if ( doubleValue >= Long.MIN_VALUE && doubleValue <= Long.MAX_VALUE ) {
                    return (long) doubleValue;
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof BigDecimal ) {
                BigDecimal bigDecimal = (BigDecimal) o;
                if ( bigDecimal.compareTo( BD_MIN ) >= 0 && bigDecimal.compareTo( BD_MAX ) <= 0 ) {
                    return bigDecimal.longValue();
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof BigInteger ) {
                BigInteger bigInteger = (BigInteger) o;
                if ( bigInteger.compareTo( BI_MIN ) >= 0 && bigInteger.compareTo( BI_MAX ) <= 0 ) {
                    return bigInteger.longValue();
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof CharSequence ) {
                return Long.parseLong( o.toString().trim() );
            }
            throw new IllegalArgumentException( "Cannot cast " + o + " to Long." );
        }

    };

    /**
     * Created with IntelliJ IDEA.
     * User: epoitras
     * Date: 11/12/12
     * Time: 4:35 PM
     * To change this template use File | Settings | File Templates.
     */
    public static final Adapter<Object, String> TO_STRING = new Adapter<Object, String>() {

        @Override
        public String apply( Object o ) {
            return o != null ? o.toString() : null;
        }
    };

    /**
     * Adapter that apply Object to BigDecimal.
     */
    public static Adapter<Object, BigDecimal> TO_BIG_DECIMAL = new Adapter<Object, BigDecimal>() {

        @Override
        public BigDecimal apply( Object o ) {
            if ( o == null ) return null;
            if ( o instanceof BigDecimal ) return (BigDecimal) o;
            if ( o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong ) {
                return new BigDecimal( ( (Number) o ).longValue() );
            } else if ( o instanceof Float || o instanceof Double ) {
                return new BigDecimal( ( (Number) o ).doubleValue() );
            } else if ( o instanceof BigInteger ) {
                return new BigDecimal( (BigInteger) o );
            } else if ( o instanceof CharSequence ) {
                return new BigDecimal( o.toString().trim() );
            }
            throw new IllegalArgumentException( "Cannot cast " + o + " to BigDecimal." );
        }

    };

    /**
     * Created with IntelliJ IDEA.
     * User: epoitras
     * Date: 07/12/12
     * Time: 8:06 AM
     * To change this template use File | Settings | File Templates.
     */
    public static Adapter<Object, Boolean> TO_BOOLEAN = new Adapter<Object, Boolean>() {

        @Override
        public Boolean apply( Object o ) {
            if ( o == null ) return null;
            if ( o instanceof Boolean ) return (Boolean) o;
            if ( o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong ) {
                return ( (Number) o ).longValue() != 0;
            } else if ( o instanceof BigDecimal ) {
                return !o.equals( BigDecimal.ZERO );
            } else if ( o instanceof BigInteger ) {
                return !o.equals( BigInteger.ZERO );
            } else if ( o instanceof CharSequence ) {
                return Boolean.parseBoolean( o.toString().trim() );
            }
            throw new IllegalArgumentException( "Cannot cast " + o + " to Boolean." );
        }

    };

    /**
     * Created with IntelliJ IDEA.
     * User: epoitras
     * Date: 07/12/12
     * Time: 8:06 AM
     * To change this template use File | Settings | File Templates.
     */
    public static Adapter<Object, Byte> TO_BYTE = new Adapter<Object, Byte>() {

        private BigDecimal BD_MIN = new BigDecimal( Byte.MIN_VALUE );
        private BigDecimal BD_MAX = new BigDecimal( Byte.MAX_VALUE );
        private BigInteger BI_MIN = new BigInteger( Byte.toString( Byte.MIN_VALUE ) );
        private BigInteger BI_MAX = new BigInteger( Byte.toString( Byte.MAX_VALUE ) );

        @Override
        public Byte apply( Object o ) {
            if ( o == null ) return null;
            if ( o instanceof Byte ) return (Byte) o;
            if ( o instanceof Short || o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong ) {
                long longValue = ( (Number) o ).longValue();
                if ( longValue >= Byte.MIN_VALUE && longValue <= Byte.MAX_VALUE ) {
                    return (byte) longValue;
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof Float || o instanceof Double ) {
                double doubleValue = ( (Number) o ).doubleValue();
                if ( doubleValue >= Byte.MIN_VALUE && doubleValue <= Byte.MAX_VALUE ) {
                    return (byte) doubleValue;
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof BigDecimal ) {
                BigDecimal bigDecimal = (BigDecimal) o;
                if ( bigDecimal.compareTo( BD_MIN ) >= 0 && bigDecimal.compareTo( BD_MAX ) <= 0 ) {
                    return bigDecimal.byteValue();
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof BigInteger ) {
                BigInteger bigInteger = (BigInteger) o;
                if ( bigInteger.compareTo( BI_MIN ) >= 0 && bigInteger.compareTo( BI_MAX ) <= 0 ) {
                    return bigInteger.byteValue();
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof CharSequence ) {
                return Byte.parseByte( o.toString().trim() );
            }
            throw new IllegalArgumentException( "Cannot cast " + o + " to Byte." );
        }

    };

    /**
     * Created with IntelliJ IDEA.
     * User: epoitras
     * Date: 07/12/12
     * Time: 8:08 AM
     * To change this template use File | Settings | File Templates.
     */
    public static Adapter<Object, Integer> TO_INTEGER = new Adapter<Object, Integer>() {

        private BigDecimal BD_MIN = new BigDecimal( Integer.MIN_VALUE );
        private BigDecimal BD_MAX = new BigDecimal( Integer.MAX_VALUE );
        private BigInteger BI_MIN = new BigInteger( Integer.toString( Integer.MIN_VALUE ) );
        private BigInteger BI_MAX = new BigInteger( Integer.toString( Integer.MAX_VALUE ) );

        @Override
        public Integer apply( Object o ) {
            if ( o == null ) return null;
            if ( o instanceof Integer ) return (Integer) o;
            if ( o instanceof Byte || o instanceof Short || o instanceof AtomicInteger ) {
                return ( (Number) o ).intValue();
            } else if ( o instanceof Long || o instanceof AtomicLong ) {
                long longValue = ( (Number) o ).longValue();
                if ( longValue >= Integer.MIN_VALUE && longValue <= Integer.MAX_VALUE ) {
                    return (int) longValue;
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof Float || o instanceof Double ) {
                double doubleValue = ( (Number) o ).doubleValue();
                if ( doubleValue >= Integer.MIN_VALUE && doubleValue <= Integer.MAX_VALUE ) {
                    return (int) doubleValue;
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof BigDecimal ) {
                BigDecimal bigDecimal = (BigDecimal) o;
                if ( bigDecimal.compareTo( BD_MIN ) >= 0 && bigDecimal.compareTo( BD_MAX ) <= 0 ) {
                    return bigDecimal.intValue();
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof BigInteger ) {
                BigInteger bigInteger = (BigInteger) o;
                if ( bigInteger.compareTo( BI_MIN ) >= 0 && bigInteger.compareTo( BI_MAX ) <= 0 ) {
                    return bigInteger.intValue();
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof CharSequence ) {
                return Integer.parseInt( o.toString().trim() );
            }
            throw new IllegalArgumentException( "Cannot cast " + o + " to Integer." );
        }

    };

    /**
     * Created with IntelliJ IDEA.
     * User: epoitras
     * Date: 07/12/12
     * Time: 8:07 AM
     * To change this template use File | Settings | File Templates.
     */
    public static Adapter<Object, Short> TO_SHORT = new Adapter<Object, Short>() {

        private BigDecimal BD_MIN = new BigDecimal( Short.MIN_VALUE );
        private BigDecimal BD_MAX = new BigDecimal( Short.MAX_VALUE );
        private BigInteger BI_MIN = new BigInteger( Short.toString( Short.MIN_VALUE ) );
        private BigInteger BI_MAX = new BigInteger( Short.toString( Short.MAX_VALUE ) );

        @Override
        public Short apply( Object o ) {
            if ( o == null ) return null;
            if ( o instanceof Short ) return (Short) o;
            if ( o instanceof Byte || o instanceof Short ) {
                return ( (Number) o ).shortValue();
            } else if ( o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong ) {
                long longValue = ( (Number) o ).longValue();
                if ( longValue >= Short.MIN_VALUE && longValue <= Short.MAX_VALUE ) {
                    return (short) longValue;
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof Float || o instanceof Double ) {
                double doubleValue = ( (Number) o ).doubleValue();
                if ( doubleValue >= Short.MIN_VALUE && doubleValue <= Short.MAX_VALUE ) {
                    return (short) doubleValue;
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof BigDecimal ) {
                BigDecimal bigDecimal = (BigDecimal) o;
                if ( bigDecimal.compareTo( BD_MIN ) >= 0 && bigDecimal.compareTo( BD_MAX ) <= 0 ) {
                    return bigDecimal.shortValue();
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof BigInteger ) {
                BigInteger bigInteger = (BigInteger) o;
                if ( bigInteger.compareTo( BI_MIN ) >= 0 && bigInteger.compareTo( BI_MAX ) <= 0 ) {
                    return bigInteger.shortValue();
                } else {
                    throw new DataTruncationException();
                }
            } else if ( o instanceof CharSequence ) {
                return Short.parseShort( o.toString().trim() );
            }
            throw new IllegalArgumentException( "Cannot cast " + o + " to Short." );
        }

    };

}
