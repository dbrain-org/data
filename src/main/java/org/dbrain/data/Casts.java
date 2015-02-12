/*
 * Copyright [2014] [Eric Poitras]
 *
 *     Licensed under the Apache License, Version 2.0 (the "License");
 *     you may not use this file except in compliance with the License.
 *     You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dbrain.data;

import org.dbrain.data.text.ParseException;
import org.dbrain.data.util.Strings;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.DateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;

/**
 * Methods used to coerce values to specific types.
 */
public class Casts {

    private static Pattern NUMBER_PATTERN = Pattern.compile( "\\d+(\\.\\d+)?" );

    /**
     * @param o
     * @param <T>
     * @return
     */
    public static <T> T identity( T o ) {
        return o;
    }

    /**
     * Cast string to BigDecimal.
     */
    public static BigDecimal toBigDecimal( String o ) {
        if ( Strings.isBlank( o ) ) return null;
        return new BigDecimal( o.trim() );
    }

    /**
     * Cast object to BigDecimal.
     */
    public static BigDecimal toBigDecimal( Object o ) {
        if ( o == null ) return null;
        if ( o instanceof BigDecimal ) return (BigDecimal) o;
        if ( o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong ) {
            return new BigDecimal( ( (Number) o ).longValue() );
        } else if ( o instanceof Float || o instanceof Double ) {
            return new BigDecimal( ( (Number) o ).doubleValue() );
        } else if ( o instanceof BigInteger ) {
            return new BigDecimal( (BigInteger) o );
        } else if ( o instanceof CharSequence ) {
            return toBigDecimal( o.toString() );
        }
        throw new DataCoercionException( "Cannot cast " + o + " to BigDecimal." );
    }


    /**
     * Cast String to BigInteger.
     */
    public static BigInteger toBigInteger( String o ) {
        if ( Strings.isBlank( o ) ) return null;
        return new BigInteger( o.trim() );
    }

    /**
     * Cast object to BigInteger.
     */
    public static BigInteger toBigInteger( Object o ) {
        if ( o == null ) return null;
        if ( o instanceof BigInteger ) return (BigInteger) o;
        if ( o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong ) {
            return new BigInteger( o.toString() );
        }
        if ( o instanceof Float || o instanceof Double ) {
            return new BigDecimal( ( (Number) o ).doubleValue() ).toBigIntegerExact();
        }
        if ( o instanceof BigDecimal ) {
            return ( (BigDecimal) o ).toBigIntegerExact();
        }
        if ( o instanceof CharSequence ) {
            return toBigInteger( o.toString() );
        }
        throw new DataCoercionException( "Cannot cast " + o + " to BigInteger." );
    }

    /**
     * Cast String to double.
     */
    public static Double toDouble( String o ) {
        if ( Strings.isBlank( o ) ) {
            return null;
        }
        return Double.parseDouble( o.trim() );
    }

    /**
     * Cast object to Double.
     */
    public static Double toDouble( Object o ) {
        if ( o == null ) return null;
        if ( o instanceof Double ) return (Double) o;
        if ( o instanceof Number ) {
            return ( (Number) o ).doubleValue();
        } else if ( o instanceof CharSequence ) {
            return toDouble( o.toString() );
        }
        throw new DataCoercionException( "Cannot cast " + o + " to Double." );
    }

    /**
     * Cast String to float.
     */
    public static Float toFloat( String o ) {
        if ( Strings.isBlank( o ) ) {
            return null;
        }
        return Float.parseFloat( o.trim() );
    }


    /**
     * Cast object to Float.
     */
    public static Float toFloat( Object o ) {
        if ( o == null ) return null;
        if ( o instanceof Float ) return (Float) o;
        if ( o instanceof Number ) {
            return ( (Number) o ).floatValue();
        } else if ( o instanceof CharSequence ) {
            return toFloat( o.toString() );
        }
        throw new DataCoercionException( "Cannot cast " + o + " to Float." );
    }

    /**
     * Cast BigDecimal to Long.
     */
    public static Long toLong( BigDecimal bigDecimal ) {
        if ( bigDecimal == null ) {
            return null;
        }
        return bigDecimal.longValueExact();
    }


    /**
     * Cast object to long.
     */
    public static Long toLong( String o ) {
        if ( Strings.isBlank( o ) ) return null;
        return toLong( new BigDecimal( o.trim() ) );
    }

    /**
     * Cast object to long.
     */
    public static Long toLong( Object o ) {
        if ( o == null ) return null;
        if ( o instanceof Long ) return (Long) o;
        if ( o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof AtomicInteger || o instanceof AtomicLong ) {
            return ( (Number) o ).longValue();
        } else if ( o instanceof Float || o instanceof Double ) {
            return new BigDecimal( ( (Number) o ).doubleValue() ).longValueExact();
        } else if ( o instanceof BigDecimal ) {
            return toLong( (BigDecimal) o );
        } else if ( o instanceof BigInteger ) {
            BigInteger bigInteger = (BigInteger) o;
            return bigInteger.longValueExact();
        } else if ( o instanceof CharSequence ) {
            return toLong( o.toString() );
        }
        throw new DataCoercionException( "Cannot cast " + o + " to Long." );
    }

    /**
     * Cast BigDecimal to integer.
     */
    public static Integer toInteger( BigDecimal bigDecimal ) {
        if ( bigDecimal == null ) {
            return null;
        }
        return bigDecimal.intValueExact();
    }

    /**
     * Cast string to integer.
     */
    public static Integer toInteger( String o ) {
        if ( Strings.isBlank( o ) ) {
            return null;
        }
        return toInteger( new BigDecimal( o.trim() ) );
    }

    /**
     * Cast object to integer.
     */
    public static Integer toInteger( Object o ) {
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
            return new BigDecimal( ( (Number) o ).doubleValue() ).intValueExact();
        } else if ( o instanceof BigDecimal ) {
            return toInteger( (BigDecimal) o );
        } else if ( o instanceof BigInteger ) {
            BigInteger bigInteger = (BigInteger) o;
            return bigInteger.intValueExact();
        } else if ( o instanceof CharSequence ) {
            return toInteger( o.toString() );
        }
        throw new DataCoercionException( "Cannot cast " + o + " to Integer." );
    }

    /**
     * Cast BigDecimal to short.
     */
    public static Short toShort( BigDecimal bigDecimal ) {
        if ( bigDecimal == null ) {
            return null;
        }
        return bigDecimal.shortValueExact();
    }

    /**
     * cast string to short.
     */
    public static Short toShort( String o ) {
        if ( Strings.isBlank( o ) ) {
            return null;
        }
        return toShort( new BigDecimal( o.trim() ) );
    }

    /**
     * Cast object to Short.
     */
    public static Short toShort( Object o ) {
        if ( o == null ) return null;
        if ( o instanceof Short ) return (Short) o;
        if ( o instanceof Byte ) {
            return ( (Number) o ).shortValue();
        } else if ( o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong ) {
            long longValue = ( (Number) o ).longValue();
            if ( longValue >= Short.MIN_VALUE && longValue <= Short.MAX_VALUE ) {
                return (short) longValue;
            } else {
                throw new DataTruncationException();
            }
        } else if ( o instanceof Float || o instanceof Double ) {
            return new BigDecimal( ( (Number) o ).doubleValue() ).shortValueExact();
        } else if ( o instanceof BigDecimal ) {
            return toShort( (BigDecimal) o );
        } else if ( o instanceof BigInteger ) {
            BigInteger bigInteger = (BigInteger) o;
            return bigInteger.shortValueExact();
        } else if ( o instanceof CharSequence ) {
            return toShort( o.toString() );
        }
        throw new DataCoercionException( "Cannot cast " + o + " to Short." );
    }

    /**
     * Cast BigDecimal to byte.
     */
    public static Byte toByte( BigDecimal bigDecimal ) {
        if ( bigDecimal == null ) {
            return null;
        }
        return bigDecimal.byteValueExact();
    }

    /**
     * Cast string to byte.
     */
    public static Byte toByte( String o ) {
        if ( Strings.isBlank( o ) ) {
            return null;
        }
        return toByte( new BigDecimal( o.trim() ) );
    }

    /**
     * Cast object to byte.
     */
    public static Byte toByte( Object o ) {
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
            return new BigDecimal( ( (Number) o ).doubleValue() ).byteValueExact();
        } else if ( o instanceof BigDecimal ) {
            return toByte( (BigDecimal) o );
        } else if ( o instanceof BigInteger ) {
            BigInteger bigInteger = (BigInteger) o;
            return bigInteger.byteValueExact();
        } else if ( o instanceof CharSequence ) {
            return toByte( o.toString() );
        }
        throw new DataCoercionException( "Cannot cast " + o + " to Byte." );
    }

    /**
     * Cast object to String (null-safe).
     */
    public static String toString( Object o ) {
        return o != null ? o.toString() : null;
    }

    /**
     * Cast an enum to enum name, null safe.
     */
    public static String toEnumName( Enum e ) {
        return e != null ? e.name() : null;
    }

    /**
     * Cast a name to an enum. Null-safe.
     */
    public static <T extends Enum<T>> T toEnum( Class<T> enumClass, String name ) {
        return Strings.isBlank( name ) ? null : Enum.valueOf( enumClass, name.trim() );
    }

    /**
     * Cast string to boolean.
     */
    public static Boolean toBoolean( String o ) {
        if ( Strings.isBlank( o ) ) {
            return null;
        }
        o = o.trim();
        if ( NUMBER_PATTERN.matcher( o ).matches() ) {
            return !( Casts.toBigDecimal( o ).compareTo( BigDecimal.ZERO ) == 0 );
        }
        if ( Boolean.TRUE.toString().equalsIgnoreCase( o ) ) {
            return true;
        } else if ( Boolean.FALSE.toString().equalsIgnoreCase( o ) ) {
            return false;
        } else {
            throw new DataCoercionException( "Cannot cast '" + o + "' to boolean." );
        }
    }

    /**
     * Cast object to Boolean.
     */
    public static Boolean toBoolean( Object o ) {
        if ( o == null ) return null;
        if ( o instanceof Boolean ) return (Boolean) o;
        if ( o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof Long || o instanceof AtomicInteger || o instanceof AtomicLong ) {
            return ( (Number) o ).longValue() != 0;
        }
        if ( o instanceof Float || o instanceof Double ) {
            return ( (Number) o ).doubleValue() != 0;
        } else if ( o instanceof BigDecimal ) {
            return !( ( (BigDecimal) o ).compareTo( BigDecimal.ZERO ) == 0 );
        } else if ( o instanceof BigInteger ) {
            return !( ( (BigInteger) o ).compareTo( BigInteger.ZERO ) == 0 );
        } else if ( o instanceof CharSequence ) {
            return toBoolean( o.toString() );
        }
        throw new DataCoercionException( "Cannot cast " + o + " to Boolean." );
    }

    /**
     * Convert a date to LocalDate.
     */
    public static LocalDate toLocalDate( Date date, ZoneId zoneId ) {
        if ( date != null ) {
            if ( date instanceof java.sql.Date ) {
                return ( (java.sql.Date) date ).toLocalDate();
            } else if ( date instanceof java.sql.Time ) {
                throw new DataCoercionException( "Cannot case " + date + " to LocalDate." );
            } else {
                return date.toInstant().atZone( zoneId ).toLocalDate();
            }
        } else {
            return null;
        }
    }

    /**
     * Convert a date to local date using system's timezone.
     */
    public static LocalDate toLocalDate( Date date ) {
        return toLocalDate( date, ZoneId.systemDefault() );
    }

    public static java.util.Date toDate( DateFormat format, String value ) {
        if ( !Strings.isBlank( value ) ) {
            try {
                return format.parse( value.trim() );
            } catch ( java.text.ParseException e ) {
                throw new ParseException( e );
            }
        } else {
            return null;
        }
    }

    public static java.sql.Date toSqlDate( DateFormat format, String value ) {
        return toSqlDate( toDate( format, value ) );
    }

    /**
     * Cast date to sql date.
     */
    public static java.sql.Date toSqlDate( java.util.Date date ) {
        return date != null ? new java.sql.Date( date.getTime() ) : null;
    }

    /**
     * Cast date to sql date.
     */
    public static java.sql.Date toSqlDate( LocalDate date ) {
        return date != null ? java.sql.Date.valueOf( date ) : null;
    }


    /**
     * Cast date to sql time.
     */
    public static java.sql.Time toSqlTime( java.util.Date date ) {
        return date != null ? new java.sql.Time( date.getTime() ) : null;
    }

    /**
     * Cast string to sql time.
     */
    public static java.sql.Time toSqlTime( DateFormat format, String time ) {
        return toSqlTime( toDate( format, time ) );
    }

    /**
     * Cast date to sql timestamp.
     */
    public static java.sql.Timestamp toSqlTimestamp( java.util.Date date ) {
        return date != null ? new java.sql.Timestamp( date.getTime() ) : null;
    }

}
