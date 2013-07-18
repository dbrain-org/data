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

package epic.data.type.impl;

import epic.data.type.DecimalType;
import epic.data.type.StringType;

import java.math.RoundingMode;

/**
 * This class contains standard types definitions.
 *
 * @author poitrae
 */
public class Types {

    /**
     * Standard string.
     * <p/>
     * <code>"bob" == "bob"</code>
     */
    public static StringType STRING = new StringType( 0, true, StringType.TrimHandling.NONE, StringType.CaseHandling.NONE, StringType.BlankHandling.NONE );

    /**
     * Standard field string == Case insensitive trimmed string.
     */
    public static StringType FSTRING = new StringType( 0, false, StringType.TrimHandling.BOTH, StringType.CaseHandling.NONE, StringType.BlankHandling.BLANK_IS_NULL );

    /**
     * Case insensitive string with trimmed trailing-spaces.
     * <p/>
     * <code>"bob  " == "BOB"</code>
     */
    public static StringType DBSTRING = new StringType( 0, false, StringType.TrimHandling.TRAILING, StringType.CaseHandling.NONE, StringType.BlankHandling.NONE );

    /**
     * Standard Integer.
     */
    public static final IntegerType INTEGER = new IntegerType();

    /**
     * Standard Long.
     */
    public static final LongType LONG = new LongType();

    /**
     * Standard Decimal type with infinite precision and a HALF_UP rounding mode.
     */
    public static final DecimalType DECIMAL = new DecimalType( null, null, RoundingMode.HALF_UP );

    /**
     * Standard date type.
     */
    public static final DateType DATE = new DateType();

    /**
     * ISO date type.
     */
    public static final DateType ISO_DATE = new DateType( "yyyy-MM-dd" );

    /**
     * Standard Time stamp type
     */
    public static final TdTimeStamp TIMESTAMP = new TdTimeStamp( "yyyy-MM-dd HH:mm:ss" );

}
