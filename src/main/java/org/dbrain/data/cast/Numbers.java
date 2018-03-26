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

package org.dbrain.data.cast;

import org.dbrain.data.DataTruncationException;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.text.NumberFormat;
import java.text.ParsePosition;
import java.util.function.Function;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 11/04/13
 * Time: 9:23 PM
 * To change this template use File | Settings | File Templates.
 */
public class Numbers {

    /**
     * Convert a string to a number using a NumberFormat.
     */
    public static Function<String, Number> numberParser( final NumberFormat df ) {

        return new Function<String, Number>() {
            @Override
            public Number apply( String s ) {
                if ( s == null ) {
                    return null;
                } else synchronized ( this ) {
                    ParsePosition pp = new ParsePosition( 0 );
                    Number result = df.parse( s, pp );
                    if ( pp.getIndex() != s.length() ) {
                        throw new DataTruncationException();
                    }
                    return result;
                }
            }
        };

    }
}
