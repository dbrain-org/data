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

package org.dbrain.data.util;

import junit.framework.Assert;
import org.dbrain.data.DataTruncationException;
import org.dbrain.data.cast.Numbers;
import org.junit.Test;

import java.text.DecimalFormat;
import java.util.Locale;
import java.util.function.Function;

/**
 * Test String to Numbers adapters.
 */
public class TestStringToNumberAdapter {

    @Test
    public void testSimpleAdapt() {

        Function<String, Number> dfa = Numbers.numberParser( DecimalFormat.getIntegerInstance( Locale.ENGLISH ) );
        Number n = dfa.apply( "124" );

        Assert.assertEquals( n.intValue(), 124 );
    }

    @Test( expected = DataTruncationException.class )
    public void testSimpleFailure() {
        Function<String, Number> dfa = Numbers.numberParser( DecimalFormat.getIntegerInstance( Locale.ENGLISH ) );
        Number n = dfa.apply( "NAN" );

        Assert.assertEquals( n.intValue(), 124 );

    }

}
