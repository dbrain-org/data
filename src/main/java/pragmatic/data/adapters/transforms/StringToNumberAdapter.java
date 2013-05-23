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

package pragmatic.data.adapters.transforms;

import pragmatic.data.Adapter;
import pragmatic.data.adapters.DataTruncationException;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParsePosition;

/**
 * Wraps a NumberFormat into an adapter.
 */
public class StringToNumberAdapter extends DecimalFormat implements Adapter<String, Number> {

    private final NumberFormat df;

    public StringToNumberAdapter( NumberFormat df ) {
        this.df = df;
    }

    @Override
    public Number adapt( String s ) {
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
}
