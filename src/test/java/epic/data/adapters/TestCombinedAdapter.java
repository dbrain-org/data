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

import junit.framework.Assert;
import org.junit.Test;
import epic.data.Adapter;
import epic.data.adapters.transforms.StringToNumberAdapter;

import java.text.DecimalFormat;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 17/12/12
 * Time: 6:04 PM
 * To change this template use File | Settings | File Templates.
 */
public class TestCombinedAdapter {

    @Test
    public void testCombine2() throws Exception {
        Adapter<String, String> slc = StringAdapters.maxLength( 10 );
        Adapter<Object, String> combined = Adapters.combine( ObjectAdapters.STRING, StringAdapters.TRIM, slc );

        Assert.assertEquals( combined.adapt( new StringBuilder("test  " ) ), "test" );
    }

    public void testString2Int() {
        Adapter<Object, Integer> o2i = ObjectAdapters.INTEGER;
        StringToNumberAdapter s2n = new StringToNumberAdapter( new DecimalFormat( "####" ) );

        Adapter<String, Integer> s2i = Adapters.combine( s2n, o2i );



    }
}
