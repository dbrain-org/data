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

package epic.data.util;

import junit.framework.Assert;
import org.junit.Test;
import java.util.function.Function;

import java.util.Objects;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 17/12/12
 * Time: 6:04 PM
 * To change this template use File | Settings | File Templates.
 */
public class TestCompose {

    @Test
    public void testCompose1() throws Exception {
        Function<String, String> slc = (s) -> Strings.maxLength( s, 10 );
        Function<Object, String> composed = Functions.compose( Objects::toString, Strings::trim, slc );
        Assert.assertEquals( composed.apply( new StringBuilder( "test  " ) ), "test" );
    }

}
