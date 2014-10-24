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

package org.dbrain.data.type;

import junit.framework.Assert;
import org.dbrain.data.type.string.StringType;
import org.junit.Test;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 11/04/13
 * Time: 2:49 PM
 * To change this template use File | Settings | File Templates.
 */
public class TestStringType {

    @Test
    public void testTrimTrail() throws Exception {
        StringType trimTrail = new StringType( 0, false, StringType.TrimHandling.TRAILING, StringType.CaseHandling.NONE, StringType.BlankHandling.NONE );

        Assert.assertEquals( trimTrail.cast( "Test" ), "Test" );
        Assert.assertEquals( trimTrail.cast( "Test   " ), "Test" );
        Assert.assertEquals( trimTrail.cast( 123 ), "123" );

        Assert.assertTrue( trimTrail.equals( "Test", "Test  " ));
        Assert.assertTrue( trimTrail.equals( "Test   ", "Test  " ));
        Assert.assertTrue( trimTrail.equals( null, null ));
        Assert.assertTrue( trimTrail.in( "Test", null, 123, "Test  " ));
        Assert.assertFalse( trimTrail.in( "test", null, 123, "Test  " ));

        Assert.assertFalse( trimTrail.equals( " Test", "Test  " ));
        Assert.assertFalse( trimTrail.equals( null, "Test  " ));
        Assert.assertFalse( trimTrail.equals( " Test", null ));

        StringType trimTrailMaxSize = new StringType( 4, false, StringType.TrimHandling.TRAILING, StringType.CaseHandling.NONE, StringType.BlankHandling.NONE );

        Assert.assertEquals( trimTrailMaxSize.cast( "Test" ), "Test" );
        Assert.assertEquals( trimTrailMaxSize.cast( "Test   " ), "Test" );
        Assert.assertEquals( trimTrailMaxSize.cast( 123 ), "123" );

        Assert.assertTrue( trimTrailMaxSize.equals( "Test", "Test  " ));
        Assert.assertTrue( trimTrailMaxSize.equals( "Test   ", "Test  " ));
        Assert.assertTrue( trimTrailMaxSize.equals( null, null ));
        Assert.assertFalse( trimTrailMaxSize.equals( null, "Test  " ));
        Assert.assertFalse( trimTrailMaxSize.equals( "Test  ", null ));

    }



}
