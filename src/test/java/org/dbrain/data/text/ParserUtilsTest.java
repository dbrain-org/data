/*
 * Copyright [2015] [Eric Poitras]
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

package org.dbrain.data.text;

import org.junit.Assert;
import org.junit.Test;

/**
 * Created by epoitras on 9/3/15.
 */
public class ParserUtilsTest {

    @Test
    public void testIsJavaIdentifier() throws Exception {
        Assert.assertTrue( ParserUtils.isJavaIdentifier( "a" ) );
        Assert.assertTrue( ParserUtils.isJavaIdentifier( "a1" ) );
        Assert.assertTrue( ParserUtils.isJavaIdentifier( "a1$2" ) );

        Assert.assertFalse( ParserUtils.isJavaIdentifier( null ) );
        Assert.assertFalse( ParserUtils.isJavaIdentifier( "" ) );
        Assert.assertFalse( ParserUtils.isJavaIdentifier( "1$2" ) );
        Assert.assertFalse( ParserUtils.isJavaIdentifier( " a1$2" ) );
        Assert.assertFalse( ParserUtils.isJavaIdentifier( "a1[]" ) );
        Assert.assertFalse( ParserUtils.isJavaIdentifier( "a1()" ) );
        Assert.assertFalse( ParserUtils.isJavaIdentifier( "a1{}" ) );


    }
}