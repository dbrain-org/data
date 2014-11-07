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

package org.dbrain.data.parsing;

import junit.framework.Assert;
import org.dbrain.data.text.TokenParser;
import org.junit.Test;

import java.io.Reader;
import java.io.StringReader;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 03/04/13
 * Time: 5:29 PM
 * To change this template use File | Settings | File Templates.
 */
public class TokenParserTest {

    @Test
    public void testTokenParserLitterals() throws Exception {

        Reader r = new StringReader( "10 'identifier' \"string1\" \\string2\\" );

        TokenParser tokenParser = new TokenParser( r );

        Number number = tokenParser.readNumeric();
        String identifier = tokenParser.readIdentifier();
        String string1 = tokenParser.readString();
        String string2 = tokenParser.readString();

        Assert.assertEquals( 10, number.intValue() );
        Assert.assertEquals( identifier, "identifier" );
        Assert.assertEquals( string1, "string1" );
        Assert.assertEquals( string2, "string2" );
    }

}
