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

package org.dbrain.data;

import org.junit.Assert;
import org.junit.Test;

/**
 * Created by epoitras on 10/27/15.
 */
public class TypeRegistry_Test {

    TypeRegistry r = TypeRegistry.newInstance( c -> c.getSimpleName() )
                                 .registerType( StringBuilder.class )
                                 .registerType( StringBuffer.class )
                                 .build();

    TypeRegistry n = TypeRegistry.newInstance( Number.class, c -> c.getSimpleName() )
                                 .registerType( Long.class )
                                 .registerType( Double.class )
                                 .build();


    @Test
    public void testGetClass() throws Exception {
        Assert.assertEquals( r.getClass( StringBuffer.class.getSimpleName() ), StringBuffer.class );
        Assert.assertEquals( r.getClass( StringBuilder.class.getSimpleName() ), StringBuilder.class );
        Assert.assertNull( r.getClass( String.class.getSimpleName() ) );
    }

    @Test
    public void testGetName() throws Exception {
        Assert.assertEquals( r.getName( StringBuffer.class ), StringBuffer.class.getSimpleName() );
        Assert.assertEquals( r.getName( StringBuilder.class ), StringBuilder.class.getSimpleName() );
        Assert.assertNull( r.getName( String.class ) );
    }

    @Test
    public void testBaseClass() throws Exception {
        Assert.assertEquals( r.getBaseClass(), Object.class );
        Assert.assertEquals( n.getBaseClass(), Number.class );
    }

    @Test( expected = IllegalStateException.class )
    public void testIllegalClass() throws Exception {
        TypeRegistry.newInstance( Number.class, p -> p.getSimpleName() ).registerType( String.class ).build();
    }

    @Test( expected = IllegalStateException.class )
    public void testDupClass() throws Exception {
        TypeRegistry.newInstance( p -> p.getSimpleName() )
                    .registerType( Path.class )
                    .registerType( java.nio.file.Path.class )
                    .build();
    }

    @Test
    public void testTwiceSameClassOk() throws Exception {
        TypeRegistry.newInstance( p -> p.getSimpleName() )
                    .registerType( Path.class )
                    .registerType( Path.class )
                    .build();
    }


}