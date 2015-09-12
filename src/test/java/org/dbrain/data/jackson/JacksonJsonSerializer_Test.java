package org.dbrain.data.jackson;

import org.dbrain.data.Fqn;
import org.dbrain.data.Path;
import org.dbrain.data.Serializer;
import org.junit.Assert;
import org.junit.Test;

/**
 * Created by epoitras on 12/09/15.
 */
public class JacksonJsonSerializer_Test {

    private Serializer s = new JacksonJsonSerializer();

    @Test
    public void testPathSerializer() throws Exception {

        Path p1 = Path.of( "a123[45]" );
        String str = s.objectToString( p1 );
        Path p2 = s.parseObject( str, Path.class );

        Assert.assertEquals( p1, p2 );
    }

    @Test
    public void testPathSerializerNull() throws Exception {

        Path p1 = null;
        String str = s.objectToString( p1 );
        Path p2 = s.parseObject( str, Path.class );

        Assert.assertEquals( p1, p2 );
    }

    @Test
    public void testFqnSerializer() throws Exception {

        Fqn p1 = Fqn.of( "a123.a45" );
        String str = s.objectToString( p1 );
        Fqn p2 = s.parseObject( str, Fqn.class );

        Assert.assertEquals( p1, p2 );
    }

    @Test
    public void testFqnSerializerNull() throws Exception {

        Fqn p1 = null;
        String str = s.objectToString( p1 );
        Fqn p2 = s.parseObject( str, Fqn.class );

        Assert.assertEquals( p1, p2 );
    }


}