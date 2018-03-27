package org.dbrain.data.jackson;

import org.dbrain.data.Fqn;
import org.dbrain.data.Path;
import org.dbrain.data.TextSerializer;
import org.dbrain.data.tree.Node;
import org.dbrain.data.tree.NodeList;
import org.dbrain.data.tree.NodeMap;
import org.dbrain.data.text.ParseException;
import org.junit.Assert;
import org.junit.Test;

import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by epoitras on 12/09/15.
 */
public class JacksonSerializer_Test {

    private TextSerializer textSerializer = JacksonSerializer.newBuilder().build();


    @Test( expected = ParseException.class )
    public void testEmptyStream() throws Exception {
        Assert.assertNull( textSerializer.read( "", Node.class ) );
    }

    @Test
    public void test_object_1() throws Exception {
        NodeMap map = textSerializer.read( "{}", NodeMap.class );
        Assert.assertEquals( 0, map.size() );
    }

    @Test( expected = ParseException.class )
    public void test_object_fail_1() throws Exception {
        textSerializer.read( "{", NodeMap.class );
    }

    @Test( expected = ParseException.class )
    public void test_object_fail_2() throws Exception {
        textSerializer.read( "{} {", NodeMap.class );
    }

    @Test( expected = ParseException.class )
    public void test_object_fail_3() throws Exception {
        textSerializer.read( "{}  [", NodeMap.class );
    }

    @Test
    public void test_object_2() throws Exception {
        NodeMap map = textSerializer.read( "{ \"boolean_true\" : true, \"boolean_false\" : false, \"null\": null, \"string\": \"string\", \"double\":123.4, \"integer\":123456789,\"array\":[],\"object\":{} }", NodeMap.class );
        Assert.assertEquals( 8, map.size() );
        Assert.assertEquals( Boolean.TRUE.toString(), map.getString( "boolean_true" ) );
        Assert.assertEquals( Boolean.FALSE.toString(), map.getString( "boolean_false" ) );
        Assert.assertNull( map.getObject( "null" ) );
        Assert.assertEquals( "string", map.getString( "string" ) );
        Assert.assertEquals( new Double( 123.4D ), map.getDouble( "double" ) );
        Assert.assertEquals( new Integer( 123456789 ), map.getInt( "integer" ) );
        Assert.assertEquals( new ArrayList<>(), map.getObject( "array" ) );
        Assert.assertEquals( new HashMap<>(), map.getObject( "object" ) );
    }

    @Test
    public void test_array_1() throws Exception {
        NodeList list = textSerializer.read( "[]", NodeList.class );
        Assert.assertEquals( 0, list.size() );
    }

    @Test
    public void test_array_2() throws Exception {
        NodeList list = textSerializer.read( "[\"test\", 123, true, null]", NodeList.class );
        Assert.assertEquals( 4, list.size() );
        Assert.assertEquals( "test", list.getString( 0 ) );
        Assert.assertEquals( new Double( 123.0 ), list.getDouble( 1 ) );
        Assert.assertTrue( list.getBoolean( 2 ) );
        Assert.assertTrue( list.get( 3 ).isNull() );
    }

    @Test
    public void test_array_3() throws Exception {
        NodeList list = textSerializer.read( "[]", Node.class ).getList();
        Assert.assertEquals( 0, list.size() );
    }

    @Test
    public void test_array_4() throws Exception {
        NodeList list = textSerializer.read( "[\"test\", 123, true, null]", Node.class ).getList();
        Assert.assertEquals( 4, list.size() );
        Assert.assertEquals( "test", list.getString( 0 ) );
        Assert.assertEquals( new Double( 123.0 ), list.getDouble( 1 ) );
        Assert.assertTrue( list.getBoolean( 2 ) );
        Assert.assertTrue( list.get( 3 ).isNull() );
    }


    @Test
    public void testParseJsonFile() throws Exception {
        Node node = textSerializer
                .read( new InputStreamReader( getClass().getResourceAsStream( "/SampleJson.json" ) ), Node.class );
    }

    @Test
    public void testPathSerializer() throws Exception {

        Path p1 = Path.of( "a123[45]" );
        String str = textSerializer.writeToString( p1 );
        Path p2 = textSerializer.read( str, Path.class );

        Assert.assertEquals( p1, p2 );
    }

    @Test
    public void testPathSerializerNull() throws Exception {

        Path p1 = null;
        String str = textSerializer.writeToString( p1 );
        Path p2 = textSerializer.read( str, Path.class );

        Assert.assertEquals( p1, p2 );
    }

    @Test
    public void testFqnSerializer() throws Exception {

        Fqn p1 = Fqn.of( "a123.a45" );
        String str = textSerializer.writeToString( p1 );
        Fqn p2 = textSerializer.read( str, Fqn.class );

        Assert.assertEquals( p1, p2 );
    }

    @Test
    public void testFqnSerializerNull() throws Exception {

        Fqn p1 = null;
        String str = textSerializer.writeToString( p1 );
        Fqn p2 = textSerializer.read( str, Fqn.class );

        Assert.assertEquals( p1, p2 );
    }



}