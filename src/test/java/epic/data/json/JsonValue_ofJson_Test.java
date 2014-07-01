package epic.data.json;

import junit.framework.Assert;
import org.junit.Test;

import java.io.InputStreamReader;

/**
 * Created by epoitras on 30/06/14.
 */
public class JsonValue_ofJson_Test {


    @Test
    public void test_object_1() throws Exception {
        JsonMap map = JsonValue.ofJson( "{}" ).asMap();
        Assert.assertEquals( 0, map.size() );
    }

    @Test
    public void test_object_2() throws Exception {
        JsonMap map = JsonValue.ofJson( "{titi:test}" ).asMap();
        Assert.assertEquals( 1, map.size() );
        Assert.assertEquals( "test", map.getString( "titi" ) );

    }

    @Test
    public void test_array_1() throws Exception {
        JsonList list = JsonValue.ofJson( "[]" ).asList();
        Assert.assertEquals( 0, list.size() );
    }

    @Test
    public void test_array_2() throws Exception {
        JsonList list = JsonValue.ofJson( "[test, 123, true, null]" ).asList();
        Assert.assertEquals( 4, list.size() );
        Assert.assertEquals( "test", list.getString( 0 ) );
        Assert.assertEquals( 123.0, list.getDouble( 1 ) );
        Assert.assertTrue( list.getBoolean( 2 ) );
        Assert.assertNull( list.get( 3 ) );
    }

    @Test
    public void testPaseJsonFile() throws Exception {
        JsonValue value = JsonValue.ofJson( new JsonParser( new InputStreamReader( getClass().getResourceAsStream( "/SampleJson.json" )) ) );

    }
}
