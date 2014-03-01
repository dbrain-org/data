package epic.data.type;

import epic.data.type.string.StringType;
import junit.framework.Assert;
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
