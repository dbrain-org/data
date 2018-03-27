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

package org.dbrain.data.cast;

import junit.framework.Assert;
import org.dbrain.data.DataCoercionException;
import org.junit.Test;

import java.time.LocalDate;
import java.util.Date;

/**
 * Test coercions of dates.
 */
public class Casts_toLocalDate_Test {

    @Test
    public void testDate() throws Exception {
        // Ugliest API ever made:
        Date date1 = new Date( 2014 - 1900, 0, 1 );
        LocalDate date2 = LocalDates.toLocalDate( date1 );
        Assert.assertEquals( LocalDate.of( 2014, 1, 1 ), date2 );
    }

    @Test
    public void testDateNull() throws Exception {
        LocalDate date2 = LocalDates.toLocalDate( null );
        Assert.assertNull( date2 );
    }

    /**
     * This used to throw an exception in the first version.
     */
    @Test
    public void testSqlDate() throws Exception {
        // Ugliest API ever made:
        Date date1 = new java.sql.Date( 2014 - 1900, 0, 1 );
        LocalDate date2 = LocalDates.toLocalDate( date1 );
        Assert.assertEquals( LocalDate.of( 2014, 1, 1 ), date2 );
    }

    /**
     * From timestamp.
     */
    @Test
    public void testSqlTimestamp() throws Exception {
        // Ugliest API ever made:
        Date date1 = new java.sql.Timestamp( 2014 - 1900, 0, 1, 0, 0, 0, 0 );
        LocalDate date2 = LocalDates.toLocalDate( date1 );
        Assert.assertEquals( LocalDate.of( 2014, 1, 1 ), date2 );
    }

    /**
     * Should not cast.
     */
    @Test( expected = DataCoercionException.class )
    public void testSqlTime() throws Exception {
        // Ugliest API ever made:
        Date date1 = new java.sql.Time( 0, 0, 0 );
        LocalDates.toLocalDate( date1 );
    }

}
