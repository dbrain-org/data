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

/**
 * Methods used to cast values to specific types.
 *
 * Unless noted, those functions handle null fluently so you can call func1(func2(func3(x))) without
 * worrying that x might be null.
 *
 */
public class SqlTimestamps {

    /**
     * Cast date to sql timestamp.
     */
    public static java.sql.Timestamp toSqlTimestamp( java.util.Date date ) {
        return date != null ? new java.sql.Timestamp( date.getTime() ) : null;
    }

}
