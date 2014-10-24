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

package org.dbrain.data.type.impl;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.util.NoSuchElementException;

/**
 * Base type for bean (pojo) values.
 *
 * @author PoitraE
 */
public class BeanType<E> extends EntityType<E, Operations.Entity<E>> {

    private RawOperations rawOps = new RawOperations();
    private Class<E> clazz;

    /**
     * Construct a new BeanType.
     *
     * @param clazz2 The bean clazz.
     */
    public BeanType( Class<E> clazz2 ) {
        this.clazz = clazz2;
    }

    @Override
    public Operations.Entity<E> getRawOperations() {
        return rawOps;
    }

    @Override
    public Class<E> getBaseClass() {
        return clazz;
    }

    @Override
    public E cast( Object value ) {
        if ( value == null ) return null;
        if ( clazz.isAssignableFrom( value.getClass() ) ) return clazz.cast( value );

        throw new TypeCastException( this, value );
    }

    public String toString( E value ) {
        return value.toString();
    }

    protected BeanInfo getBeanInfo( Object value ) {
        try {
            return java.beans.Introspector.getBeanInfo( value.getClass() );
        } catch ( IntrospectionException ex ) {
            throw new UnsupportedOperationException( ex );
        }
    }


    private class RawOperations implements Operations.Entity<E> {

        protected Object getProperty( Object value, PropertyDescriptor d ) {
            try {

                Method reader = d.getReadMethod();
                return reader.invoke( value );

            } catch ( Exception ex ) {
                throw new UnsupportedOperationException( ex );
            }
        }

        @Override
        public Object getProperty( E value, String propertyName ) {
            for ( PropertyDescriptor d : getBeanInfo( value ).getPropertyDescriptors() ) {
                if ( d.getName().equals( propertyName ) ) return getProperty( value, d );
            }
            throw new NoSuchElementException( propertyName );
        }

        @Override
        public Operations.LValue getPropertyLValue( E value, String propertyName ) {
            for ( PropertyDescriptor d : getBeanInfo( value ).getPropertyDescriptors() ) {
                if ( d.getName().equals( propertyName ) ) return new TdBeanLValue( value, d );
            }
            throw new NoSuchElementException( propertyName );
        }

    }

    /**
     * LValue implementation over a bean's property
     */
    private static class TdBeanLValue implements Operations.LValue {

        private Object             bean;
        private PropertyDescriptor property;

        public TdBeanLValue( Object context, PropertyDescriptor d ) {
            this.bean = context;
            this.property = d;
        }

        @Override
        public void setValue( Object value ) {
            try {

                //  Method writer = property.getWriteMethod();
                // RootType<?> rootType = TypeUtil.getTypeDefFromClass( property.getPropertyType() );
                // writer.invoke( bean, rootType.cast( value ) );

            } catch ( Exception ex ) {
                throw new UnsupportedOperationException( ex );
            }
        }

    }

}
