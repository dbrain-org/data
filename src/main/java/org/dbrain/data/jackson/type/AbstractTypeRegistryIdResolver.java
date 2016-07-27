package org.dbrain.data.jackson.type;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.DatabindContext;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.jsontype.TypeIdResolver;
import com.fasterxml.jackson.databind.type.TypeFactory;
import org.dbrain.data.TypeRegistry;

/**
 * Abstract class to implement concrete custom type id resolver.
 *
 * This class cannot be used directly since you must provide some way of injecting the type registry. By default, jackson will search for a no parameter constructor and won't find it.
 *
 * @see com.fasterxml.jackson.databind.cfg.HandlerInstantiator as a way of customizing the class creation.
 *
 */
public abstract class AbstractTypeRegistryIdResolver implements TypeIdResolver {

	protected abstract TypeRegistry getTypeRegistry();


	@Override
	public void init( JavaType baseType ) {
		if ( !getTypeRegistry().getBaseClass().isAssignableFrom( baseType.getRawClass() ) ) {
			throw new IllegalArgumentException( "Type " + baseType + " must be a subtype of " + getTypeRegistry().getBaseClass().getName() );
		}
	}

	@Override
	public JsonTypeInfo.Id getMechanism() {
		return JsonTypeInfo.Id.CUSTOM;
	}

	@Override
	public String idFromValue( Object obj ) {
		return idFromValueAndType( obj, obj.getClass() );
	}

	@Override
	public String idFromBaseType() {
		String result = getTypeRegistry().getNameByType( getTypeRegistry().getBaseClass() );
		if ( result == null ) {
			throw new IllegalStateException( getTypeRegistry().getBaseClass().getName() + " not registred in type registry." );
		}
		return result;
	}

	@Override
	public String idFromValueAndType( Object obj, Class<?> clazz ) {
		String result = getTypeRegistry().getNameByType( clazz );
		if ( result == null ) {
			throw new IllegalStateException( clazz.getName() + " not registred in type registry." );
		}
		return result;
	}

	@Override
	public JavaType typeFromId( String id ) {
		Class<?> result = getTypeRegistry().getTypeByName( id );
		if ( result == null ) {
			throw new IllegalStateException( id + " not registred in type registry." );
		}
		return TypeFactory.defaultInstance().constructType( result );
	}

	@Override
	public JavaType typeFromId( DatabindContext context, String id ) {
		Class<?> result = getTypeRegistry().getTypeByName( id );
		if ( result == null ) {
			throw new IllegalStateException( id + " not registred in type registry." );
		}
		return context.getTypeFactory().constructType( getTypeRegistry().getTypeByName( id ) );
	}

}
