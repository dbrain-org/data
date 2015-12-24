package org.dbrain.data.jackson.type;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.DeserializationConfig;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.SerializationConfig;
import com.fasterxml.jackson.databind.jsontype.NamedType;
import com.fasterxml.jackson.databind.jsontype.TypeDeserializer;
import com.fasterxml.jackson.databind.jsontype.TypeIdResolver;
import com.fasterxml.jackson.databind.jsontype.TypeResolverBuilder;
import com.fasterxml.jackson.databind.jsontype.TypeSerializer;
import com.fasterxml.jackson.databind.jsontype.impl.AsArrayTypeDeserializer;
import com.fasterxml.jackson.databind.jsontype.impl.AsArrayTypeSerializer;
import com.fasterxml.jackson.databind.jsontype.impl.AsExistingPropertyTypeSerializer;
import com.fasterxml.jackson.databind.jsontype.impl.AsExternalTypeDeserializer;
import com.fasterxml.jackson.databind.jsontype.impl.AsExternalTypeSerializer;
import com.fasterxml.jackson.databind.jsontype.impl.AsPropertyTypeDeserializer;
import com.fasterxml.jackson.databind.jsontype.impl.AsPropertyTypeSerializer;
import com.fasterxml.jackson.databind.jsontype.impl.AsWrapperTypeDeserializer;
import com.fasterxml.jackson.databind.jsontype.impl.AsWrapperTypeSerializer;
import com.fasterxml.jackson.databind.jsontype.impl.StdTypeResolverBuilder;
import org.dbrain.data.TypeRegistry;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Allows to override type resolving for some sub-trees defined by type registries.
 */
public class TypeRegistryResolver implements TypeResolverBuilder<TypeRegistryResolver> {

	/**
	 * Start from empty builder
	 */
	public static Builder newBuilder() {
		return new Builder();
	}

	/**
	 * Fast track to build a resolver from a registry. Type info is inserted as a property.
	 *
	 * @param propertyName The name of the property.
	 */
	public static TypeRegistryResolver of( TypeRegistry registry, String propertyName ) {
		return newBuilder().with( registry, propertyName ).build();
	}


	private final TypeResolverBuilder delegate;
	private final List<Entry> entries;


	private TypeRegistryResolver( TypeResolverBuilder delegate, List<Entry> entries ) {
		this.delegate = delegate;
		this.entries = entries;
	}

	@Override
	public Class<?> getDefaultImpl() {
		return delegate.getDefaultImpl();
	}

	@Override
	public TypeSerializer buildTypeSerializer( SerializationConfig config, JavaType baseType, Collection<NamedType> subtypes ) {
		Entry entry = lookupRegistry( baseType.getRawClass() );
		if ( entry != null ) {
			TypeRegistryIdResolver idRes = new TypeRegistryIdResolver( entry.getRegistry() );
			switch ( entry.getInsertAs() ) {
				case WRAPPER_ARRAY:
					return new AsArrayTypeSerializer( idRes, null );
				case PROPERTY:
					return new AsPropertyTypeSerializer( idRes, null, entry.getName() );
				case WRAPPER_OBJECT:
					return new AsWrapperTypeSerializer( idRes, null );
				case EXTERNAL_PROPERTY:
					return new AsExternalTypeSerializer( idRes, null, entry.getName() );
				case EXISTING_PROPERTY:
					return new AsExistingPropertyTypeSerializer( idRes, null, entry.getName() );
				default:
					throw new IllegalStateException();
			}
		} else {
			return delegate.buildTypeSerializer( config, baseType, subtypes );
		}
	}

	@Override
	public TypeDeserializer buildTypeDeserializer( DeserializationConfig config, JavaType baseType, Collection<NamedType> subtypes ) {
		Entry entry = lookupRegistry( baseType.getRawClass() );
		if ( entry != null ) {
			TypeRegistryIdResolver idRes = new TypeRegistryIdResolver( entry.getRegistry() );
			// First, method for converting type info to type id:
			switch ( entry.getInsertAs() ) {
				case WRAPPER_ARRAY:
					return new AsArrayTypeDeserializer( baseType, idRes, entry.getName(), entry.isVisible(), getDefaultImpl() );
				case PROPERTY:
				case EXISTING_PROPERTY: // as per [#528] same class as PROPERTY
					return new AsPropertyTypeDeserializer( baseType, idRes, entry.getName(), entry.isVisible(), getDefaultImpl(), entry.getInsertAs() );
				case WRAPPER_OBJECT:
					return new AsWrapperTypeDeserializer( baseType, idRes, entry.getName(), entry.isVisible(), getDefaultImpl() );
				case EXTERNAL_PROPERTY:
					return new AsExternalTypeDeserializer( baseType, idRes, entry.getName(), entry.isVisible(), getDefaultImpl() );
				default:
					throw new IllegalStateException();
			}
		} else {
			return delegate.buildTypeDeserializer( config, baseType, subtypes );
		}
	}

	@Override
	public TypeRegistryResolver init( JsonTypeInfo.Id idType, TypeIdResolver res ) {
		delegate.init( idType, res );
		return this;
	}

	@Override
	public TypeRegistryResolver inclusion( JsonTypeInfo.As includeAs ) {
		delegate.inclusion( includeAs );
		return this;
	}

	@Override
	public TypeRegistryResolver typeProperty( String propName ) {
		delegate.typeProperty( propName );
		return this;
	}

	@Override
	public TypeRegistryResolver defaultImpl( Class<?> defaultImpl ) {
		delegate.defaultImpl( defaultImpl );
		return this;
	}

	@Override
	public TypeRegistryResolver typeIdVisibility( boolean isVisible ) {
		delegate.typeIdVisibility( isVisible );
		return this;
	}


	public Entry lookupRegistry( Class clazz ) {
		Entry result = null;
		for ( Entry r : entries ) {
			if ( r.getRegistry().getBaseClass().isAssignableFrom( clazz ) ) {
				if ( result == null ) {
					result = r;
				} else {
					// Is current result more generic ? If so, override with the most specific.
					if ( result.getRegistry().getBaseClass().isAssignableFrom( r.getRegistry().getBaseClass() ) ) {
						result = r;
					}
				}
			}
		}
		return result;
	}

	/**
	 * Describe an entry to override typing.
	 */
	private static class Entry {

		private TypeRegistry registry;

		private JsonTypeInfo.As insertAs;

		private String name;

		private boolean visible;

		public Entry( TypeRegistry registry, JsonTypeInfo.As insertAs, String name, boolean visible ) {
			this.registry = registry;
			this.insertAs = insertAs;
			this.name = name;
			this.visible = visible;
		}

		public TypeRegistry getRegistry() {
			return registry;
		}

		public JsonTypeInfo.As getInsertAs() {
			return insertAs;
		}

		public String getName() {
			return name;
		}

		public boolean isVisible() {
			return visible;
		}
	}

	/**
	 * Builder class.
	 */
	public static class Builder {

		private TypeResolverBuilder delegate;
		private List<Entry> entries = new ArrayList<>();

		public Builder withDelegate( TypeResolverBuilder<?> delegate ) {
			this.delegate = delegate;
			return this;
		}

		public Builder with( TypeRegistry registry, String propertyName ) {
			return with( registry, JsonTypeInfo.As.PROPERTY, propertyName );
		}

		public Builder with( TypeRegistry registry, JsonTypeInfo.As includeAs, String propertyName ) {
			return with( registry, includeAs, propertyName, false );
		}

		public Builder with( TypeRegistry registry, JsonTypeInfo.As includeAs, String propertyName, boolean visible ) {
			entries.add( new Entry( registry, includeAs, propertyName, visible ) );
			return this;
		}


		public TypeRegistryResolver build() {
			TypeResolverBuilder finalDelegate = delegate != null ? delegate : StdTypeResolverBuilder.noTypeInfoBuilder();
			return new TypeRegistryResolver( finalDelegate, entries );
		}

	}



}
