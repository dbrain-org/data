package org.dbrain.data.jackson.type;

import org.dbrain.data.TypeRegistry;

/**
 * Concrete implementation of TypeRegistryIdResolver.
 */
public class TypeRegistryIdResolver extends AbstractTypeRegistryIdResolver {

	protected final TypeRegistry typeRegistry;

	public TypeRegistryIdResolver( TypeRegistry typeRegistry ) {
		this.typeRegistry = typeRegistry;
	}

	@Override
	protected TypeRegistry getTypeRegistry() {
		return typeRegistry;
	}

	@Override
	public String getDescForKnownTypeIds() {
		return getClass().getName();
	}
}
