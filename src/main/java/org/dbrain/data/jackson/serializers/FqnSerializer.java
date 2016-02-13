package org.dbrain.data.jackson.serializers;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.dbrain.data.Fqn;

import java.io.IOException;

/**
 * Created by epoitras on 12/09/15.
 */
public class FqnSerializer extends JsonSerializer<Fqn> {

    @Override
    public void serialize( Fqn fqn, JsonGenerator jsonGenerator, SerializerProvider serializerProvider ) throws IOException, JsonProcessingException {
        if ( fqn == null ) {
            jsonGenerator.writeNull();
        } else {
            jsonGenerator.writeString( fqn.toString() );
        }
    }

}
