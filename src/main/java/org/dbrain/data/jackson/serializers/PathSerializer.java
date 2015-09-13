package org.dbrain.data.jackson.serializers;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.dbrain.data.Path;
import org.dbrain.data.Value;

import java.io.IOException;

/**
 * Created by epoitras on 12/09/15.
 */
public class PathSerializer extends JsonSerializer<Path> {

    @Override
    public void serialize( Path path, JsonGenerator jsonGenerator, SerializerProvider serializerProvider ) throws IOException, JsonProcessingException {
        if ( path == null ) {
            jsonGenerator.writeNull();
        } else {
            jsonGenerator.writeString( path.toString() );
        }
    }

}
