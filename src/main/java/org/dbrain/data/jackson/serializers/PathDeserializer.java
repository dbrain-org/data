package org.dbrain.data.jackson.serializers;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import org.dbrain.data.Path;

import java.io.IOException;

/**
 * Created by epoitras on 12/09/15.
 */
public class PathDeserializer extends JsonDeserializer<Path> {

    @Override
    public Path deserialize( JsonParser jsonParser, DeserializationContext deserializationContext ) throws IOException, JsonProcessingException {
        if ( jsonParser.getCurrentToken() == JsonToken.VALUE_NULL ) {
            jsonParser.nextToken();
            return null;
        } else if ( jsonParser.getCurrentToken() == JsonToken.VALUE_STRING ) {
            Path result = Path.of( jsonParser.getText() );
            jsonParser.nextToken();
            return result;

        }
        return null;
    }
}
