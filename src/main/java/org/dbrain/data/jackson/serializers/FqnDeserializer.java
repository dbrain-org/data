package org.dbrain.data.jackson.serializers;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import org.dbrain.data.Fqn;

import java.io.IOException;

/**
 * Created by epoitras on 12/09/15.
 */
public class FqnDeserializer extends JsonDeserializer<Fqn> {

    @Override
    public Fqn deserialize( JsonParser jsonParser, DeserializationContext deserializationContext ) throws IOException, JsonProcessingException {
        if ( jsonParser.getCurrentToken() == JsonToken.VALUE_NULL ) {
            return null;
        } else if ( jsonParser.getCurrentToken() == JsonToken.VALUE_STRING ) {
            return Fqn.of( jsonParser.getText() );
        } else {
            throw deserializationContext.wrongTokenException( jsonParser, JsonToken.VALUE_STRING, "" );
        }
    }
}
