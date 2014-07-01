package epic.data.json;

import epic.data.Casts;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.function.Function;

/**
 * Created by epoitras on 26/06/14.
 */
public abstract class JsonValue {

    private static final JsonValue TRUE  = new JsonBoolean( Boolean.TRUE );
    private static final JsonValue FALSE = new JsonBoolean( Boolean.FALSE );


    public static JsonValue of( String s ) {
        return s != null ? new JsonString( s ) : null;
    }

    public static JsonValue of( Double d ) {
        return d != null ? new JsonDouble( d ) : null;
    }

    public static JsonValue of( Boolean b ) {
        if ( b != null ) {
            return b ? TRUE : FALSE;
        } else {
            return null;
        }
    }

    public static JsonValue ofJson( String jsonString ) {
        return ofJson( new JsonParser( new StringReader( jsonString ) ), true );
    }

    public static JsonValue ofJson( JsonParser parser ) {
        return ofJson( parser, true );
    }

    public static JsonValue ofJson( JsonParser parser, boolean validateEof ) {
        if ( parser.getToken() != null ) {
            JsonValue result;
            switch ( parser.getToken() ) {
                case STRING:
                    result = of( parser.readString() );
                    break;
                case DOUBLE:
                    result = of( parser.readDouble() );
                    break;
                case BOOLEAN:
                    result = of( parser.readBoolean() );
                    break;
                case NULL:
                    parser.readToken( JsonParser.Token.NULL );
                    result = null;
                    break;
                case OPEN_OBJECT: {
                    parser.readToken( JsonParser.Token.OPEN_OBJECT );
                    HashMap<String, JsonValue> values = new HashMap<>();
                    while ( parser.getToken() != JsonParser.Token.CLOSE_OBJECT ) {
                        // Skip commas.
                        while ( parser.getToken() == JsonParser.Token.COMMA ) {
                            parser.readToken( JsonParser.Token.COMMA );
                        }
                        if ( parser.getToken() != JsonParser.Token.CLOSE_OBJECT ) {
                            String key = parser.readString();
                            parser.readToken( JsonParser.Token.COLON );
                            JsonValue value = ofJson( parser, false );
                            values.put( key, value );
                        }
                    }
                    parser.readToken( JsonParser.Token.CLOSE_OBJECT );
                    result = new JsonMap( values );
                }
                break;
                case OPEN_ARRAY: {
                    parser.readToken( JsonParser.Token.OPEN_ARRAY );
                    ArrayList<JsonValue> values = new ArrayList<>();
                    while ( parser.getToken() != JsonParser.Token.CLOSE_ARRAY ) {
                        // Skip commas.
                        while ( parser.getToken() == JsonParser.Token.COMMA ) {
                            parser.readToken( JsonParser.Token.COMMA );
                        }
                        if ( parser.getToken() != JsonParser.Token.CLOSE_ARRAY ) {
                            values.add( ofJson( parser, false ) );
                        }
                    }
                    parser.readToken( JsonParser.Token.CLOSE_ARRAY );
                    result = new JsonList( values );
                }
                break;
                default:
                    throw parser.error( "Expected value" );
            }
            if ( validateEof ) {
                if ( parser.getToken() != null ) {
                    throw parser.error( "Expected end of file." );
                }
            }

            return result;
        } else {
            return null;
        }

    }

    public abstract String asString();

    public abstract Double asDouble();

    public abstract Boolean asBoolean();

    public abstract JsonMap asMap();

    public abstract JsonList asList();

    /**
     * Wrap a string value.
     */
    public static final class JsonString extends JsonValue {

        private String value;

        private JsonString( String value ) {
            this.value = value;
        }

        public String get() {
            return value;
        }

        public <T> T getAs( Function<String, T> f ) {
            return f.apply( get() );
        }

        @Override
        public String asString() {
            return this.value;
        }

        @Override
        public Double asDouble() {
            return Casts.toDouble( value );
        }

        @Override
        public Boolean asBoolean() {
            return Casts.toBoolean( value );
        }

        @Override
        public JsonMap asMap() {
            throw new UnsupportedOperationException();
        }

        @Override
        public JsonList asList() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * Wrap a double value.
     */
    public static final class JsonDouble extends JsonValue {

        private final Double value;

        private JsonDouble( Double value ) {
            this.value = value;
        }
        @Override
        public String asString() {
            return value.toString();
        }

        @Override
        public Double asDouble() {
            return value;
        }

        @Override
        public Boolean asBoolean() {
            throw new UnsupportedOperationException();
        }

        @Override
        public JsonMap asMap() {
            throw new UnsupportedOperationException();
        }

        @Override
        public JsonList asList() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * Wrap a boolean value;
     */
    public static final class JsonBoolean extends JsonValue {

        private final Boolean value;

        private JsonBoolean( Boolean value ) {
            this.value = value;
        }

        public Boolean get() {
            return value;
        }

        @Override
        public String asString() {
            return value.toString();
        }

        @Override
        public Double asDouble() {
            throw new UnsupportedOperationException();
        }

        @Override
        public Boolean asBoolean() {
            return value;
        }

        @Override
        public JsonMap asMap() {
            throw new UnsupportedOperationException();
        }

        @Override
        public JsonList asList() {
            throw new UnsupportedOperationException();
        }
    }

}
