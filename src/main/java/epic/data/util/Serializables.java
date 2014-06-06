package epic.data.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * Created by epoitras on 03/06/14.
 */
public class Serializables {


    /**
     * Retrieve a hash getValue for the provided Serializable type. The hashing algorithm is provided
     * by the hashType parameter and can be anything supported by the java security api.
     *
     * @param o        The object to serialize.
     * @param hashType The type of hash requested.
     * @return the hash getValue.
     */
    public static byte[] hashSerializable( Serializable o, String hashType ) {
        try ( ByteArrayOutputStream buffer = new ByteArrayOutputStream();
                ObjectOutputStream out = new ObjectOutputStream( buffer ); ) {

            out.writeObject( o );
            out.close();
            buffer.close();

            byte[] digest = MessageDigest.getInstance( hashType ).digest( buffer.toByteArray() );

            return digest;

        } catch ( IOException e ) {
            throw new IllegalStateException( e );
        } catch ( NoSuchAlgorithmException e ) {
            throw new IllegalArgumentException( e );
        }
    }

    /**
     * Retrieve a hash getValue for the provided Serializable type. The hashing algorithm is provided
     * by the hashType parameter and can be anything supported by the java security api. The hash
     * binary buffer is converted to string using an hexadecimal encoding.
     *
     * @param o
     * @param hashType
     * @return
     */
    public static String hashSerializableAsString( Serializable o, String hashType ) {
        byte[] hash = hashSerializable( o, hashType );
        StringBuilder sb = new StringBuilder();
        for ( byte b : hash ) {
            int hex1 = ( ( b >>> 4 ) & 0xf );
            int hex2 = ( b & 0xf );
            sb.append( Integer.toHexString( hex1 ) );
            sb.append( Integer.toHexString( hex2 ) );
        }
        return sb.toString();
    }

    /**
     * Deep clone using serializable interface.
     *
     * @param o The objcet to clone, or null.
     * @return The cloned object, or null if null was provided.
     */
    public static <T extends Serializable> T clone( T o ) {

        // return null if null is provided.
        if ( o == null ) {
            return null;
        }

        try ( ByteArrayOutputStream rawOut = new ByteArrayOutputStream(); ObjectOutputStream out = new ObjectOutputStream( rawOut ) ) {

            out.writeObject( o );
            out.close();
            rawOut.close();

            try ( ByteArrayInputStream rawIn = new ByteArrayInputStream( rawOut.toByteArray() ); ObjectInputStream in = new ObjectInputStream( rawIn ) ) {
                return (T) in.readObject();
            }

        } catch ( ClassNotFoundException | IOException e ) {
            throw new IllegalStateException( e );
        }

    }
}
