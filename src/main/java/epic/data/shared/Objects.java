package epic.data.shared;

import java.io.ByteArrayOutputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.security.MessageDigest;

/**
 * Created with IntelliJ IDEA.
 * User: epoitras
 * Date: 11/04/13
 * Time: 8:46 PM
 * To change this template use File | Settings | File Templates.
 */
public class Objects {


    /**
     * Retrieve a hash getValue for the provided Serializable type. The hashing algorithm is provided
     * by the hashType parameter and can be anything supported by the java security api.
     *
     * @param o        The object to serialize.
     * @param hashType The type of hash requested.
     * @return the hash getValue.
     */
    static public byte[] hashSerializable( Serializable o, String hashType ) {
        try {

            ByteArrayOutputStream buffer = new ByteArrayOutputStream();
            ObjectOutputStream out = new ObjectOutputStream( buffer );
            out.writeObject( o );
            out.close();
            buffer.close();

            byte[] digest = MessageDigest.getInstance( hashType ).digest( buffer.toByteArray() );

            return digest;

        } catch ( Exception e ) {
            throw new RuntimeException( e );
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
    static public String hashSerializableAsString( Serializable o, String hashType ) {
        byte[] hash = hashSerializable( o, hashType );
        StringBuilder sb = new StringBuilder();
        for ( byte b : hash ) {
            int hex1 = (int) ( ( b >>> 4 ) & 0xf );
            int hex2 = (int) ( b & 0xf );
            sb.append( Integer.toHexString( hex1 ) );
            sb.append( Integer.toHexString( hex2 ) );
        }
        return sb.toString();
    }
}
