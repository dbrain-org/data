package epic.data.type;

import epic.data.formats.ObjectFormats;
import epic.data.shared.Strings;
import epic.data.Adapter;
import epic.data.adapters.Adapters;
import epic.data.adapters.ObjectAdapters;
import epic.data.adapters.StringAdapters;
import epic.data.Formatter;

import java.util.Comparator;
import java.util.Locale;

/**
 * String data type. Implements feature found in SQL varchar.
 *
 * - Maximum length, or zero for none.
 * - Case sensitivity.
 * - Trimming leading spaces, trailing spaces, or none.
 * - Case handling.
 * - Collation and language support.
 * - Graceful null support with possibility to map blanks values to null or vice-versa.
 *
 */
public class StringType extends AbstractDataType<String> {

    private Comparator<String> nativeComparator;
    private Adapter<Object, String> castAdapter;

    /**
     * Create a new StringType object. - This type support case sensitivity. -
     * This type support trailing spaces handling.
     */
    public StringType( int maxLength, boolean caseSensitive, TrimHandling trimHandling, CaseHandling caseHandling, BlankHandling blankHandling ) {

        nativeComparator = caseSensitive ? String.CASE_INSENSITIVE_ORDER : Strings.NATURAL_ORDER;

        // First trim the string.
        Adapter<String, String> trimAdapter;
        switch( trimHandling ) {
            case BOTH:
                trimAdapter = StringAdapters.TRIM;
                break;
            case LEADING:
                trimAdapter = StringAdapters.LTRIM;
                break;
            case TRAILING:
                trimAdapter = StringAdapters.RTRIM;
                break;
            default:
                trimAdapter = null;
        }

        // Adapter to check the length.
        Adapter<String, String> lengthAdapter;
        if ( maxLength > 0 ) {
            lengthAdapter = StringAdapters.maxLength( maxLength );
        } else {
            lengthAdapter = null;
        }

        Adapter<String, String> caseAdapter;
        switch ( caseHandling ) {
            case LOWERCASE:
                caseAdapter = StringAdapters.lowercase( Locale.getDefault() );
                break;
            case UPPERCASE:
                caseAdapter = StringAdapters.uppercase( Locale.getDefault() );
                break;
            default:
                caseAdapter = null;
        }

        // Adapter to check the blank
        Adapter<String, String> blankAdapter = null;
        switch ( blankHandling ) {
            case BLANK_IS_NULL:
                blankAdapter = StringAdapters.NULL_TO_BLANK;
                break;
        }

        // Combine them
        Adapter<String, String> combinedAdapter = Adapters.combineAlike( trimAdapter, lengthAdapter, caseAdapter, blankAdapter );
        if ( combinedAdapter != null ) {
            castAdapter = Adapters.combine( ObjectAdapters.TO_STRING, combinedAdapter );
        } else {
            castAdapter = ObjectAdapters.TO_STRING;
        }

    }


    @Override
    public Formatter<? super String> getDisplayFormatter() {
        return ObjectFormats.TO_STRING;
    }

    @Override
    public Comparator<String> getComparator() {
        return nativeComparator;
    }

    @Override
    public Adapter<Object, String> getCastAdapter() {
        return castAdapter;
    }

    /**
     * Enumeration about the possible way to handle blank values.
     *
     * @author PoitraE
     */
    public static enum BlankHandling {

        /**
         * No process is performed.
         */
        NONE,

        /**
         * Blank values are considered null.
         */
        BLANK_IS_NULL

    }

    /**
     * Enumaration about the possible way to handle string case.
     *
     * @author PoitraE
     */
    public static enum CaseHandling {

        /**
         * Case is left untouched.
         */
        NONE,

        /**
         * All letters are converted to uppercase.
         */
        UPPERCASE,

        /**
         * All letters are converted to lowercase.
         */
        LOWERCASE

    }

    /**
     * Enumaration about the possible way to handle string case.
     *
     * @author PoitraE
     */
    public static enum TrimHandling {

        /**
         * Case is left untouched.
         */
        NONE,

        /**
         * Trim leading spaces
         */
        LEADING,

        /**
         * Trim trailing spaces
         */
        TRAILING,

        /**
         * Trim both leading and trailing spaces
         */
        BOTH

    }
}
