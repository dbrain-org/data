package epic.data.type.string;

import epic.data.Formatter;
import epic.data.type.AbstractDataType;
import epic.data.util.Functions;
import epic.data.formats.Formats;
import epic.data.util.Objects;
import epic.data.util.Strings;

import java.util.Comparator;
import java.util.Locale;
import java.util.function.Function;

/**
 * String data type.
 *
 * Goals: Implements feature found in SQL varchar.
 * <p>
 * - Maximum length, or zero for none.
 * - Case sensitivity.
 * - Trimming leading spaces, trailing spaces, or none.
 * - Case handling.
 * - Collation and language support.
 * - Graceful null support with possibility to map blanks values to null or vice-versa.
 */
public class StringType extends AbstractDataType<String> {

    private Comparator<String>       comparator;
    private Function<Object, String> castFunction;

    /**
     * Create a new StringType object. - This type support case sensitivity. -
     * This type support trailing spaces handling.
     */
    public StringType( int maxLength, boolean caseSensitive, TrimHandling trimHandling, CaseHandling caseHandling, BlankHandling blankHandling ) {

        comparator = caseSensitive ? String.CASE_INSENSITIVE_ORDER : Strings.NATURAL_ORDER;

        // First trim the string.
        Function<String, String> trimFunction;
        switch ( trimHandling ) {
            case BOTH:
                trimFunction = Strings::trim;
                break;
            case LEADING:
                trimFunction = Strings::ltrim;
                break;
            case TRAILING:
                trimFunction = Strings::rtrim;
                break;
            default:
                trimFunction = null;
        }

        // Adapter to check the length.
        Function<String, String> lengthFunction;
        if ( maxLength > 0 ) {
            lengthFunction = ( s ) -> Strings.maxLength( s, maxLength );
        } else {
            lengthFunction = null;
        }

        Function<String, String> caseFunction;
        switch ( caseHandling ) {
            case LOWERCASE:
                caseFunction = ( s ) -> Strings.lowercase( s, Locale.getDefault() );
                break;
            case UPPERCASE:
                caseFunction = ( s ) -> Strings.uppercase( s, Locale.getDefault() );
                break;
            default:
                caseFunction = null;
        }

        // Adapter to check the blank
        Function<String, String> blankFunction = null;
        switch ( blankHandling ) {
            case BLANK_IS_NULL:
                blankFunction = Strings::nullToBlank;
                break;
        }

        // Combine them
        Function<String, String> combinedFunction = Functions.composeAlike( trimFunction, lengthFunction, caseFunction, blankFunction );
        if ( combinedFunction != null ) {
            castFunction = Functions.compose( Objects::toString, combinedFunction );
        } else {
            castFunction = Objects::toString;
        }

    }


    @Override
    public Formatter<? super String> getDisplayFormatter() {
        return Formats.TO_STRING;
    }

    @Override
    public Comparator<String> getComparator() {
        return comparator;
    }

    @Override
    public Function<Object, String> getCastFunction() {
        return castFunction;
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
