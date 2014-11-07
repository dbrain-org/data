/*
 * Copyright [2014] [Eric Poitras]
 *
 *     Licensed under the Apache License, Version 2.0 (the "License");
 *     you may not use this file except in compliance with the License.
 *     You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dbrain.data.text;

/**
 * Token class.
 *
 * Tokens represent elements of a text stream. The only special token is EOF that means there is no more
 * characters available to read from the stream.
 *
 *
 * @author Eric Poitras
 * @version 1.0
 */
public enum Token {

    /**
     * Occurs where there in no more caracter on the stream.
     */
    EOF,

    /**
     * Strings without quotes.
     */
    KEYWORD,

    /**
     * Identifier getValue.
     *
     *
     * 'StringInSingleQuote';
     */
    IDENTIFIER,

    /**
     * String getValue.
     *
     * <ul>
     * <li>"string in double quote";</li>
     * <li>\long multi and possibly multi-line string\</li>
     * </ul>
     */
    STRING,

    /**
     * NumericOps values.
     *
     * Examples :
     *
     * <ul>
     * <li>123.23 - Real number</li>
     * <li>123 - Integral number</li>
     * <li>0x123 - Hexadecimal number</li>
     * <li>0b10100011 - Binary number</li>
     * <li>123.23E-123 - Scientific notation number</li>
     * </ul>
     */
    NUMERIC,

    /**
     * Character : ","
     */
    SEP,

    /**
     * Character : ";"
     */
    EOS,

    /**
     * Character : "("
     */
    OPEN_PARENTESIS,

    /**
     * Character : "["
     */
    OPEN_BRACKET,

    /**
     * Character : "{"
     */
    OPEN_ALINEAS,

    /**
     * Character : ")"
     */
    CLOSE_PARENTESIS,

    /**
     * Character : "]"
     */
    CLOSE_BRACKET,

    /**
     * Character : "}"
     */
    CLOSE_ALINEAS,

    /**
     * Character : "+"
     */
    PLUS,

    /**
     * Character : "-"
     */
    MINUS,

    /**
     * Character : "*"
     */
    MULTIPLY,

    /**
     * Character : "/"
     */
    DIVIDE,

    /**
     * Character : "%"
     */
    MODULUS,

    /**
     * Character : "++"
     */
    INCREMENT,

    /**
     * Character : "--"
     */
    DECREMENT,

    /**
     * Character : "+="
     */
    PLUS_EQUAL,

    /**
     * Character : "-="
     */
    MINUS_EQUAL,

    /**
     * Character : "*="
     */
    MULTIPLY_EQUAL,

    /**
     * Character : "/="
     */
    DIVIDE_EQUAL,

    /**
     * Character : "%="
     */
    MODULUS_EQUAL,

    /**
     * Character : "&gt;"
     */
    GREATER,

    /**
     * Character : "&lt;"
     */
    LESSER,

    /**
     * Character : "="
     */
    EQUAL,

    /**
     * Character : "&gt;="
     */
    GREATER_EQUAL,

    /**
     * Character : "&lt;="
     */
    LESSER_EQUAL,

    /**
     * Character : "&lt;&gt;"
     */
    NOT_EQUAL,

    /**
     * Character : "&amp;"
     */
    BW_AND,

    /**
     * Character : "|"
     */
    BW_OR,

    /**
     * Character : "^"
     */
    BW_XOR,

    /**
     * Character : "~"
     */
    BW_NOT,

    /**
     * Character : "&amp;&amp;"
     */
    AND,

    /**
     * Character : "||"
     */
    OR,

    /**
     * Character : "^^"
     */
    XOR,

    /**
     * Character : "!"
     */
    NOT,

    /**
     * Character : "@"
     */
    AT,

    /**
     * Character : ":="
     */
    COLON_EQUAL,

    /**
     * Character : ":"
     */
    COLON,

    /**
     * Character : "."
     */
    DOT

}