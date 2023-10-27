/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.daffodil.lib.util;

import com.ibm.icu.text.DecimalFormat;
import com.ibm.icu.text.DecimalFormatSymbols;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FixedICUDecimalFormat {

    /**
     * This regex enables us to correct a problem in ICUs interpretation of the pattern in the case
     * where the pattern has no digits before the decimal points, such as "#.##"
     * so that there should be zero leading digits if the integer part is 0.
     * <p>
     *     See https://unicode-org.atlassian.net/browse/ICU-22558
     * <p>
     * It captures unescaped digits or '#' characters
     * preceding a non-escaped decimal point in an ICU4J pattern string.
     * <p>
     * The whole regex (without java escapes) is {@code (?<!\\)([0-9#]*)(?<!\\)(\.) }
     * <ul>
     * <li>{@code (?<!\\)}: Negative lookbehind to ensure the character is not preceded by a backslash.</li>
     * <li>{@code ([0-9#]*)}: Capture zero or more consecutive digits or '#' characters in group 1.</li>
     * <li>{@code (?<!\\)(\.)}: The negative lookbehind ensures that only a non-escaped decimal point
     * will be captured in group 2.</li>
     * </ul>
     *
     * With this pattern:
     * <ul>
     * <li>Group 1 will capture any preceding unescaped digits or '#' characters.</li>
     * <li>Group 2 will capture the non-escaped decimal point.</li>
     * </ul>
     *
     * The match will fail only if there is no non-escaped decimal point.
     *
     * Package private for unit testing.
     */
    static Pattern pattern = Pattern.compile("(?<!\\\\)([0-9#]*)(?<!\\\\)(\\.)");

    /**
     * Construct a DecimalFormat, but correct for the ICU mis-interpretation of "#." where it
     * mistakenly sets the minimum number of integer digits to 1, not 0.
     *
     * This is not fooled by things like "#5#.0" which is silly, but behaves like "0.0".
     * <p>
     * See https://unicode-org.atlassian.net/browse/ICU-22558
     * </p>
     *
     * @param patternString - an ICU pattern for a DecimalFormat, to be created and fixed (if needed)
     * @return a DecimalFormat properly initialized by the pattern.
     */
    public static DecimalFormat fromPattern(String patternString) {
        DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(); // default locale
        return fromPattern(patternString, symbols);
    }
    /**
     * Package private helper version that takes symbols so that you can define a locale.
     * This allows tests to not have to adjust for locale decimal points "." vs. ",",
     * by specifying a locale. But it isn't needed for regular usage.
     *
     * @param patternString - ICU pattern to fix (if needed)
     * @param symbols - used to specify locale (because decimal points can be "." or ","
     * @return a DecimalFormat properly initialized by the pattern.
     */
    static DecimalFormat fromPattern(String patternString, DecimalFormatSymbols symbols) {
        DecimalFormat decimalFormat = new DecimalFormat(patternString, symbols);
        Matcher matcher = pattern.matcher(patternString);
        if (matcher.find()) {
            // There IS a decimal point.
            String digits = matcher.group(1); // digits or # unescaped, before the decimal point
            assert(Objects.equals(".", matcher.group(2))); // 2nd group matches the decimal point itself
            long count = digits.chars().filter(Character::isDigit).count();
            if (count == 0) {
                // The decimal point is not preceded by any required digits.
                // This is the fix ICU is getting wrong.
                //
                // Note even when ICU fixes this, this code won't break. It's just
                // unnecessary.
                //
                //
                decimalFormat.setMinimumIntegerDigits(0);
            }
        }
        return decimalFormat;
    }
}
