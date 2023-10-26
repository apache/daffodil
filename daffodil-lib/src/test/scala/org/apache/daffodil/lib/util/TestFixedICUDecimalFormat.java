package org.apache.daffodil.lib.util;

import com.ibm.icu.text.DecimalFormat;
import com.ibm.icu.text.DecimalFormatSymbols;
import org.junit.Test;
import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.*;

import java.util.Locale;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Tests about ICU-22558
 *
 * https://unicode-org.atlassian.net/browse/ICU-22558
 */
public class TestFixedICUDecimalFormat {

    // Create DecimalFormatSymbols for a Locale where the decimal separator is a dot
    DecimalFormatSymbols symbols = new DecimalFormatSymbols(Locale.US);

    /**
     * Illustrates the bug in ICU.
     */
    @Test
    public void testDecimalFormatWithHash() {
        // Create DecimalFormat instance with the pattern "#.##"
        DecimalFormat decimalFormat = new DecimalFormat("#.##", symbols);

        // Convert the number 0.12 to String
        String formattedNumber = decimalFormat.format(0.12);

        // This assert tests for the current behavior. (which is buggy)
        assertEquals("0.12", formattedNumber); // Wrong answer: See ICU-22558
        // Assert that the formatted string is ".12"
        if (formattedNumber == ".12") {
            // see https://unicode-org.atlassian.net/browse/ICU-22558
            fail("Bug ICU-22558 has been fixed. This test set is no longer needed to illustrate the problem.");
        }
    }


    Pattern pattern = FixedICUDecimalFormat.pattern;

    /**
     * Tests to be sure the regex that is used in the fix is robust.
     */
    @Test
    public void testExtractDecimalPointAndPrecedingUnescapedDigits0() {
        Matcher matcher = pattern.matcher("\\.");
        assertFalse(matcher.find());
    }

    @Test
    public void testExtractDecimalPointAndPrecedingUnescapedDigits1() {
        Matcher matcher = pattern.matcher("#.##");
        assertTrue(matcher.find());
        assertEquals("#", matcher.group(1));
        assertEquals(".", matcher.group(2));
    }

    @Test
    public void testExtractDecimalPointAndPrecedingUnescapedDigits2() {
        Matcher matcher = pattern.matcher("\\#.##");
        assertTrue(matcher.find());
        assertEquals("", matcher.group(1));
        assertEquals(".", matcher.group(2));
    }

    @Test
    public void testExtractDecimalPointAndPrecedingUnescapedChar3() {
        Matcher matcher = pattern.matcher("'text'#.##");
        assertTrue(matcher.find());
        assertEquals("#", matcher.group(1));
        assertEquals(".", matcher.group(2));    }

    @Test
    public void testExtractDecimalPointAndPrecedingUnescapedChar4() {
        Matcher matcher = pattern.matcher(".##");
        assertTrue(matcher.find());
        assertEquals("", matcher.group(1));
        assertEquals(".", matcher.group(2));
    }

    @Test
    public void testExtractDecimalPointAndPrecedingUnescapedChar5() {
        Matcher matcher = pattern.matcher(".");
        assertTrue(matcher.find());
        assertEquals("", matcher.group(1));
        assertEquals(".", matcher.group(2));
    }

    @Test
    public void testExtractDecimalPointAndPrecedingUnescapedChar6() {
        Matcher matcher = pattern.matcher("5.#");
        assertTrue(matcher.find());
        assertEquals("5", matcher.group(1));
        assertEquals(".", matcher.group(2));
    }

    @Test
    public void testExtractDecimalPointAndPrecedingUnescapedChar7() {
        Matcher matcher = pattern.matcher("5#.#");
        assertTrue(matcher.find());
        assertEquals("5#", matcher.group(1));
        assertEquals(".", matcher.group(2));
    }

    @Test
    public void testExtractDecimalPointAndPrecedingUnescapedChar8() {
        Matcher matcher = pattern.matcher("$\\.$#7\\##9#5#.#");
        assertTrue(matcher.find());
        assertEquals("#9#5#", matcher.group(1));
        assertEquals(".", matcher.group(2));
    }


    /**
     * Show that the fix works.
     */
    @Test
    public void testFixedDecimalFormatWithHash() {
        DecimalFormat decimalFormat = FixedICUDecimalFormat.fromPattern("#.##", symbols);
        String formattedNumber = decimalFormat.format(0.12);
        assertEquals(".12", formattedNumber);
    }

    /**
     * Show that the fix doesn't break patterns with leading digits.
     */
    @Test
    public void testFixedDecimalFormatWithZero() {
        DecimalFormat decimalFormat = FixedICUDecimalFormat.fromPattern("0.##", symbols);
        String formattedNumber = decimalFormat.format(0.12);
        assertEquals("0.12", formattedNumber);
    }
}




