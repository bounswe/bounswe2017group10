package com.bounswe2017.group10.atlas.util;


import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;

public class UtilsTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testTokenToAuthString() {
        String token = "";
        assertEquals("JWT " + token, Utils.tokenToAuthString(token));

        token = "aoesnutda";
        assertEquals("JWT " + token, Utils.tokenToAuthString(token));
    }

    @Test
    public void testIsLocalUrl() {
        String url = "http://localhost:8000";
        assertFalse(Utils.isLocalUrl(url));

        url = "";
        assertFalse(Utils.isLocalUrl(url));

        url = "content://";
        assertTrue(Utils.isLocalUrl(url));

        url = "content:";
        assertFalse(Utils.isLocalUrl(url));
    }

    @Test
    public void testIsClose() {
        double a = 5;
        double b = 6;

        assertFalse(Utils.isClose(a, b));

        a = b;
        assertTrue(Utils.isClose(a, b));

        // 64-bit double precision number has approximately 14 precise digits after decimal point
        double remainingPrecision = 1e-14/Constants.DOUBLE_EQUALITY_EPSILON;

        a = b + (1 - remainingPrecision)*Constants.DOUBLE_EQUALITY_EPSILON;
        assertTrue(Utils.isClose(a, b));

        a = b - (1 - remainingPrecision)*Constants.DOUBLE_EQUALITY_EPSILON;
        assertTrue(Utils.isClose(a, b));

        a = b + (1 + remainingPrecision)*Constants.DOUBLE_EQUALITY_EPSILON;
        assertFalse(Utils.isClose(a, b));

        a = b - (1 + remainingPrecision)*Constants.DOUBLE_EQUALITY_EPSILON;
        assertFalse(Utils.isClose(a, b));
    }

    @Test
    public void testObjectEquals() {
        Integer a = null;
        Integer b = new Integer(5);

        // null and not-null are not equal
        assertFalse(Utils.objectEquals(a, b));
        // two null objs are equal
        assertTrue(Utils.objectEquals(a, null));

        // not null, but not equal
        a = new Integer(6);
        assertFalse(Utils.objectEquals(a, b));

        // not null, equal
        a = new Integer(b.intValue());
        assertTrue(Utils.objectEquals(a, b));
    }

    @Test
    public void testRoundToDecimals() {
        double a = 3;

        assertTrue(Utils.isClose(a, Utils.roundToDecimals(a, 0)));
        assertTrue(Utils.isClose(a, Utils.roundToDecimals(a, 1)));

        a = 3.00001;
        assertTrue(Utils.isClose(3, Utils.roundToDecimals(a, 4)));
        assertFalse(Utils.isClose(3, Utils.roundToDecimals(a, 5)));

        a = 3.14723;
        assertTrue(Utils.isClose(3.14, Utils.roundToDecimals(a, 2)));
        assertFalse(Utils.isClose(3.14, Utils.roundToDecimals(a, 3)));

        assertTrue(Utils.isClose(a, Utils.roundToDecimals(a, 8)));

        thrown.expect(IllegalArgumentException.class);
        Utils.roundToDecimals(a, -1);
        Utils.roundToDecimals(a, 9);
    }

    @Test
    public void testIsValidYear() {
        String s = "";
        assertFalse(Utils.isValidYear(s));

        s = "what";
        assertFalse(Utils.isValidYear(s));

        s = "" + (Constants.MIN_YEAR - 1);
        assertFalse(Utils.isValidYear(s));

        s = "" + (Constants.MAX_YEAR + 1);
        assertFalse(Utils.isValidYear(s));

        s = "" + (Constants.MIN_YEAR + Constants.MAX_YEAR)/2;
        assertTrue(Utils.isValidYear(s));
    }
}
