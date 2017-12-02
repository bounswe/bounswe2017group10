package com.bounswe2017.group10.atlas.util;


import org.junit.Test;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;

public class UtilsTest {

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
}
