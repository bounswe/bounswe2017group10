package com.bounswe2017.group10.atlas.util;


import org.junit.Test;


import static org.junit.Assert.*;

public class DateUtilsTest {

    @Test
    public void testToStandardDate() {
        int y = 1990;
        int m = 5;
        int d = 3;
        assertEquals("1990-05-03", DateUtils.toStandardDate(y, m, d));

        y = 1990;
        m = 11;
        d = 3;
        assertEquals("1990-11-03", DateUtils.toStandardDate(y, m, d));

        y = 1990;
        m = 5;
        d = 31;
        assertEquals("1990-05-31", DateUtils.toStandardDate(y, m, d));

        y = 2018;
        m = 12;
        d = 31;
        assertEquals("2018-12-31", DateUtils.toStandardDate(y, m, d));
    }

    @Test
    public void testFromStandardDate() {
        String date = "2013-05-01";
        int[] expected = new int[]{2013, 5, 1};
        int[] actual = DateUtils.fromStandardDate(date);
        assertArrayEquals(expected, actual);

        date = "2013-11-12";
        expected = new int[]{2013, 11, 12};
        actual = DateUtils.fromStandardDate(date);
        assertArrayEquals(expected, actual);

        date = "1990-05-30";
        expected = new int[]{1990, 5, 30};
        actual = DateUtils.fromStandardDate(date);
        assertArrayEquals(expected, actual);

        date = "1990-11-01";
        expected = new int[]{1990, 11, 1};
        actual = DateUtils.fromStandardDate(date);
        assertArrayEquals(expected, actual);
    }
}
