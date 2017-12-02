package com.bounswe2017.group10.atlas.adapter;


import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertArrayEquals;

public class TestFeedRow {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testToYearFormat() {
        int firstYear = 0;
        int secondYear = 0;

        assertEquals(FeedRow.toYearFormat(firstYear, secondYear), "0/0");

        firstYear = -5;
        secondYear = -30;

        assertEquals(FeedRow.toYearFormat(firstYear, secondYear), "-5/-30");

        firstYear = 2000;
        secondYear = -30;

        assertEquals(FeedRow.toYearFormat(firstYear, secondYear), "2000/-30");
    }

    @Test
    public void testFromYearFormat() {
        String year = "-30/2000";
        int[] yearPair = {-30, 2000};

        assertArrayEquals(yearPair, FeedRow.fromYearFormat(year));

        year = "0/0";
        yearPair = new int[]{0, 0};
        assertArrayEquals(yearPair, FeedRow.fromYearFormat(year));

        thrown.expect(IllegalArgumentException.class);

        year = "5/4/";
        FeedRow.fromYearFormat(year);

        year = "205-2077";
        FeedRow.fromYearFormat(year);
    }
}
