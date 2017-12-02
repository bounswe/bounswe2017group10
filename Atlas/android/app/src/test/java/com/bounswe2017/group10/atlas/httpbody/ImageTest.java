package com.bounswe2017.group10.atlas.httpbody;


import org.junit.Test;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

public class ImageTest {

    @Test
    public void testEquals() {
        Image img1 = null;
        Image img2 = null;

        assertEquals(img1, img2);

        img1 = new Image();

        assertNotEquals(img1, img2);

        img2 = new Image();
        assertEquals(img1, img2);

        img1.setUrl("url1");
        assertNotEquals(img1, img2);

        img2.setUrl(img1.getUrl());
        assertEquals(img1, img2);
    }
}
