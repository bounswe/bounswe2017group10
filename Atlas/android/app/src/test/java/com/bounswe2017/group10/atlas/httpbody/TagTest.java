package com.bounswe2017.group10.atlas.httpbody;


import org.junit.Test;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

public class TagTest {

    @Test
    public void testEquals() {
        Tag t1 = null;
        Tag t2 = null;

        assertEquals(t1, t2);

        t1 = new Tag("tag1");
        assertNotEquals(t1, t2);

        t2 = new Tag("tag2");
        assertNotEquals(t1, t2);

        t2.setName(t1.getName());
        assertEquals(t1, t2);
    }
}
