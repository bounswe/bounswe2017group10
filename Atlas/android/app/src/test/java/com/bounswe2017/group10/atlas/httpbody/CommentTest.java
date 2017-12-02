package com.bounswe2017.group10.atlas.httpbody;


import org.junit.Test;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

public class CommentTest {

    @Test
    public void testEquals() {
        Comment c1 = null;
        Comment c2 = null;

        assertEquals(c1, c2);

        c1 = new Comment();
        assertNotEquals(c1, c2);

        c2 = new Comment();
        assertEquals(c1, c2);

        c1.setText("comment1");
        assertNotEquals(c1, c2);

        c2.setText(c1.getText());
        assertEquals(c1, c2);

        c1.setUser("user1");
        assertNotEquals(c1, c2);

        c2.setUser(c1.getUser());
        assertEquals(c1, c2);

        // the rest is too long
    }
}
