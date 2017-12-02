package com.bounswe2017.group10.atlas.httpbody;


import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

public class CultureItemTest {

    @Test
    public void testEquals() {
        CultureItem firstItem = new CultureItem();
        CultureItem secondItem = new CultureItem();

        assertEquals(firstItem, secondItem);

        firstItem.setId(5);
        assertNotEquals(firstItem, secondItem);

        secondItem.setId(firstItem.getId());
        assertEquals(firstItem, secondItem);
        secondItem.setDescription("");
        assertNotEquals(firstItem, secondItem);

        firstItem.setDescription(secondItem.getDescription());
        assertEquals(firstItem, secondItem);

        Tag t1 = new Tag("tag1");
        ArrayList<Tag> firstTagList = new ArrayList<>();
        firstTagList.add(t1);
        firstItem.setTagList(firstTagList);

        assertNotEquals(firstItem, secondItem);

        ArrayList<Tag> secondTagList = new ArrayList<>();
        secondTagList.add(new Tag(t1.getName()));
        secondItem.setTagList(secondTagList);

        assertEquals(firstItem, secondItem);
    }

    @Test
    public void testLatLongEquals() {
        CultureItem firstItem = new CultureItem();
        CultureItem secondItem = new CultureItem();

        firstItem.setLatitude("17.010010");
        secondItem.setLatitude("17.010010");

        assertEquals(firstItem, secondItem);
    }
}
