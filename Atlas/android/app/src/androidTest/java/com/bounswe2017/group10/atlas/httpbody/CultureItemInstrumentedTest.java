package com.bounswe2017.group10.atlas.httpbody;

import android.content.Intent;
import android.os.Bundle;
import android.os.Parcel;
import android.support.test.rule.ActivityTestRule;

import com.bounswe2017.group10.atlas.adapter.FeedRow;
import com.bounswe2017.group10.atlas.profile.ProfileActivity;
import com.bounswe2017.group10.atlas.util.Constants;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.util.ArrayList;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNull;

@RunWith(JUnit4.class)
public class CultureItemInstrumentedTest {

    @Rule
    public ActivityTestRule<ProfileActivity> mActivityRule = new ActivityTestRule<>(ProfileActivity.class, true, false);

    CultureItem mItem;

    @Before
    public void init() {
        mItem = new CultureItem();
        mItem.setId(27);
        mItem.setUser(42);
        mItem.setTitle("Turkish coffee");
        mItem.setDescription("...Example description...");
        mItem.setPlaceName("Istanbul");
        mItem.setLatitude(37.48);
        mItem.setLongitude(40.24);

        ArrayList<Image> imgList = new ArrayList<>();
        Image img = new Image();
        img.setUrl("localhost:242");
        imgList.add(img);
        mItem.setImageList(imgList);

        ArrayList<Tag> tagList = new ArrayList<>();
        mItem.setTagList(tagList);

        ArrayList<Comment> commentList = new ArrayList<>();
        Comment comment = new Comment();
        comment.setText("Very nice topic!");
        commentList.add(comment);
        mItem.setCommentList(commentList);

        mItem.setPublicAccessibility(true);

        Intent intent = new Intent();
        mActivityRule.launchActivity(intent);
    }

    @Test
    public void testToFeedRow() {
        FeedRow feedRow = mItem.toFeedRow();

        // check text and tags
        assertEquals(feedRow.getTagList(), mItem.getTagList());
        assertEquals(feedRow.getTitle(), mItem.getTitle());
        assertEquals(feedRow.getDescription(), mItem.getDescription());

        // assert that if there are images, feedRow gets the first image
        ArrayList<Image> imgList = new ArrayList<>();
        Image img = new Image();
        img.setUrl("localhost:242");
        imgList.add(img);
        mItem.setImageList(imgList);
        feedRow = mItem.toFeedRow();
        assertEquals(feedRow.getImageUrl(), mItem.getImageList().get(0).getUrl());

        // if empty, then feedrow gets null
        mItem.setImageList(new ArrayList<>());
        feedRow = mItem.toFeedRow();
        assertNull(feedRow.getImageUrl());
    }

    @Test
    public void testParcelling() {
        // parcel CultureItem
        Parcel parcel = Parcel.obtain();
        mItem.writeToParcel(parcel, 0);

        // set parcel to read
        parcel.setDataPosition(0);

        // create cultureItem from parcel
        CultureItem itemFromParcel = CultureItem.CREATOR.createFromParcel(parcel);

        assertEquals(mItem, itemFromParcel);
    }

    @Test
    public void testBundling() {
        Bundle bundle = new Bundle();
        bundle.putParcelable(Constants.CULTURE_ITEM, mItem);

        CultureItem itemFromBundle = bundle.getParcelable(Constants.CULTURE_ITEM);

        assertEquals(mItem, itemFromBundle);
    }
}
