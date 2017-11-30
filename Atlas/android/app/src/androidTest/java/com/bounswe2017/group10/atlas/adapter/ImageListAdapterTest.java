package com.bounswe2017.group10.atlas.adapter;


import android.content.Intent;
import android.net.Uri;
import android.support.test.rule.ActivityTestRule;
import android.widget.ListView;

import com.bounswe2017.group10.atlas.profile.ProfileActivity;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.util.ArrayList;

import static android.support.test.espresso.Espresso.onView;
import static android.support.test.espresso.assertion.ViewAssertions.doesNotExist;
import static android.support.test.espresso.assertion.ViewAssertions.matches;
import static android.support.test.espresso.matcher.RootMatchers.withDecorView;
import static android.support.test.espresso.matcher.ViewMatchers.isDisplayed;
import static android.support.test.espresso.matcher.ViewMatchers.withContentDescription;
import static junit.framework.Assert.assertEquals;
import static org.hamcrest.Matchers.equalTo;

@RunWith(JUnit4.class)
public class ImageListAdapterTest {

    @Rule
    public ActivityTestRule<ProfileActivity> mActivityRule = new ActivityTestRule<>(ProfileActivity.class, true, true);

    private ProfileActivity mActivity;
    private ImageListAdapter mAdapter;
    private ArrayList<ImageRow> mImages;
    private ListView mListView;

    private Uri[] uriList = {
                                Uri.parse("http://localhost:1"),
                                Uri.parse("http://localhost:2"),
                            };

    @Before
    public void init() {
        // init activity
        Intent intent = new Intent();
        mActivityRule.launchActivity(intent);
        mActivity = mActivityRule.getActivity();

        // init adapter
        mImages = new ArrayList<>();
        mAdapter = new ImageListAdapter(mActivity, mImages);

        // init list view
        mListView = new ListView(mActivity);
        mListView.setAdapter(mAdapter);

        // set listview as content view
        mActivity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                mActivity.setContentView(mListView);
            }
        });
    }

    @Test
    public void testShowAddedOrRemovedItems() {
        // assert that no item is shown initially
        assertEquals(0, mListView.getChildCount());

        // add single item
        ImageRow imageRow = new ImageRow();
        imageRow.setUri(uriList[0]);
        mImages.add(imageRow);
        mActivity.runOnUiThread(() -> {
            mAdapter.notifyDataSetChanged();
        });

        // listview shows only the comment with text
        onView(withContentDescription(uriList[0].toString()))
                .inRoot(withDecorView(equalTo(mActivity.getWindow().getDecorView())))
                .check(matches(isDisplayed()));

        // add the other items
        imageRow = new ImageRow();
        imageRow.setUri(uriList[1]);
        mImages.add(imageRow);
        mActivity.runOnUiThread(() -> {
            mAdapter.notifyDataSetChanged();
        });

        // listview shows the item
        onView(withContentDescription(uriList[1].toString()))
                .inRoot(withDecorView(equalTo(mActivity.getWindow().getDecorView())))
                .check(matches(isDisplayed()));

        // remove 1 item
        mImages.remove(0);
        mActivity.runOnUiThread(() -> {
            mAdapter.notifyDataSetChanged();
        });

        // listview shows the remaining item
        onView(withContentDescription(uriList[1].toString()))
                .inRoot(withDecorView(equalTo(mActivity.getWindow().getDecorView())))
                .check(matches(isDisplayed()));

        // remove all the items
        mImages.clear();
        mActivity.runOnUiThread(() -> {
            mAdapter.notifyDataSetChanged();
        });

        onView(withContentDescription(uriList[0].toString()))
                .inRoot(withDecorView(equalTo(mActivity.getWindow().getDecorView())))
                .check(doesNotExist());
    }
}

