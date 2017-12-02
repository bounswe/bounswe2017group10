package com.bounswe2017.group10.atlas.adapter;


import android.content.Intent;
import android.support.test.rule.ActivityTestRule;
import android.widget.ListView;

import com.bounswe2017.group10.atlas.auth.AuthActivity;
import com.bounswe2017.group10.atlas.httpbody.Comment;
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
import static android.support.test.espresso.matcher.ViewMatchers.withText;
import static junit.framework.Assert.assertEquals;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.not;

@RunWith(JUnit4.class)
public class CommentAdapterTest {

    @Rule
    public ActivityTestRule<ProfileActivity> mActivityRule = new ActivityTestRule<>(ProfileActivity.class, true, true);

    private ProfileActivity mActivity;
    private CommentAdapter mAdapter;
    private ArrayList<CommentRow> mComments;
    private ListView mListView;

    @Before
    public void init() {
        // init activity
        Intent intent = new Intent();
        mActivityRule.launchActivity(intent);
        mActivity = mActivityRule.getActivity();

        // init adapter
        mComments = new ArrayList<>();
        mAdapter = new CommentAdapter(mActivity, mComments);

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
        Comment comment = new Comment();
        comment.setText("1");
        mComments.add(comment.toCommentRow());
        mActivity.runOnUiThread(() -> {
            mAdapter.notifyDataSetChanged();
        });

        // listview shows only the comment with text
        onView(withText("1"))
                .inRoot(withDecorView(equalTo(mActivity.getWindow().getDecorView())))
                .check(matches(isDisplayed()));

        // add 4 more items
        for (int i = 2; i < 2 + 4; ++i) {
            comment = new Comment();
            comment.setText("" + i);
            mComments.add(comment.toCommentRow());
        }
        mActivity.runOnUiThread(() -> {
            mAdapter.notifyDataSetChanged();
        });

        // listview shows all the items
        for (int i = 2; i < 2 + 4; ++i) {
            onView(withText("" + i))
                    .inRoot(withDecorView(equalTo(mActivity.getWindow().getDecorView())))
                    .check(matches(isDisplayed()));
        }

        // remove 3 items
        for (int i = 0; i < 3; ++i) {
            mComments.remove(0);
        }
        mActivity.runOnUiThread(() -> {
            mAdapter.notifyDataSetChanged();
        });

        // listview shows the remaining 2 items
        for (int i = 4; i < 2 + 4; ++i) {
            onView(withText("" + i))
                    .inRoot(withDecorView(equalTo(mActivity.getWindow().getDecorView())))
                    .check(matches(isDisplayed()));
        }

        // remove all the items
        mComments.clear();
        mActivity.runOnUiThread(() -> {
            mAdapter.notifyDataSetChanged();
        });

        // listviews shows no items
        for (int i = 4; i < 2 + 4; ++i) {
            onView(withText("" + i))
                    .inRoot(withDecorView(equalTo(mActivity.getWindow().getDecorView())))
                    .check(doesNotExist());
        }
    }
}
