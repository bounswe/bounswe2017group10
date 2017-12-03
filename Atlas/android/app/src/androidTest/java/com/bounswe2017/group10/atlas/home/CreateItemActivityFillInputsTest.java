package com.bounswe2017.group10.atlas.home;


import android.content.Intent;
import android.content.SharedPreferences;
import android.support.test.espresso.Espresso;
import android.support.test.filters.LargeTest;
import android.support.test.rule.ActivityTestRule;
import android.widget.ListView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.auth.AuthActivity;
import com.bounswe2017.group10.atlas.test_utilities.TestUtilities;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.httpbody.Tag;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;

import static android.support.test.espresso.Espresso.onView;
import static android.support.test.espresso.assertion.ViewAssertions.matches;
import static android.support.test.espresso.matcher.RootMatchers.withDecorView;
import static android.support.test.espresso.matcher.ViewMatchers.isDisplayed;
import static android.support.test.espresso.matcher.ViewMatchers.withId;
import static android.support.test.espresso.matcher.ViewMatchers.withText;
import static junit.framework.Assert.assertEquals;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.not;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import java.util.ArrayList;

@LargeTest
public class CreateItemActivityFillInputsTest {

    @Rule
    public ActivityTestRule<CreateItemActivity> mActivityRule = new ActivityTestRule<>(CreateItemActivity.class, true, true);

    @Rule
    public ActivityTestRule<AuthActivity> mLoginActivityRule = new ActivityTestRule<>(AuthActivity.class, true, true);

    private String title;
    private String description;
    private ArrayList<Image> imgList;
    private ArrayList<Tag> tagList;

    private CreateItemActivity mActivity;

    @Before
    public void init() {
        // log in
        SharedPreferences.Editor editor = Utils.getSharedPrefEditor(mLoginActivityRule.getActivity());
        TestUtilities.simulateLogIn(editor);

        // CultureItem data
        title = "Beautiful Prague";
        description = "Item description";
        long user = 5;

        imgList = new ArrayList<>();
        Image img = new Image();
        img.setUrl("http://img.url.com");
        imgList.add(img);
        imgList.add(img);
        imgList.add(img);

        tagList = new ArrayList<>();
        tagList.add(new Tag("tag1"));
        tagList.add(new Tag("tag2"));

        // prepare CultureItem with data
        CultureItem item = new CultureItem();
        item.setUser(user);
        item.setTitle(title);
        item.setDescription(description);
        item.setImageList(imgList);
        item.setTagList(tagList);


        Intent intent = new Intent();
        intent.putExtra(Constants.CULTURE_ITEM, item);
        intent.putExtra(Constants.CREATE_STRATEGY, Constants.EDIT);
        mActivityRule.launchActivity(intent);
        mActivity = mActivityRule.getActivity();
        Espresso.closeSoftKeyboard();
    }

    @Test
    public void testTextViews() {
        // check if EditText objects are filled with correct information
        onView(withId(R.id.title_edittext))
                .inRoot(withDecorView(equalTo(mActivity.getWindow().getDecorView())))
                .check(matches(withText(title)));

        onView(withId(R.id.description_edittext))
                .inRoot(withDecorView(equalTo(mActivity.getWindow().getDecorView())))
                .check(matches(withText(description)));
    }

    @Test
    public void testTags() {
        // check if all the tags are shown
        for (Tag t : tagList) {
            onView(allOf(withId(R.id.tag_textview), withText(t.getName())))
                    .inRoot(withDecorView(equalTo(mActivity.getWindow().getDecorView())))
                    .check(matches(isDisplayed()));
        }
    }

    @Test
    public void testImageCount() {
        // check if correct number of images are shown
        ListView listView = mActivity.findViewById(R.id.image_listview);
        assertEquals(listView.getAdapter().getCount(), imgList.size());
    }
}
