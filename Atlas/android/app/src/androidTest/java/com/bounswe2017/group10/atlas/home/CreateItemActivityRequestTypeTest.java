package com.bounswe2017.group10.atlas.home;


import android.content.Intent;
import android.content.SharedPreferences;
import android.support.test.rule.ActivityTestRule;
import android.widget.EditText;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.test_utilities.APIDelegate;
import com.bounswe2017.group10.atlas.auth.AuthActivity;
import com.bounswe2017.group10.atlas.test_utilities.TestUtilities;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.remote.API;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.remote.RetrofitBuilder;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mockito;


import retrofit2.converter.gson.GsonConverterFactory;

import static android.support.test.espresso.Espresso.onView;
import static android.support.test.espresso.action.ViewActions.click;
import static android.support.test.espresso.matcher.RootMatchers.withDecorView;
import static android.support.test.espresso.matcher.ViewMatchers.withId;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.times;

public class CreateItemActivityRequestTypeTest {

    @Rule
    public ActivityTestRule<CreateItemActivity> mActivityRule = new ActivityTestRule<>(CreateItemActivity.class, true, true);

    @Rule
    public ActivityTestRule<AuthActivity> mLoginActivityRule = new ActivityTestRule<>(AuthActivity.class, true, true);

    private CreateItemActivity mActivity;
    private APIDelegate spy;

    /**
     * An extension of CultureItem that overrides equals method for this test
     * case's needs.
     */
    public static class TestCultureItem extends CultureItem {
        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof CultureItem)) {
                return false;
            }
            CultureItem other = (CultureItem)obj;
            return this.getTitle().equals(other.getTitle()) &&
                    this.getId() == other.getId();
        }
    }

    @Before
    public void init() {
        // log in
        SharedPreferences.Editor editor = Utils.getSharedPrefEditor(mLoginActivityRule.getActivity());
        TestUtilities.simulateLogIn(editor);

        // create a custom API to spy on
        RetrofitBuilder builder = new RetrofitBuilder()
                .baseUrl("http://0.1.2.3:12345")
                .addConverterFactory(GsonConverterFactory.create());
        API api = builder.build().create(API.class);
        APIDelegate delegate = new APIDelegate(api);
        spy = Mockito.spy(delegate);
        APIUtils.setServerAPI(spy);
    }

    @Test
    public void testEditRequest() {
        // CultureItem data
        TestCultureItem item = new TestCultureItem();
        item.setTitle("Beautiful Prague");
        item.setId(124);

        // send an argument item
        Intent intent = new Intent();
        intent.putExtra(Constants.CULTURE_ITEM, item);
        intent.putExtra(Constants.CREATE_STRATEGY, Constants.EDIT);
        mActivityRule.launchActivity(intent);
        mActivity = mActivityRule.getActivity();

        // click on FAB to make a request
        onView(withId(R.id.floatingActionButton))
                .inRoot(withDecorView(equalTo(mActivity.getWindow().getDecorView())))
                .perform(click());

        String authStr = Utils.getSharedPref(mActivity).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
        // verify that edit request has been made
        Mockito.verify(spy, times(1)).updateItem(authStr, item.getId(), item);
        // verify that create request has not been made
        Mockito.verify(spy, times(0)).createItem(authStr, item);
    }

    @Test
    public void testCreateRequest() {
        // CultureItem data
        String title = "Beautiful Prague";
        TestCultureItem item = new TestCultureItem();
        item.setTitle(title);

        Intent intent = new Intent();
        mActivityRule.launchActivity(intent);
        mActivity = mActivityRule.getActivity();

        // fill title edittext
        ((EditText)mActivity.findViewById(R.id.title_edittext)).setText(title);

        // click on FAB to make a request
        onView(withId(R.id.floatingActionButton))
                .inRoot(withDecorView(equalTo(mActivity.getWindow().getDecorView())))
                .perform(click());

        String authStr = Utils.getSharedPref(mActivity).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
        // verify that create request has been made
        Mockito.verify(spy, times(1)).createItem(authStr, item);
        // verify that edit request has not been made
        Mockito.verify(spy, times(0)).updateItem(authStr, item.getId(), item);
    }
}
