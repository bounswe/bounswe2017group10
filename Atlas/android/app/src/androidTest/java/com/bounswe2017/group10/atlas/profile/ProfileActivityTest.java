package com.bounswe2017.group10.atlas.profile;


import android.app.Instrumentation;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Looper;
import android.support.test.rule.ActivityTestRule;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.auth.AuthActivity;
import com.bounswe2017.group10.atlas.auth.TestUtilities;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import static android.support.test.InstrumentationRegistry.getInstrumentation;
import static android.support.test.espresso.Espresso.onView;
import static android.support.test.espresso.action.ViewActions.click;
import static android.support.test.espresso.action.ViewActions.scrollTo;
import static android.support.test.espresso.assertion.ViewAssertions.matches;
import static android.support.test.espresso.matcher.ViewMatchers.isDisplayed;
import static android.support.test.espresso.matcher.ViewMatchers.withClassName;
import static android.support.test.espresso.matcher.ViewMatchers.withText;
import static junit.framework.Assert.assertTrue;
import static net.bytebuddy.matcher.ElementMatchers.is;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.core.StringEndsWith.endsWith;

public class ProfileActivityTest {

    @Rule
    public ActivityTestRule<AuthActivity> mLoginActivityRule = new ActivityTestRule<>(AuthActivity.class, true, true);

    @Rule
    public ActivityTestRule<ProfileActivity> mActivityRule = new ActivityTestRule<>(ProfileActivity.class, true, false);

    public ProfileActivity mActivity;

    @Before
    public void init() {
        // log in
        SharedPreferences.Editor editor = Utils.getSharedPrefEditor(mLoginActivityRule.getActivity());
        TestUtilities.simulateLogIn(editor);

        // launch profile activity
        Intent intent = new Intent();
        mActivityRule.launchActivity(intent);
        mActivity = mActivityRule.getActivity();
    }

    @Test
    public void testShowUserInformation() {
        // prepare the expected strings
        SharedPreferences pref = Utils.getSharedPref(mActivity);
        String firstName = pref.getString(Constants.FIRSTNAME, "");
        String lastName = pref.getString(Constants.LASTNAME, "");
        String expectedName = mActivity.getString(R.string.fullname, firstName, lastName);
        String expectedEmail = pref.getString(Constants.EMAIL, "");

        // check if name is shown
        onView(allOf(withClassName(endsWith("TextView")), withText(expectedName))).check(matches(isDisplayed()));
        // check if email is shown
        onView(allOf(withClassName(endsWith("TextView")), withText(expectedEmail))).check(matches(isDisplayed()));
    }

    @Test
    public void testLogoutFromProfile() {
        // logout
        SharedPreferences pref = Utils.getSharedPref(mActivity);
        Instrumentation.ActivityMonitor monitor = getInstrumentation().addMonitor(AuthActivity.class.getName(), null, false);

        // click logout button
        onView(allOf(withClassName(endsWith("TextView")), withText(R.string.logout))).perform(scrollTo(), click());
        // check if we have successfully logged out
        assertTrue(TestUtilities.hasLoggedOut(pref, monitor));
    }
}
