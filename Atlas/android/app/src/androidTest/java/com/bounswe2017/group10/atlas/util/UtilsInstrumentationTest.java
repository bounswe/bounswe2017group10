package com.bounswe2017.group10.atlas.util;


import android.Manifest;
import android.app.Instrumentation;
import android.content.Intent;
import android.content.SharedPreferences;
import android.support.test.rule.ActivityTestRule;
import android.support.test.rule.GrantPermissionRule;

import com.bounswe2017.group10.atlas.auth.AuthActivity;
import com.bounswe2017.group10.atlas.test_utilities.TestUtilities;
import com.bounswe2017.group10.atlas.profile.ProfileActivity;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;


import static android.support.test.InstrumentationRegistry.getInstrumentation;
import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertTrue;

@RunWith(JUnit4.class)
public class UtilsInstrumentationTest {

    @Rule
    public ActivityTestRule<ProfileActivity> mActivityRule = new ActivityTestRule<>(ProfileActivity.class, true, false);

    @Rule
    public GrantPermissionRule permissionRule = GrantPermissionRule.grant(Manifest.permission.READ_EXTERNAL_STORAGE, Manifest.permission.WRITE_EXTERNAL_STORAGE);

    public ProfileActivity mActivity;

    @Before
    public void init() {
        Intent intent = new Intent();
        mActivityRule.launchActivity(intent);
        mActivity = mActivityRule.getActivity();
    }

    @Test
    public void testGetSharedPrefEditor() {
        SharedPreferences.Editor editor = Utils.getSharedPrefEditor(mActivity);
        assertNotNull(editor);
    }

    @Test
    public void testGetSharedPref() {
        SharedPreferences pref = Utils.getSharedPref(mActivity);
        assertNotNull(pref);
    }


    @Test
    public void testLogout() {
        // simulate a login
        SharedPreferences.Editor editor = Utils.getSharedPrefEditor(mActivity);
        TestUtilities.simulateLogIn(editor);

        SharedPreferences pref = Utils.getSharedPref(mActivity);
        Instrumentation.ActivityMonitor monitor = getInstrumentation().addMonitor(AuthActivity.class.getName(), null, false);

        Utils.logout(mActivity);

        // check if we have logged out correctly
        assertTrue(TestUtilities.hasLoggedOut(pref, monitor));
    }
}
