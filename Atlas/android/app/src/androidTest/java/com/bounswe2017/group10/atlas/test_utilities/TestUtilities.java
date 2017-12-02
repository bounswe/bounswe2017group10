package com.bounswe2017.group10.atlas.test_utilities;


import android.app.Activity;
import android.app.Instrumentation;
import android.content.SharedPreferences;

import com.bounswe2017.group10.atlas.auth.AuthActivity;
import com.bounswe2017.group10.atlas.util.Constants;

import static android.support.test.InstrumentationRegistry.getInstrumentation;

public class TestUtilities {

    public static void simulateLogIn(SharedPreferences.Editor editor) {
        editor.putString(Constants.AUTH_STR, "JWT aoestnuhhi2473STNAHO");
        editor.putString(Constants.FIRSTNAME, "Esref");
        editor.putString(Constants.LASTNAME, "Ozdemir");
        editor.putString(Constants.EMAIL, "abc@abn.com");
        editor.putString(Constants.USER_ID, "eozd");
        editor.apply();
    }

    public static boolean hasLoggedOut(SharedPreferences pref, Instrumentation.ActivityMonitor monitor) {
        Activity currActivity = getInstrumentation().waitForMonitor(monitor);

        // the user information is removed from SharedPreferences
        boolean emptyPref = !pref.contains(Constants.AUTH_STR) &&
                            !pref.contains(Constants.FIRSTNAME) &&
                            !pref.contains(Constants.LASTNAME) &&
                            !pref.contains(Constants.EMAIL) &&
                            !pref.contains(Constants.USER_ID);
        // activity is changed to AuthActivity
        boolean correctActivity = AuthActivity.class.getName().equals(currActivity.getClass().getName());

        return emptyPref && correctActivity;
    }
}
