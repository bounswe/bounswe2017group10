package com.bounswe2017.group10.atlas.util;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.widget.Toast;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.auth.AuthActivity;

/**
 * A class that contains general utility methods.
 */
public class Utils {

    public static String tokenToAuthString(String token) {
        return "JWT " + token;
    }

    public static void showToast(Context context, String errorMessage) {
        Toast.makeText(context, errorMessage, Toast.LENGTH_SHORT).show();
    }

    public static SharedPreferences.Editor getSharedPrefEditor(Context context) {
        SharedPreferences preferences = context.getSharedPreferences(context.getString(R.string.shared_pref_file), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = preferences.edit();
        return editor;
    }

    public static SharedPreferences getSharedPref(Context context) {
        SharedPreferences preferences = context.getSharedPreferences(context.getString(R.string.shared_pref_file), Context.MODE_PRIVATE);
        return preferences;
    }

    public static void logout(Context context) {
        // remove token from sharedpref
        Utils.getSharedPrefEditor(context).remove(Constants.AUTH_STR).apply();
        // go to authentication activity
        Intent intent = new Intent(context, AuthActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK|Intent.FLAG_ACTIVITY_NEW_TASK);
        context.startActivity(intent);
    }
}
