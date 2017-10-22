package com.bounswe2017.group10.atlas.util;

import android.content.Context;
import android.widget.Toast;

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
}
