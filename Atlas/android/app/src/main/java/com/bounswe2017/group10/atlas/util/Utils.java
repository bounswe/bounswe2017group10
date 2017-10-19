package com.bounswe2017.group10.atlas.util;


import android.content.Context;
import android.widget.Toast;

public class Utils {

    public static void showToast(Context context, String errorMessage) {
        Toast.makeText(context, errorMessage, Toast.LENGTH_SHORT).show();
    }
}
