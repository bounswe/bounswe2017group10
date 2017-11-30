package com.bounswe2017.group10.atlas.util;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Environment;
import android.support.v4.content.FileProvider;
import android.widget.Toast;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.auth.AuthActivity;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

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
        SharedPreferences.Editor editor = Utils.getSharedPrefEditor(context);
        editor.remove(Constants.AUTH_STR);
        editor.remove(Constants.FIRSTNAME);
        editor.remove(Constants.LASTNAME);
        editor.remove(Constants.EMAIL);
        editor.remove(Constants.USER_ID);
        editor.apply();

        // go to authentication activity
        Intent intent = new Intent(context, AuthActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
        context.startActivity(intent);

    }

    public static Uri getNewImageUri(Context context) throws IOException {
        String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.US).format(new Date());
        String imageFileName = "JPEG_" + timeStamp + "_";
        File storageDir = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES);
        File image = File.createTempFile(
                imageFileName,  // prefix
                ".jpg",         // suffix
                storageDir      // directory
        );
        Uri uri = FileProvider.getUriForFile(context, Constants.FILE_PROVIDER_AUTHORITY, image);

        return uri;
    }

    public static boolean isLocalUrl(String url) {
        return url.startsWith("content://");
    }
}
