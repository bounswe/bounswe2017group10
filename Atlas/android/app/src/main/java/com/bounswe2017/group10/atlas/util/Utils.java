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

    /**
     * Given a token string such as "snocehu724ao726a2HAOS42a", returns an
     * authentication string that can be used with Authorization headers.
     *
     * @param token Token string.
     * @return Authorization string that can be directly used in Authorization header.
     */
    public static String tokenToAuthString(String token) {
        return "JWT " + token;
    }

    /**
     * Show a Toast on the given context with the given message. Toast duration
     * is set to Toast.LENGTH_SHORT
     *
     * @param context Context in which to show the Toast.
     * @param errorMessage Message to be displayed.
     */
    public static void showToast(Context context, String errorMessage) {
        Toast.makeText(context, errorMessage, Toast.LENGTH_SHORT).show();
    }

    /**
     * Get SharedPreferences.Editor instance used by the app.
     *
     * @param context Context object.
     * @return SharedPreferences.Editor object.
     */
    public static SharedPreferences.Editor getSharedPrefEditor(Context context) {
        SharedPreferences preferences = context.getSharedPreferences(context.getString(R.string.shared_pref_file), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = preferences.edit();
        return editor;
    }

    /**
     * Get SharedPreferences instance used by the app.
     *
     * @param context Context object.
     * @return SharedPreferences object.
     */
    public static SharedPreferences getSharedPref(Context context) {
        SharedPreferences preferences = context.getSharedPreferences(context.getString(R.string.shared_pref_file), Context.MODE_PRIVATE);
        return preferences;
    }

    /**
     * Perform a logout. This function removes all the user-related information
     * from SharedPreferences, and takes the user to AuthActivity for further
     * auhtentication.
     *
     * @param context Context in which to logout.
     */
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

    /**
     * Get a new image Uri on local Android device external storage. Later, this URI
     * can be used to save an image to the device.
     *
     * Returned URI has name "JPEG_<timestamp>_...jpg" where
     * <timestamp> is in "yyyyMMdd_HHmmss" date format.
     *
     * @param context Context in which we will request a File on external storage.
     * @return URI of the image on external storage.
     * @throws IOException If unable to get a File URI, this method throws an IOException
     */
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

    /**
     * Return ture if the given url is a local Android url.
     *
     * A local Android URL is defined as one that starts with String
     * "content://"
     *
     * @param url Url
     * @return true if the given url is Android-local; false, otherwise.
     */
    public static boolean isLocalUrl(String url) {
        return url.startsWith("content://");
    }

    /**
     * Return true if the given two doubles are within Constants.DOUBLE_EQUALITY_EPSILON
     * away from each other.
     *
     * @param a First double.
     * @param b Second double.
     * @return true if the given two double values are close; false, otherwise.
     */
    public static boolean isClose(double a, double b) {
        return Math.abs(a - b) <= Constants.DOUBLE_EQUALITY_EPSILON;
    }

    /**
     * Return true if the given two Objects are equal. Objects a and b are
     * equal if, and only if,
     *
     * a == null && b == null, OR
     * a != null && b != null && a.equals(b)
     *
     * @param a First Object.
     * @param b Second Object.
     * @return true if a and b are equal; false, otherwise.
     */
    public static boolean objectEquals(Object a, Object b) {
        if (a == null && b != null) {
            return false;
        } else if (a != null && b == null) {
            return false;
        } else if (a != null && b != null) {
            return a.equals(b);
        }
        return true;
    }

    /**
     * Rounds the given double to precision number of decimals. For example,
     *
     * roundToDecimals(3.1437242, 3) == 3.143
     *
     * @param num Double number to round to decimals
     * @param precision Number of digits after decimal point
     * @return double number with precision many digits after decimal point.
     */
    public static double roundToDecimals(double num, int precision) {
        if (precision < 0 || precision > 8) {
            throw new IllegalArgumentException("Precision must be an int from 0 to 8.");
        }
        long multiplier = (long)Math.pow(10, (double)precision);
        return ((int)(num*multiplier))/(double)multiplier;
    }
}
