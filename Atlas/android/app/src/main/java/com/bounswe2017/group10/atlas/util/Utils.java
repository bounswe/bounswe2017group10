package com.bounswe2017.group10.atlas.util;


/**
 * A class that contains general utility methods.
 */
public class Utils {

    public static String tokenToAuthString(String token) {
        return "JWT " + token;
    }
}
