package com.bounswe2017.group10.atlas.util;


public class DateUtils {
    public static String toStandardDate(int year, int month, int day) {
        StringBuilder builder = new StringBuilder();
        builder.append(Integer.toString(year));
        builder.append('/');
        builder.append(Integer.toString(month));
        builder.append('/');
        builder.append(Integer.toString(day));
        return builder.toString();
    }
}
