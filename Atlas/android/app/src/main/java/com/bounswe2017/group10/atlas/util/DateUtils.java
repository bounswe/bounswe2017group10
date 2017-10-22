package com.bounswe2017.group10.atlas.util;


public class DateUtils {

    /**
     * Convert a year, month, day triple to YYYY-MM-DD string.
     *
     * @param year Year.
     * @param month Month.
     * @param day Day.
     * @return YYYY-MM-DD string.
     */
    public static String toStandardDate(int year, int month, int day) {
        String yearStr = Integer.toString(year);
        String monthStr = Integer.toString(month);
        String dayStr = Integer.toString(day);
        if (month < 10) {
            monthStr = "0" + monthStr;
        }
        if (day < 10) {
            dayStr = "0" + dayStr;
        }
        StringBuilder builder = new StringBuilder();
        builder.append(yearStr);
        builder.append('-');
        builder.append(monthStr);
        builder.append('-');
        builder.append(dayStr);
        return builder.toString();
    }

    /**
     * Convert a standard string in YYYY-MM-DD format
     * to year, month, day array.
     *
     * @param standardDate Date in YYYY-MM-DD.
     * @return Array of year, month, day.
     */
    public static int[] fromStandardDate(String standardDate) {
        int[] parts = new int[3];
        String[] sParts = standardDate.split("-");
        for (int i = 0; i < sParts.length; ++i) {
            parts[i] = Integer.parseInt(sParts[i]);
        }
        return parts;
    }
}
