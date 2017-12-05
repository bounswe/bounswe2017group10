package com.bounswe2017.group10.atlas.adapter;


import java.util.ArrayList;
import java.util.List;

/**
 * This class represents a single row of user feed. Data stored inside this object
 * is mapped to row view.
 */
public class FeedRow {
    private static final int TAG_COUNT = 3;

    /**
     * Converts a given year pair into a String recognized by FeedRow objects.
     *
     * @param startYear Beginning year.
     * @param endYear Ending year.
     * @return A string in the format "<begin-year>/<end-year>"
     */
    public static String toYearFormat(int startYear, int endYear) {
        return String.format("%d/%d", startYear, endYear);
    }

    /**
     * Converts a given String in the format returned by toYearFormat(int, int)
     * to a pair of integers.
     *
     * @param yearFormat
     * @throws IllegalArgumentException If the string is not in the format "<begin-year>/<end-year>"
     * @return A pair of integers {startYear, endYear}.
     */
    public static int[] fromYearFormat(String yearFormat) {
        String[] parts = yearFormat.split("/");
        if (parts.length != 2) {
            throw new IllegalArgumentException("Given string is not in expected format: " + yearFormat);
        }

        int[] intParts = {Integer.parseInt(parts[0]), Integer.parseInt(parts[1])};
        return intParts;
    }

    private String imageUrl;
    private String title;
    private String description;
    private String location;
    private String year;
    private List<String> tagList;
    private String favoriteCount;

    public FeedRow() {

    }

    public FeedRow(String imageUrl, String title, String description, String location, String year, List<String> tagList, String favoriteCount) {
        this.imageUrl = imageUrl;
        this.title = title;
        this.description = description;
        this.location = location;
        this.year = year;
        this.tagList = new ArrayList<>();
        for (int i = 0; i < tagList.size(); ++i) {
            if (i < TAG_COUNT) {
                this.tagList.add(tagList.get(i));
            }
        }
        this.favoriteCount = favoriteCount;
    }

    public String getImageUrl() {
        return imageUrl;
    }

    public void setImageUrl(String imageUrl) {
        this.imageUrl = imageUrl;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getLocation() {
        return location;
    }

    public void setLocation(String location) {
        this.location = location;
    }

    public String getYear() {
        return year;
    }

    public void setYear(String year) {
        this.year = year;
    }

    public void setTagList(List<String> tagList) {
        this.tagList = tagList;
    }

    public List<String> getTagList() {
        return tagList;
    }

    public String getFavoriteCount() {
        return favoriteCount;
    }

    public void setFavoriteCount(String favoriteCount) {
        this.favoriteCount = favoriteCount;
    }
}
