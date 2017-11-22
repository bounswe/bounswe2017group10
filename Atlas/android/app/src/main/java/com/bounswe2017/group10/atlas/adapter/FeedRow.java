package com.bounswe2017.group10.atlas.adapter;


import java.util.ArrayList;
import java.util.List;

/**
 * This class represents a single row of user feed. Data stored inside this object
 * is mapped to row view.
 */
public class FeedRow {
    private static final int TAG_COUNT = 3;

    private String imageUrl;
    private String title;
    private String description;
    private List<String> tagList;

    public FeedRow() {

    }

    public FeedRow(String imageUrl, String title, String description, List<String> tagList) {
        this.imageUrl = imageUrl;
        this.title = title;
        this.description = description;
        this.tagList = new ArrayList<>();
        for (int i = 0; i < tagList.size(); ++i) {
            if (i < TAG_COUNT) {
                this.tagList.add(tagList.get(i));
            }
        }
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

    public List<String> getTagList() {
        return tagList;
    }
}
