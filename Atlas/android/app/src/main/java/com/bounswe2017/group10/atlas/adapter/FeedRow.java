package com.bounswe2017.group10.atlas.adapter;


/**
 * This class represents a single row of user feed. Data stored inside this object
 * is mapped to row view.
 */
public class FeedRow {
    private String imageUrl;
    private String title;
    private String description;

    public FeedRow(String imageUrl, String title, String description) {
        this.imageUrl = imageUrl;
        this.title = title;
        this.description = description;
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
}
