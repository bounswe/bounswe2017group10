package com.bounswe2017.group10.atlas.adapter;


public class ImageRow {
    private String url;

    public String getUrl() {
        return url;
    }

    public void setUrl(String imageUrl) {
        this.url = imageUrl;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof  ImageRow) {
            ImageRow ref = (ImageRow)obj;
            return this.getUrl().equals(ref.getUrl());
        } else {
            return false;
        }
    }
}
