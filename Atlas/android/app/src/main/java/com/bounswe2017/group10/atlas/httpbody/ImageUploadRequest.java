package com.bounswe2017.group10.atlas.httpbody;


import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;

import java.util.List;

public class ImageUploadRequest {

    @SerializedName("images")
    @Expose
    private List<Image> images;

    public ImageUploadRequest(List<Image> imageList) {
        this.images = imageList;
    }

    public List<Image> getImages() {
        return images;
    }

    public void setImages(List<Image> images) {
        this.images = images;
    }
}

