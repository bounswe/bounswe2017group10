package com.bounswe2017.group10.atlas.httpbody;


import android.net.Uri;

import com.bounswe2017.group10.atlas.adapter.ImageRow;
import com.bounswe2017.group10.atlas.util.Utils;
import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;

import java.io.Serializable;

public class Image implements Serializable {
    @SerializedName("url")
    @Expose
    private String url;

    @SerializedName("main")
    @Expose
    private boolean main;

    public boolean isMain() {
        return main;
    }

    public void setMain(boolean main) {
        this.main = main;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public ImageRow toImageRow() {
        ImageRow row = new ImageRow();
        row.setUri(Uri.parse(url));
        return row;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Image)) {
            return false;
        }
        Image other = (Image)obj;
        // main is not important
        return Utils.objectEquals(this.url, other.url);
    }
}
