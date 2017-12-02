package com.bounswe2017.group10.atlas.adapter;


import android.net.Uri;

import com.bounswe2017.group10.atlas.httpbody.Image;


public class ImageRow {
    private Uri uri;

    public Uri getUri() {
        return uri;
    }

    public void setUri(Uri imageUri) {
        this.uri = imageUri;
    }

    public Image toImage() {
        Image img = new Image();
        img.setUrl(this.uri.toString());
        return img;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof  ImageRow) {
            ImageRow ref = (ImageRow)obj;
            return this.getUri().equals(ref.getUri());
        } else {
            return false;
        }
    }
}
