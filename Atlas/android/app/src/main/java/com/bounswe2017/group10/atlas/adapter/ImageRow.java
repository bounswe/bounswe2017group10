package com.bounswe2017.group10.atlas.adapter;


import android.net.Uri;


public class ImageRow {
    private Uri uri;

    public Uri getUri() {
        return uri;
    }

    public void setUri(Uri imageUri) {
        this.uri = imageUri;
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
