package com.bounswe2017.group10.atlas;


import android.app.Application;

import com.bumptech.glide.request.target.ViewTarget;

public class App extends Application {

    @Override
    public void onCreate() {
        super.onCreate();
        ViewTarget.setTagId(R.id.glide_tag);
    }
}
