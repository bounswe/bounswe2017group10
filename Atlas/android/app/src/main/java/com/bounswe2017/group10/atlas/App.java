package com.bounswe2017.group10.atlas;


import android.app.Application;

import com.bounswe2017.group10.atlas.util.Constants;
import com.bumptech.glide.request.target.ViewTarget;
import com.cloudinary.Cloudinary;
import com.cloudinary.android.MediaManager;

import java.util.HashMap;
import java.util.Map;

public class App extends Application {

    @Override
    public void onCreate() {
        super.onCreate();
        // glide specific code
        ViewTarget.setTagId(R.id.glide_tag);

        // cloudinary specific code
        Map cloudinaryConfig = new HashMap();
        cloudinaryConfig.put("cloud_name", Constants.CLOUDINARY_CLOUD_NAME);
        cloudinaryConfig.put("api_key", "642824638492586");
        cloudinaryConfig.put("api_secret", "Ij8AR7OJpMcUGxPTB1fw4Ij7dio");
        cloudinaryConfig.put("enhance_image_tag", true);
        cloudinaryConfig.put("static_image_support", true);
        MediaManager.init(this, cloudinaryConfig);
    }
}
