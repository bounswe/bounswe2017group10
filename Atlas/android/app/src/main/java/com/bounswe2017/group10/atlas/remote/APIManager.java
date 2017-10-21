package com.bounswe2017.group10.atlas.remote;


import com.bounswe2017.group10.atlas.BuildConfig;

import java.util.concurrent.TimeUnit;

import retrofit2.converter.gson.GsonConverterFactory;

public class APIManager {

    private String baseUrl;
    private API api;

    public APIManager(String baseUrl) {
        this.baseUrl = baseUrl;
        RetrofitBuilder builder = new RetrofitBuilder()
                .baseUrl(this.baseUrl)
                .addConverterFactory(GsonConverterFactory.create())
                .connectTimeout(60, TimeUnit.SECONDS)
                .readTimeout(60, TimeUnit.SECONDS)
                .writeTimeout(60, TimeUnit.SECONDS);

        if (BuildConfig.DEBUG) {
            builder.addInterceptor();
        }

        this.api = builder.build().create(API.class);
    }

    public API getAPI() {
        return api;
    }

    public String getBaseUrl() {
        return baseUrl;
    }
}
