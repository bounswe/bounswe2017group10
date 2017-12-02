package com.bounswe2017.group10.atlas.remote;


import com.bounswe2017.group10.atlas.BuildConfig;

import java.util.concurrent.TimeUnit;

import retrofit2.converter.gson.GsonConverterFactory;

public class APIUtils {

    private static API serverAPI  = constructServerAPI("http://ff024ce0.ngrok.io");

    public static API serverAPI() {
        return serverAPI;
    }

    public static void setServerAPI(API api) {
        serverAPI = api;
    }

    private static API constructServerAPI(String baseUrl) {
        RetrofitBuilder builder = new RetrofitBuilder()
                .baseUrl(baseUrl)
                .addConverterFactory(GsonConverterFactory.create())
                .connectTimeout(60, TimeUnit.SECONDS)
                .readTimeout(60, TimeUnit.SECONDS)
                .writeTimeout(60, TimeUnit.SECONDS);

        if (BuildConfig.DEBUG) {
            builder.addInterceptor();
        }

        return builder.build().create(API.class);
    }
}
