package com.bounswe2017.group10.atlas.remote;


public class APIUtils {

    private APIUtils() {
    }

    public static final String BASE_URL = "http://54.235.57.209:81/";

    private static API api = null;

    public static API getAPI() {
        if (api == null) {
            api = RetrofitClient.getClient(BASE_URL).create(API.class);
        }
        return api;
    }
}
