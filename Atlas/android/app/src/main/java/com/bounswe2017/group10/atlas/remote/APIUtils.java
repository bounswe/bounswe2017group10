package com.bounswe2017.group10.atlas.remote;


public class APIUtils {

    private static final APIManager serverAPIManager = new APIManager("http://54.235.57.209:81/");

    public static API serverAPI() {
        return serverAPIManager.getAPI();
    }
}
