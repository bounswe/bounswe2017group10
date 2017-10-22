package com.bounswe2017.group10.atlas.remote;


import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupResponse;

import retrofit2.Call;
import retrofit2.http.Body;

public class APIDelegate implements API {

    private API api;

    public APIDelegate(API api) {
        this.api = api;
    }

    @Override
    public Call<SignupResponse> signup(@Body SignupRequest body) {
        return api.signup(body);
    }

    @Override
    public Call<LoginResponse> login(@Body LoginRequest body) {
        return api.login(body);
    }
}
