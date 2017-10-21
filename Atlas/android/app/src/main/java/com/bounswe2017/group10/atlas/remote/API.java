package com.bounswe2017.group10.atlas.remote;


import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupResponse;

import okhttp3.HttpUrl;
import retrofit2.Call;
import retrofit2.http.Body;
import retrofit2.http.POST;

public interface API {
    @POST("/api/auth/signup")
    Call<SignupResponse> signup(@Body SignupRequest body);

    @POST("/api/auth/login")
    Call<LoginResponse> login(@Body LoginRequest body);
}
