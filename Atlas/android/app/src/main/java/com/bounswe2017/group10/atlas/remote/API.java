package com.bounswe2017.group10.atlas.remote;


import com.bounswe2017.group10.atlas.httpbody.CreateItemResponse;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupResponse;

import retrofit2.Call;
import retrofit2.http.Body;
import retrofit2.http.GET;
import retrofit2.http.Header;
import retrofit2.http.POST;
import retrofit2.http.Path;

public interface API {
    @POST("/api/auth/signup")
    Call<SignupResponse> signup(@Body SignupRequest body);

    @POST("/api/auth/login")
    Call<LoginResponse> login(@Body LoginRequest body);

    @POST("/cultural_heritage_item")
    Call<CreateItemResponse> createItem(@Header("Authorization") String authStr, @Body CultureItem body);

    @GET("/cultural_heritage_item/{id}")
    Call<CultureItem> getItem(@Header("Authorization") String authStr, @Path("id") long id);
}
