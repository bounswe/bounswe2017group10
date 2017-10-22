package com.bounswe2017.group10.atlas.auth;


import com.bounswe2017.group10.atlas.httpbody.CreateItemResponse;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupResponse;
import com.bounswe2017.group10.atlas.httpbody.UserResponse;
import com.bounswe2017.group10.atlas.remote.API;

import retrofit2.Call;
import retrofit2.http.Body;
import retrofit2.http.Header;
import retrofit2.http.Path;

/**
 * This is a delegate class for API interface and is mainly intended
 * to be used for testing purposes. One possible way to use this class
 * is to replace an API instance with APIDelegate and spy on the
 * APIDelegate instance using a mocking framework such as Mockito to
 * see if a desired function call has been made.
 */
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

    @Override
    public Call<CreateItemResponse> createItem(@Header("Authorization") String authStr, @Body CultureItem body) {
        return api.createItem(authStr, body);
    }

    @Override
    public Call<CultureItem> getItem(@Header("Authorization") String authStr, @Path("id") long id) {
        return api.getItem(authStr, id);
    }

    @Override
    public Call<UserResponse> getMe(@Header("Authorization") String authStr) {
        return api.getMe(authStr);
    }
}
