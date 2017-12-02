package com.bounswe2017.group10.atlas.test_utilities;


import com.bounswe2017.group10.atlas.httpbody.Comment;
import com.bounswe2017.group10.atlas.httpbody.CreateItemResponse;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.httpbody.GetItemsResponse;
import com.bounswe2017.group10.atlas.httpbody.ImageUploadRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.httpbody.PostCommentRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupResponse;
import com.bounswe2017.group10.atlas.httpbody.UserResponse;
import com.bounswe2017.group10.atlas.httpbody.Tag;
import com.bounswe2017.group10.atlas.remote.API;

import java.util.List;

import retrofit2.Call;
import retrofit2.http.Body;
import retrofit2.http.GET;
import retrofit2.http.Header;
import retrofit2.http.POST;
import retrofit2.http.Path;
import retrofit2.http.Query;

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
    public Call<SignupResponse> signup(SignupRequest body) {
        return api.signup(body);
    }

    @Override
    public Call<LoginResponse> login(LoginRequest body) {
        return api.login(body);
    }

    @Override
    public Call<CreateItemResponse> createItem(String authStr, CultureItem body) {
        return api.createItem(authStr, body);
    }

    @Override
    public Call<CultureItem> getItem(String authStr, long id) {
        return api.getItem(authStr, id);
    }

    @Override
    public Call<UserResponse> getMe(String authStr) {
        return api.getMe(authStr);
    }

    @Override
    public Call<Void> uploadImages(String authStr, long id,  ImageUploadRequest imageList) {
        return api.uploadImages(authStr, id, imageList);
    }

    @Override
    public Call<GetItemsResponse> getItems(String authStr, long limit, long offset) {
        return api.getItems(authStr, limit, offset);
    }

    @Override
    public Call<List<Tag>> getAllTags(@Header("Authorization") String authStr) {
        return api.getAllTags(authStr);
    }

    @Override
    public Call<Comment> postComment(String authStr, long id, PostCommentRequest pack) {
        return api.postComment(authStr,id,pack);
    }

    @Override
    public Call<Void> deleteItem(String authStr, long id) {
        return api.deleteItem(authStr, id);
    }

    @Override
    public Call<Void> updateItem(String authStr, long id, CultureItem body) {
        return api.updateItem(authStr, id, body);
    }

    @Override
    public Call<GetItemsResponse> search(String authStr, String query) {
        return api.search(authStr, query);
    }
}
