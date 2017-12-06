package com.bounswe2017.group10.atlas.remote;


import com.bounswe2017.group10.atlas.httpbody.Comment;
import com.bounswe2017.group10.atlas.httpbody.CreateItemResponse;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.httpbody.GetItemsResponse;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.httpbody.ImageUploadRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.httpbody.PostCommentRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupResponse;
import com.bounswe2017.group10.atlas.httpbody.Tag;
import com.bounswe2017.group10.atlas.httpbody.UserResponse;

import java.util.List;

import retrofit2.Call;
import retrofit2.http.Body;
import retrofit2.http.DELETE;
import retrofit2.http.GET;
import retrofit2.http.Header;
import retrofit2.http.PATCH;
import retrofit2.http.POST;
import retrofit2.http.PUT;
import retrofit2.http.Path;
import retrofit2.http.Query;

public interface API {
    @POST("/api/auth/signup")
    Call<SignupResponse> signup(@Body SignupRequest body);

    @POST("/api/auth/login")
    Call<LoginResponse> login(@Body LoginRequest body);

    @POST("/cultural_heritage_item")
    Call<CreateItemResponse> createItem(@Header("Authorization") String authStr,
                                        @Body CultureItem body);

    @GET("/cultural_heritage_item/{id}")
    Call<CultureItem> getItem(@Header("Authorization") String authStr,
                              @Path("id") long id);

    @DELETE("/cultural_heritage_item/{id}")
    Call<Void> deleteItem(@Header("Authorization") String authStr,
                          @Path("id") long id);

    @PUT("/cultural_heritage_item/{id}")
    Call<Void> updateItem(@Header("Authorization") String authStr,
                          @Path("id") long id,
                          @Body CultureItem body);

    @POST("/cultural_heritage_item/{id}/image")
    Call<Void> uploadImages(@Header("Authorization") String authStr,
                                   @Path("id") long id,
                                   @Body ImageUploadRequest imageList);

    @GET("/cultural_heritage_item")
    Call<GetItemsResponse> getItems(@Header("Authorization") String authStr,
                                    @Query("limit") long limit,
                                    @Query("offset") long offset);

    @GET("/cultural_heritage_item/search/{query}")
    Call<GetItemsResponse> search(@Header("Authorization") String authStr,
                                  @Path("query") String query);

    @GET("/api/auth/me")
    Call<UserResponse> getMe(@Header("Authorization") String authStr);

    @GET("/tags")
    Call<List<Tag>> getAllTags(@Header("Authorization") String authStr);

    @POST("/cultural_heritage_item/{id}/comment")
    Call<Comment> postComment(@Header("Authorization") String authStr,
                                    @Path("id") long id,
                                    @Body PostCommentRequest comment);


    @POST("/user/cultural_heritage_item/{id}/favorite")
    Call<CultureItem> favoriteItem(@Header("Authorization") String authStr,
                          @Path("id") long id);

    @DELETE("/user/cultural_heritage_item/{id}/favorite")
    Call<Void> unfavoriteItem(@Header("Authorization") String authStr,
                            @Path("id") long id);
  
    @GET("/cultural_heritage_item/myitems/")
    Call<GetItemsResponse> getOwnItems(@Header("Authorization") String authStr,
                                    @Query("limit") long limit,
                                    @Query("offset") long offset);
    
    @GET("/cultural_heritage_item/recommendation")
    Call<GetItemsResponse> getRecommendedItems(@Header("Authorization") String authStr,
                                               @Query("item_id") long id,
                                               @Query("limit") long limit);

    @GET("/user/cultural_heritage_item/favorite/")
    Call<GetItemsResponse> getMyFavItems(@Header("Authorization") String authStr,
                                       @Query("limit") long limit,
                                       @Query("offset") long offset);
}
