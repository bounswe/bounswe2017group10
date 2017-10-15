package com.bounswe2017.group10.atlas.auth;

import android.util.Log;

import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupResponse;
import com.bounswe2017.group10.atlas.remote.API;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.remote.ResponseCallback;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


class AuthManager {

    private static final API api = APIUtils.getAPI();

    /**
     * Makes a login request with the given request body. One of the given callback methods are called
     * depending on the response type.
     *
     * @param body Body of the response as a POJO.
     * @param onSuccess ResponseCallback object to be called on successful request.
     * @param onFailure ResponseCallback object to be called on failed request.
     */
    static void login(LoginRequest body, ResponseCallback<LoginResponse> onSuccess, ResponseCallback<LoginResponse> onFailure) {
        Log.d("LOGIN", body.getUsername() + body.getEmail() + body.getPassword());
        api.login(body).enqueue(new Callback<LoginResponse>() {
            @Override
            public void onResponse(Call<LoginResponse> call, Response<LoginResponse> response) {
                if (response.isSuccessful()) {
                    onSuccess.onResponse(response.body());
                } else {
                    // do logging
                    onFailure.onResponse(response.body());
                }
            }

            @Override
            public void onFailure(Call<LoginResponse> call, Throwable t) {
                // do logging; can't do much on network failure.
            }
        });
    }

    /**
     * Makes a signup request with the given request body. One of the given callback methods are called
     * depending on the response type.
     *
     * @param body Body of the response as a POJO.
     * @param onSuccess ResponseCallback object to be called on successful request.
     * @param onFailure ResponseCallback object to be called on failed request.
     */
    static void signup(SignupRequest body, ResponseCallback<SignupResponse> onSuccess, ResponseCallback<SignupResponse> onFailure) {
        api.signup(body).enqueue(new Callback<SignupResponse>() {
            @Override
            public void onResponse(Call<SignupResponse> call, Response<SignupResponse> response) {
                if (response.isSuccessful()) {
                    onSuccess.onResponse(response.body());
                } else {
                    // do logging
                    onFailure.onResponse(response.body());
                }
            }

            @Override
            public void onFailure(Call<SignupResponse> call, Throwable t) {
                // do logging; can't do much on network failure.
            }
        });
    }
}
