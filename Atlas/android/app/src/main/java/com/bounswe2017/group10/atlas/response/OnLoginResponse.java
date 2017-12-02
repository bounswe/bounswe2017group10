package com.bounswe2017.group10.atlas.response;


import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.util.Log;
import android.view.View;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.home.HomeActivity;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.httpbody.UserResponse;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

import static com.bounswe2017.group10.atlas.util.Utils.showToast;
import static com.bounswe2017.group10.atlas.util.Utils.tokenToAuthString;

/**
 * Implement retrofit response callback interface to be used for login requests.
 */
public class OnLoginResponse implements Callback<LoginResponse> {
    private static final String TAG = "OnLoginResponse";
    private ProgressBar progress;
    private Context context;

    public OnLoginResponse(Context context, ProgressBar progress) {
        this.progress = progress;
        this.context = context;
    }

    @Override
    public void onResponse(Call<LoginResponse> call, Response<LoginResponse> response) {
        progress.setVisibility(View.GONE);
        if (response.isSuccessful()) {
            // store token in SharedPreferences
            String token = response.body().getToken();
            String authStr = Utils.tokenToAuthString(token);
            Utils.getSharedPrefEditor(context).putString(Constants.AUTH_STR, authStr).apply();
            // store user info in SharedPreferences
            storePersonalDetails(authStr);
        } else if (response.code() == 400) {
            showToast(context.getApplicationContext(), context.getResources().getString(R.string.wrong_credentials));
        }
    }

    @Override
    public void onFailure(Call<LoginResponse> call, Throwable t) {
        progress.setVisibility(View.GONE);
        showToast(context.getApplicationContext(), context.getResources().getString(R.string.connection_failure));
    }

    private void goToHomeActivity() {
        Intent intent = new Intent(context, HomeActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK|Intent.FLAG_ACTIVITY_NEW_TASK);
        context.startActivity(intent);
    }

    /**
     * Request personal user details from the server and store them in SharedPreferences.
     */
    private void storePersonalDetails(String authStr) {
        APIUtils.serverAPI().getMe(authStr).enqueue(new Callback<UserResponse>() {
            @Override
            public void onResponse(Call<UserResponse> call, Response<UserResponse> response) {
                if (response.isSuccessful()) {
                    UserResponse body = response.body();
                    SharedPreferences.Editor editor = Utils.getSharedPrefEditor(context);
                    editor.putString(Constants.FIRSTNAME, body.getFirstname()).apply();
                    editor.putString(Constants.LASTNAME, body.getLastname()).apply();
                    editor.putString(Constants.EMAIL, body.getEmail()).apply();
                    editor.putLong(Constants.USER_ID, body.getUserId()).apply();

                    // go to home
                    goToHomeActivity();
                } else {
                    showToast(context, context.getResources().getString(R.string.failed_profilgetuserinformation));
                }
            }
            @Override
            public void onFailure(Call<UserResponse> call, Throwable t) {
                showToast(context, context.getString(R.string.connection_failure));
            }
        });
    }
}
