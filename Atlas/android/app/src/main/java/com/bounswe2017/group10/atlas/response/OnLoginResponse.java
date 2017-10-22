package com.bounswe2017.group10.atlas.response;


import android.content.Context;
import android.content.Intent;
import android.util.Log;
import android.view.View;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.home.HomeActivity;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.util.Constants;

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
            String token = response.body().getToken();
            startHomeActivity(token);
        } else if (response.code() == 400) {
            showToast(context.getApplicationContext(), context.getResources().getString(R.string.wrong_credentials));
        }
    }

    @Override
    public void onFailure(Call<LoginResponse> call, Throwable t) {
        progress.setVisibility(View.GONE);
        showToast(context.getApplicationContext(), context.getResources().getString(R.string.connection_failure));
        Log.d(TAG, "LOGIN connection failure: " + t.toString());
        Log.d(TAG, "LOGIN connection failure isExecuted: " + call.isExecuted());
        Log.d(TAG, "LOGIN connection failure isCanceled: " + call.isCanceled());
    }

    /**
     * Start home activity with the given token.
     *
     * @param token Access token obtained from the server.
     */
    private void startHomeActivity(String token) {
        String authStr = tokenToAuthString(token);
        Intent intent = new Intent(context, HomeActivity.class).putExtra(Constants.AUTH_STR, authStr);
        context.startActivity(intent);
    }
}
