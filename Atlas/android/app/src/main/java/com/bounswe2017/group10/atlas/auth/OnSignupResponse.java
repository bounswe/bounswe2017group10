package com.bounswe2017.group10.atlas.auth;


import android.content.Context;
import android.view.View;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupResponse;
import com.bounswe2017.group10.atlas.remote.APIUtils;

import org.json.JSONObject;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

import static com.bounswe2017.group10.atlas.util.Utils.showToast;

/**
 * Implement retrofit response callback interface to be used for signup requests.
 */
public class OnSignupResponse implements Callback<SignupResponse> {
    private SignupRequest origRequest;
    private Context context;
    private ProgressBar progress;

    OnSignupResponse(Context context, ProgressBar progress, SignupRequest origRequest) {
        this.origRequest = origRequest;
        this.progress = progress;
        this.context = context;
    }

    @Override
    public void onResponse(Call<SignupResponse> call, Response<SignupResponse> response) {
        if (response.isSuccessful()) {
            LoginRequest loginRequest = new LoginRequest();
            loginRequest.setUsernameOrEmail(origRequest.getUsername());
            loginRequest.setPassword(origRequest.getPassword());
            APIUtils.serverAPI().login(loginRequest).enqueue(new OnLoginResponse(context, progress));
        } else {
            try {
                JSONObject jObjError = new JSONObject(response.errorBody().string());
                if(jObjError.has("username"))
                    showToast(context, jObjError.getString("username"));
                else if(jObjError.has("email"))
                    showToast(context, jObjError.getString("email"));
                else if(jObjError.has("non_field_errors"))
                    showToast(context, jObjError.getString("non_field_errors"));
                else if(jObjError.has("firstname"))
                    showToast(context, jObjError.getString("firstname"));
                else if(jObjError.has("lastname"))
                    showToast(context, jObjError.getString("lastname"));
                else
                    showToast(context,"couldn't signup");
            } catch (Exception e) {
                showToast(context, e.getMessage());
            }
            progress.setVisibility(View.INVISIBLE);
        }
    }

    @Override
    public void onFailure(Call<SignupResponse> call, Throwable t) {
    }
}
