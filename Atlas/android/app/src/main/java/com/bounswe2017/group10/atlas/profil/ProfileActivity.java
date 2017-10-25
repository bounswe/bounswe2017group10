package com.bounswe2017.group10.atlas.profil;

import android.content.Context;
import android.content.Intent;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.View;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.auth.AuthActivity;
import com.bounswe2017.group10.atlas.httpbody.UserResponse;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

import static com.bounswe2017.group10.atlas.util.Utils.getSharedPref;
import static com.bounswe2017.group10.atlas.util.Utils.logout;
import static com.bounswe2017.group10.atlas.util.Utils.showToast;

public class ProfileActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_profile);

        Context appContext = getApplicationContext();

        String authStr = getSharedPref(this).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
        APIUtils.serverAPI().getMe(authStr).enqueue(new Callback<UserResponse>() {
            @Override
            public void onResponse(Call<UserResponse> call, Response<UserResponse> response) {
                if (response.isSuccessful()) {
                    UserResponse body = response.body();
                    if(body.getFirstname()==null){
                        ((TextView) findViewById(R.id.user_profile_name)).setText(body.getUsername());
                    }else if (body.getLastname()==null){
                        ((TextView) findViewById(R.id.user_profile_name)).setText(body.getFirstname());
                    }else {
                        ((TextView) findViewById(R.id.user_profile_name)).setText(body.getFirstname() + " " + body.getLastname());
                    }
                    ((TextView) findViewById(R.id.user_profile_email)).setText(body.getEmail());
                } else {
                    showToast(appContext, getResources().getString(R.string.failed_profilgetuserinformation));
                }
            }
            @Override
            public void onFailure(Call<UserResponse> call, Throwable t) {
                showToast(appContext, getResources().getString(R.string.connection_failure));
            }
        });

        TextView logouttext = findViewById(R.id.plogout);
        logouttext.setOnClickListener((View btnview)-> {
            logout(appContext);
        });
    }
}
