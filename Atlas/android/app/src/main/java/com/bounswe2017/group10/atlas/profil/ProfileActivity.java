package com.bounswe2017.group10.atlas.profil;

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

public class ProfileActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_profile);

        String authStr = getSharedPref(this).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
        APIUtils.serverAPI().getMe(authStr).enqueue(new Callback<UserResponse>() {
            @Override
            public void onResponse(Call<UserResponse> call, Response<UserResponse> response) {
                if (response.isSuccessful()) {
                    UserResponse body = response.body();
                    ((TextView) findViewById(R.id.user_profile_name)).setText(body.getFirstname()+" "+body.getLastname());
                    ((TextView) findViewById(R.id.user_profile_email)).setText(body.getEmail());
                } else {
                    ((TextView) findViewById(R.id.user_profile_name)).setText("name");
                    ((TextView) findViewById(R.id.user_profile_email)).setText("email");
                }
            }
            @Override
            public void onFailure(Call<UserResponse> call, Throwable t) {
                ((TextView) findViewById(R.id.user_profile_name)).setText("name");
                ((TextView) findViewById(R.id.user_profile_email)).setText("email");
            }
        });

        TextView logout = findViewById(R.id.plogout);
        logout.setOnClickListener((View btnview)-> {
            // remove token from sharedpref
            Utils.getSharedPrefEditor(this).remove(Constants.AUTH_STR).apply();
            // go to authentication activity
            Intent intent = new Intent(this, AuthActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK|Intent.FLAG_ACTIVITY_NEW_TASK);
            this.startActivity(intent);
        });
    }
}
