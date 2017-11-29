package com.bounswe2017.group10.atlas.profil;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.auth.AuthActivity;
import com.bounswe2017.group10.atlas.home.HomeActivity;
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
        SharedPreferences pref = Utils.getSharedPref(this);
        String firstName = pref.getString(Constants.FIRSTNAME, "");
        String lastName = pref.getString(Constants.LASTNAME, "");
        String email = pref.getString(Constants.EMAIL, "");

        ((TextView) findViewById(R.id.user_profile_name)).setText(firstName + " " + lastName);
        ((TextView) findViewById(R.id.user_profile_email)).setText(email);

        TextView logouttext = findViewById(R.id.plogout);
        logouttext.setOnClickListener((View btnview)-> {
            logout(appContext);
        });

        TextView myItem = findViewById(R.id.pmyitem);
        myItem.setOnClickListener((View btnview)->{
            Intent intent = new Intent(this, OwnItemActivity.class);
            startActivity(intent);
        } );
    }
}
