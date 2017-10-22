package com.bounswe2017.group10.atlas.home;


import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.FragmentActivity;
import android.util.Log;

import com.bounswe2017.group10.atlas.R;

public class HomeActivity extends FragmentActivity {

    private static final String TAG = "HomeActivity";

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_home);

        Intent intent = getIntent();
        String token = intent.getStringExtra("token");
        Log.d(TAG, "Token: " + token);
    }
}
