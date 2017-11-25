package com.bounswe2017.group10.atlas.home;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.design.widget.FloatingActionButton;
import android.support.design.widget.NavigationView;
import android.support.v4.view.GravityCompat;
import android.support.v4.widget.DrawerLayout;
import android.support.v7.app.ActionBar;
import android.support.v7.app.ActionBarDrawerToggle;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.profil.ProfileActivity;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.httpbody.UserResponse;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.util.Utils;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

import static com.bounswe2017.group10.atlas.util.Utils.getSharedPref;
import static com.bounswe2017.group10.atlas.util.Utils.logout;
import static com.bounswe2017.group10.atlas.util.Utils.showToast;


public class HomeActivity extends AppCompatActivity implements NavigationView.OnNavigationItemSelectedListener{

    private DrawerLayout mDrawerLayout;
    private ActionBar mActionBar;
    private ActionBarDrawerToggle mDrawerToggle;
    private FloatingActionButton mFab;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.navigate_bar);
        Toolbar toolbar = findViewById(R.id.home_toolbar);
        setSupportActionBar(toolbar);
        this.mActionBar = getSupportActionBar();
        mActionBar.setDisplayHomeAsUpEnabled(true);
        mActionBar.setHomeAsUpIndicator(R.drawable.ic_menu_black_24dp);
        mDrawerLayout = findViewById(R.id.nav_layout);
        mDrawerToggle = new ActionBarDrawerToggle(this, mDrawerLayout, toolbar, R.string.app_name, R.string.app_name);
        mDrawerToggle.setToolbarNavigationClickListener((View v) -> {
            if (mDrawerLayout.isDrawerVisible(GravityCompat.START)) {
                mDrawerLayout.closeDrawer(GravityCompat.START);
            } else {
                mDrawerLayout.openDrawer(GravityCompat.START);
            }
        });

        storePersonalDetails();

        NavigationView navigationView = findViewById(R.id.nav_view);
        navigationView.setNavigationItemSelectedListener(this);

        mFab = findViewById(R.id.floatingActionButton);
        mFab.setOnClickListener((View v) -> {
            Intent intent = new Intent(this, CreateItemActivity.class);
            startActivity(intent);
        });

        ListItemsFragment listItemsFragment = new ListItemsFragment();
        listItemsFragment.setRequestStrategy(new ListItemsFragment.FeedStrategy());
        listItemsFragment.addAfterItemClickedListener(() -> {
            mFab.setVisibility(View.INVISIBLE);
        });
        getSupportFragmentManager()
                .beginTransaction()
                .replace(R.id.home_container, listItemsFragment)
                .commit();

    }

    /**
     * Request personal user details from the server and store them in SharedPreferences.
     */
    private void storePersonalDetails() {
        //add username and email to navigation bar
        String authStr = getSharedPref(this).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
        APIUtils.serverAPI().getMe(authStr).enqueue(new Callback<UserResponse>() {
            @Override
            public void onResponse(Call<UserResponse> call, Response<UserResponse> response) {
                if (response.isSuccessful()) {
                    UserResponse body = response.body();
                    SharedPreferences.Editor editor = Utils.getSharedPrefEditor(getApplicationContext());
                    editor.putString(Constants.FIRSTNAME, body.getFirstname()).apply();
                    editor.putString(Constants.LASTNAME, body.getLastname()).apply();
                    editor.putString(Constants.EMAIL, body.getEmail()).apply();
                    editor.putLong(Constants.USER_ID, body.getUserId()).apply();

                    String firstName = body.getFirstname();
                    if (firstName == null) firstName = "";
                    String lastName = body.getLastname();
                    if (lastName == null) lastName = "";

                    ((TextView) findViewById(R.id.nav_pname)).setText(firstName + " " + lastName);
                    ((TextView) findViewById(R.id.nav_pmail)).setText(body.getEmail());
                } else {
                    showToast(getApplicationContext(), getResources().getString(R.string.failed_profilgetuserinformation));
                }
            }
            @Override
            public void onFailure(Call<UserResponse> call, Throwable t) {
                showToast(getApplicationContext(), getResources().getString(R.string.connection_failure));
            }
        });
    }

    @Override
    @SuppressLint("RestrictedApi")
    public void onBackPressed() {
        mFab.setVisibility(View.VISIBLE);
        if (mDrawerLayout.isDrawerOpen(GravityCompat.START)) {
            mDrawerLayout.closeDrawer(GravityCompat.START);
        } else {
            super.onBackPressed();
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action_home bar if it is present.
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.action_home, menu);
        return super.onCreateOptionsMenu(menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.action_settings:
                // show the app settings
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    @SuppressWarnings("StatementWithEmptyBody")
    @Override
    public boolean onNavigationItemSelected(MenuItem item) {
        // Handle navigation view item clicks here.
        int id = item.getItemId();

        if (id == R.id.profil) {
            Intent intent = new Intent(this, ProfileActivity.class);
            this.startActivity(intent);
        } else if(id == R.id.gallery){

        } else if (id == R.id.logout) {
            logout(this);
        }

        mDrawerLayout.closeDrawer(GravityCompat.START);
        return true;
    }
}


