package com.bounswe2017.group10.atlas.home;

import android.annotation.SuppressLint;
import android.app.SearchManager;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.icu.util.UniversalTimeScale;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.design.widget.FloatingActionButton;
import android.support.design.widget.NavigationView;
import android.support.v4.app.Fragment;
import android.support.v4.view.GravityCompat;
import android.support.v4.widget.DrawerLayout;
import android.support.v7.app.ActionBar;
import android.support.v7.app.ActionBarDrawerToggle;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.SearchView;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.httpbody.GetItemsResponse;
import com.bounswe2017.group10.atlas.profil.ProfileActivity;
import com.bounswe2017.group10.atlas.response.OnGetItemsResponse;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.httpbody.UserResponse;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.util.Utils;

import java.util.List;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

import static com.bounswe2017.group10.atlas.util.Utils.getSharedPref;
import static com.bounswe2017.group10.atlas.util.Utils.logout;
import static com.bounswe2017.group10.atlas.util.Utils.showToast;


public class HomeActivity extends AppCompatActivity implements NavigationView.OnNavigationItemSelectedListener{

    private ListItemsFragment mFeedFragment;
    private ListItemsFragment mSearchItemsFragment;
    private DrawerLayout mDrawerLayout;
    private ActionBar mActionBar;
    private ActionBarDrawerToggle mDrawerToggle;
    private FloatingActionButton mFab;
    private SearchView mSearchView;

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

        mSearchItemsFragment = new ListItemsFragment();
        setUpSearchFragment();

        mFeedFragment = new ListItemsFragment();
        setUpFeedFragment();
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

                    String nameText = getString(R.string.fullname, firstName, lastName);
                    ((TextView) findViewById(R.id.nav_pname)).setText(nameText);
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

    /**
     * Set up the functionality of mSearchItemsFragment. This method sets how mSearchItemsFragment
     * requests its items from the server.
     */
    private void setUpSearchFragment() {
        mSearchItemsFragment.setRequestStrategy(new ListItemsFragment.RequestStrategy() {
            @Override
            public void requestItems(Context context, int offset, OnGetItemsResponse.GetItemCallback getItemCallback) {
                // TODO: pagination for search results
                String authStr = Utils.getSharedPref(getApplicationContext()).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
                String query = mSearchView.getQuery().toString();
                OnGetItemsResponse respHandler = new OnGetItemsResponse(context, getItemCallback);
                APIUtils.serverAPI().search(authStr, query).enqueue(respHandler);
            }
        });
        mSearchItemsFragment.addAfterItemClickedListener(() -> {
            mFab.setVisibility(View.INVISIBLE);
        });
        mSearchItemsFragment.setRequestImmediately(false);
    }

    /**
     * Set up the functionality of mFeedFragment.
     */
    private void setUpFeedFragment() {
        mFeedFragment.setRequestStrategy(new ListItemsFragment.FeedStrategy());
        mFeedFragment.addAfterItemClickedListener(() -> {
            mFab.setVisibility(View.INVISIBLE);
        });
        getSupportFragmentManager()
                .beginTransaction()
                .replace(R.id.home_container, mFeedFragment)
                .commit();
    }

    @Override
    @SuppressLint("RestrictedApi")
    public void onBackPressed() {
        mFab.setVisibility(View.VISIBLE);
        if (mDrawerLayout.isDrawerOpen(GravityCompat.START)) {
            mDrawerLayout.closeDrawer(GravityCompat.START);
        } else {
            if (!mSearchView.isIconified()) {
                mSearchView.setQuery("", false);
                mSearchView.setIconified(true);
            }
            super.onBackPressed();
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action_home bar if it is present.
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.action_home, menu);

        // Searchable configuration
        SearchManager searchManager = (SearchManager)getSystemService(Context.SEARCH_SERVICE);
        mSearchView = (SearchView)menu.findItem(R.id.search).getActionView();
        mSearchView.setSearchableInfo(searchManager.getSearchableInfo(getComponentName()));
        setupSearchView();

        return super.onCreateOptionsMenu(menu);
    }

    /**
     * Set up the functionality of mSearchView.
     */
    private void setupSearchView() {
        // when search icon is pressed, show mSearchItemsFragment.
        mSearchView.setOnSearchClickListener((View v) -> {
            Fragment currFragment = getSupportFragmentManager().findFragmentById(R.id.home_container);
            if (!currFragment.equals(mSearchItemsFragment)) {
                getSupportFragmentManager()
                        .beginTransaction()
                        .replace(R.id.home_container, mSearchItemsFragment)
                        .addToBackStack(null)
                        .commit();
            }
        });
        // when a query is submitted, clear and load items from mSearchItemsFragment.
        mSearchView.setOnQueryTextListener(new SearchView.OnQueryTextListener() {
            @Override
            public boolean onQueryTextSubmit(String query) {
                mSearchItemsFragment.clearItems();
                mSearchItemsFragment.loadMoreItems();
                return true;
            }

            @Override
            public boolean onQueryTextChange(String newText) {
                // TODO: implement autocompletion
                return true;
            }
        });
    }

    private void requestSearchResults(String query, OnGetItemsResponse.GetItemCallback getItemCallback) {
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


