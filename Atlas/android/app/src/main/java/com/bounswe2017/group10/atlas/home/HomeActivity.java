package com.bounswe2017.group10.atlas.home;

import android.annotation.SuppressLint;
import android.app.SearchManager;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
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
import android.widget.ImageView;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.ImageListAdapter;
import com.bounswe2017.group10.atlas.profile.NearbyItemsActivity;
import com.bounswe2017.group10.atlas.profile.ProfileActivity;
import com.bounswe2017.group10.atlas.response.OnGetItemsResponse;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.util.Utils;
import com.bumptech.glide.Glide;
import com.bumptech.glide.request.RequestOptions;

import static com.bounswe2017.group10.atlas.util.Utils.logout;

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

        NavigationView navigationView = findViewById(R.id.nav_view);
        View header = navigationView.getHeaderView(0);
        displayPersonalDetails(header);
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
     * Display personal details stored in SharedPreferences.
     */
    private void displayPersonalDetails(View header) {
        SharedPreferences pref = Utils.getSharedPref(this);
        String firstName = pref.getString(Constants.FIRSTNAME, "");
        String lastName = pref.getString(Constants.LASTNAME, "");
        String email = pref.getString(Constants.EMAIL, "");
        String nameText = getString(R.string.fullname, firstName, lastName);
        String image = pref.getString(Constants.PROFILE_PICTURE,"");

        if(!image.equals("")) {
            Glide.with(this)
                    .load(image)
                    .apply(new RequestOptions()
                            .placeholder(R.drawable.ic_crop_original_black_48dp)
                            .error(R.drawable.ic_crop_original_black_48dp)
                            .fallback(R.drawable.ic_crop_original_black_48dp))
                    .into((ImageView) header.findViewById(R.id.nav_pimage));
        }

        ((TextView) header.findViewById(R.id.nav_pname)).setText(nameText);
        ((TextView) header.findViewById(R.id.nav_pmail)).setText(email);
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

        if (id == R.id.profil)
        {
            Intent intent = new Intent(this, ProfileActivity.class);
            this.startActivity(intent);
        }
        else if (id == R.id.logout)
        {
            logout(this);
        }
        else if(id==R.id.nearbyitems)
        {
            Intent intent = new Intent(this, NearbyItemsActivity.class);
            this.startActivity(intent);
        }

        mDrawerLayout.closeDrawer(GravityCompat.START);
        return true;
    }
}


