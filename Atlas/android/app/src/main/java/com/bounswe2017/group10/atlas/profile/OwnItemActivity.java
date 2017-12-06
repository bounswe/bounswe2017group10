package com.bounswe2017.group10.atlas.profile;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v7.app.ActionBar;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.SearchView;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.home.ListItemsFragment;

public class OwnItemActivity extends AppCompatActivity{

    private ListItemsFragment mOwnItemFragment;
    private ListItemsFragment mMyFavFragment;
    private ListItemsFragment mSearchItemsFragment;
    private ActionBar mActionBar;
    private SearchView mSearchView;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_ownitem);
        Toolbar toolbar = findViewById(R.id.profile_toolbar);
        setSupportActionBar(toolbar);
        this.mActionBar = getSupportActionBar();
        mActionBar.setDisplayHomeAsUpEnabled(true);

        Bundle b = getIntent().getExtras();
        int value = -1; // or other values
        if(b != null)
            value = b.getInt("type");

        if(value==1)
        {
            setTitle("My Items");
            mOwnItemFragment = new ListItemsFragment();
            setUpFeedFragment();
        }
        else if(value==2)
        {
            setTitle("My Favourite Items");
            mMyFavFragment = new ListItemsFragment();
            setUpMyFavFragment();
        }
        else if(value==3)
        {

        }

        //mSearchItemsFragment = new ListItemsFragment();
        //setUpSearchFragment();


    }



    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action_home bar if it is present.
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.action_home, menu);

        // Searchable configuration
        //SearchManager searchManager = (SearchManager)getSystemService(Context.SEARCH_SERVICE);
        //mSearchView = (SearchView)menu.findItem(R.id.search).getActionView();
        //mSearchView.setSearchableInfo(searchManager.getSearchableInfo(getComponentName()));
        //setupSearchView();

        return super.onCreateOptionsMenu(menu);
    }

    /**
     * Set up the functionality of mMyFavFragment.
     */
    private void setUpMyFavFragment() {
        mMyFavFragment.setRequestStrategy(new ListItemsFragment.FavItemsStrategy());
        getSupportFragmentManager()
                .beginTransaction()
                .replace(R.id.home_container, mMyFavFragment)
                .commit();
    }

    /**
     * Set up the functionality of mOwnItemFragment.
     */
    private void setUpFeedFragment() {
        mOwnItemFragment.setRequestStrategy(new ListItemsFragment.OwnItemsStrategy());
        getSupportFragmentManager()
                .beginTransaction()
                .replace(R.id.home_container, mOwnItemFragment)
                .commit();
    }

    @Override
    @SuppressLint("RestrictedApi")
    public void onBackPressed() {
        /*if (!mSearchView.isIconified()) {
            mSearchView.setQuery("", false);
            mSearchView.setIconified(true);
        }*/
        super.onBackPressed();
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

    //private void requestSearchResults(String query, OnGetItemsResponse.GetItemCallback getItemCallback) {}

    /**
     * Set up the functionality of mSearchView.
     */
    /*private void setupSearchView() {
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
    }*/

    /**
     * Set up the functionality of mSearchItemsFragment. This method sets how mSearchItemsFragment
     * requests its items from the server.
     */
    /*private void setUpSearchFragment() {
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
        mSearchItemsFragment.setRequestImmediately(false);
    }*/
}



