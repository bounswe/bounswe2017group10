package com.bounswe2017.group10.atlas.home;

import android.content.res.Configuration;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.support.v7.app.ActionBarDrawerToggle;
import android.support.v7.app.AppCompatActivity;
import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.design.widget.FloatingActionButton;
import android.support.design.widget.NavigationView;
import android.support.design.widget.Snackbar;
import android.support.design.widget.TabLayout;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.view.GravityCompat;
import android.support.v4.view.ViewPager;
import android.support.v4.widget.DrawerLayout;
import android.support.v7.app.ActionBarDrawerToggle;
import android.support.v7.widget.Toolbar;
import android.view.Gravity;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ListView;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.TabPagerAdapter;
import com.bounswe2017.group10.atlas.util.Constants;


public class HomeActivity extends FragmentActivity implements NavigationView.OnNavigationItemSelectedListener{

    private static final String TAG = "HomeActivity";


    private String authStr;
    private TabPagerAdapter mAdapter;
    private ViewPager mPager;
    private TabLayout mTabs;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        authStr = getIntent().getStringExtra(Constants.AUTH_STR);
        setContentView(R.layout.navigate_bar);

        NavigationView navigationView = findViewById(R.id.nav_view);
        navigationView.setNavigationItemSelectedListener(this);

        // set swiping
        mAdapter = initAdapter();
        mTabs = findViewById(R.id.tabs);
        mPager = findViewById(R.id.pager);
        mPager.setAdapter(mAdapter);
        mTabs.setupWithViewPager(mPager);

        mPager.addOnPageChangeListener(new ViewPager.SimpleOnPageChangeListener() {
            @Override
            public void onPageSelected(int position) {
                mPager.setCurrentItem(position);
                super.onPageSelected(position);
            }
        });
    }

    @Override
    public void onBackPressed() {
        DrawerLayout drawer = (DrawerLayout) findViewById(R.id.nav_layout);
        if (drawer.isDrawerOpen(GravityCompat.START)) {
            drawer.closeDrawer(GravityCompat.START);
        } else {
            super.onBackPressed();
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.main, menu);
        return true;
    }

    @SuppressWarnings("StatementWithEmptyBody")
    @Override
    public boolean onNavigationItemSelected(MenuItem item) {
        // Handle navigation view item clicks here.
        int id = item.getItemId();
        System.out.println("girdi buraya");

        if (id == R.id.gallery) {
            TextView t = (TextView) findViewById(R.id.nav_pname);
            t.setText("degisti");
        } else if (id == R.id.logout) {
            TextView t = (TextView) findViewById(R.id.nav_pname);
            t.setText("degisti");
        }

        DrawerLayout drawer = (DrawerLayout) findViewById(R.id.nav_layout);
        drawer.closeDrawer(GravityCompat.START);
        return true;
    }

    private TabPagerAdapter initAdapter() {
        Bundle bundle = new Bundle();
        bundle.putString(Constants.AUTH_STR, authStr);

        Fragment feedFragment = new FeedFragment();
        feedFragment.setArguments(bundle);
        Fragment createItemFragment = new CreateItemFragment();
        createItemFragment.setArguments(bundle);

        TabPagerAdapter adapter = new TabPagerAdapter(getSupportFragmentManager());
        adapter.addFragment(feedFragment, getResources().getString(R.string.feed));
        adapter.addFragment(createItemFragment, getResources().getString(R.string.create));
        return adapter;
    }
}


