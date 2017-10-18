package com.bounswe2017.group10.atlas.home;


import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.FragmentActivity;
import android.support.v4.view.ViewPager;

import com.bounswe2017.group10.atlas.R;


public class HomeActivity extends FragmentActivity {

    private static final String TAG = "HomeActivity";

    private TabPagerAdapter mAdapter;
    private ViewPager mPager;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_home);

        mAdapter = initAdapter();
        mPager = findViewById(R.id.pager);
        mPager.setAdapter(mAdapter);

        mPager.addOnPageChangeListener(new ViewPager.SimpleOnPageChangeListener() {
            @Override
            public void onPageSelected(int position) {
                mPager.setCurrentItem(position);
                super.onPageSelected(position);
            }
        });
        //Intent intent = getIntent();
        //String token = intent.getStringExtra("token");
        //Log.d(TAG, "Token: " + token);
    }

    private TabPagerAdapter initAdapter() {
        TabPagerAdapter adapter = new TabPagerAdapter(getSupportFragmentManager());
        adapter.addFragment(new FeedFragment(), "FeedFragment");
        adapter.addFragment(new CreateItemFragment(), "CreateItemFragment");
        return adapter;
    }
}


