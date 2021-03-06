package com.bounswe2017.group10.atlas.home;

import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.design.widget.FloatingActionButton;
import android.support.v4.app.FragmentActivity;
import android.view.View;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.util.Constants;

public class CreateItemActivity extends FragmentActivity {

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_create_item);

        if (savedInstanceState == null) {
            Intent intent = getIntent();
            Bundle arguments = intent.getExtras();
            CreateItemFragment createItemFragment = new CreateItemFragment();
            createItemFragment.setArguments(arguments);
            getSupportFragmentManager()
                    .beginTransaction()
                    .replace(R.id.create_item_container, createItemFragment, "CREATE_FRAGMENT")
                    .commit();
            FloatingActionButton fab = findViewById(R.id.floatingActionButton);
            fab.setOnClickListener((View v) -> {
                createItemFragment.makeRequest();
            });
        } else {
            CreateItemFragment createItemFragment = (CreateItemFragment) getSupportFragmentManager()
                    .findFragmentByTag("CREATE_FRAGMENT");
            FloatingActionButton fab = findViewById(R.id.floatingActionButton);
            fab.setOnClickListener((View v) -> {
                createItemFragment.makeRequest();
            });
        }
    }
}

