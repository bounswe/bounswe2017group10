package com.bounswe2017.group10.atlas.home;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.design.widget.FloatingActionButton;
import android.support.v4.app.FragmentActivity;
import android.view.View;

import com.bounswe2017.group10.atlas.R;

public class CreateItemActivity extends FragmentActivity {

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_create_item);

        CreateItemFragment createItemFragment = new CreateItemFragment();
        getSupportFragmentManager()
                .beginTransaction()
                .replace(R.id.create_item_container, createItemFragment)
                .commit();

        FloatingActionButton fab = findViewById(R.id.floatingActionButton);
        fab.setOnClickListener((View v) -> {
            createItemFragment.createItem();
        });
    }
}

