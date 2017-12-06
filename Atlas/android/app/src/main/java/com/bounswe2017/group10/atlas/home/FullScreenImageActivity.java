package com.bounswe2017.group10.atlas.home;


import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.LinearSnapHelper;
import android.support.v7.widget.RecyclerView;
import android.support.v7.widget.SnapHelper;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.ImageListAdapter;
import com.bounswe2017.group10.atlas.adapter.ImageRow;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.util.Constants;

import java.io.Serializable;
import java.util.ArrayList;

public class FullScreenImageActivity extends AppCompatActivity {

    private RecyclerView mRecyclerView;
    private ImageListAdapter mImgListAdapter;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_fullscreen_img);

        Intent intent = getIntent();

        ArrayList<Image> imgList = (ArrayList<Image>)intent.getSerializableExtra(Constants.IMAGE_LIST);
        ArrayList<ImageRow> imgRowList = new ArrayList<>();
        for (Image img : imgList) {
           imgRowList.add(img.toImageRow());
        }

        mImgListAdapter = new ImageListAdapter(this, imgRowList, null);
        mImgListAdapter.setCenterCropped(false);
        SnapHelper helper = new LinearSnapHelper();
        mRecyclerView = findViewById(R.id.img_recyclerview);
        helper.attachToRecyclerView(mRecyclerView);
        mRecyclerView.setAdapter(mImgListAdapter);

        
    }
}
