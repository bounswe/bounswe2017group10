package com.bounswe2017.group10.atlas.response;

import android.content.Context;
import android.support.v4.app.Fragment;
import android.view.View;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.ImageRow;
import com.bounswe2017.group10.atlas.home.CreateItemFragment;
import com.bounswe2017.group10.atlas.httpbody.CreateItemResponse;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;

import java.util.List;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


public class OnUploadImagesResponse implements Callback<Void> {

    private CreateItemFragment createFragment;
    private ProgressBar progressBar;
    private Context context;

    public OnUploadImagesResponse(CreateItemFragment createFragment, ProgressBar progressBar) {
        this.createFragment = createFragment;
        this.progressBar = progressBar;
        this.context = this.createFragment.getActivity();
    }

    @Override
    public void onResponse(Call<Void> call, Response<Void> response) {
        this.progressBar.setVisibility(View.GONE);
        if (response.isSuccessful()) {
            Utils.showToast(context, context.getResources().getString(R.string.successful_create_item));
            createFragment.clearView();
        } else {
            Utils.showToast(context, context.getString(R.string.create_item_no_images));
        }
    }

    @Override
    public void onFailure(Call<Void> call, Throwable t) {
        Utils.showToast(context, context.getResources().getString(R.string.connection_failure));
        // TODO: do logging
    }
}

