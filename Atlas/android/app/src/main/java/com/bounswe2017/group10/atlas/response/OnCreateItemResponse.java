package com.bounswe2017.group10.atlas.response;

import android.content.Context;
import android.support.v4.app.Fragment;
import android.view.View;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.ImageRow;
import com.bounswe2017.group10.atlas.home.CreateItemFragment;
import com.bounswe2017.group10.atlas.httpbody.CreateItemResponse;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.httpbody.ImageUploadRequest;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;

import java.util.List;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


public class OnCreateItemResponse implements Callback<CreateItemResponse> {

    private CreateItemFragment createFragment;
    private ProgressBar progressBar;
    private List<Image> mImageList;
    private Context context;

    public OnCreateItemResponse(CreateItemFragment createFragment, List<Image> imageList, ProgressBar progressBar) {
        this.createFragment = createFragment;
        this.progressBar = progressBar;
        this.mImageList = imageList;
        this.context = this.createFragment.getActivity();
    }

    @Override
    public void onResponse(Call<CreateItemResponse> call, Response<CreateItemResponse> response) {
        if (response.isSuccessful()) {
            if (mImageList.size() == 0) {
                Utils.showToast(context, context.getString(R.string.successful_create_item));
                progressBar.setVisibility(View.GONE);
                createFragment.clearView();
            } else {
                String authStr = Utils.getSharedPref(context).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
                int id = response.body().getId();
                APIUtils.serverAPI()
                        .uploadImages(authStr, id, new ImageUploadRequest(mImageList))
                        .enqueue(new OnUploadImagesResponse(createFragment, progressBar));
            }
        } else {
            Utils.showToast(context, context.getString(R.string.failed_create_item));
        }
    }

    @Override
    public void onFailure(Call<CreateItemResponse> call, Throwable t) {
        Utils.showToast(context, context.getResources().getString(R.string.connection_failure));
        // TODO: do logging
    }
}
