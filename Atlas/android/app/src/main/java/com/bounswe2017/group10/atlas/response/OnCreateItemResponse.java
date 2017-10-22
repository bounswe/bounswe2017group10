package com.bounswe2017.group10.atlas.response;

import android.content.Context;
import android.view.View;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.CreateItemResponse;
import com.bounswe2017.group10.atlas.util.Utils;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


public class OnCreateItemResponse implements Callback<CreateItemResponse> {

    private Context context;
    private ProgressBar progressBar;

    public OnCreateItemResponse(Context context, ProgressBar progressBar) {
        this.context = context;
        this.progressBar = progressBar;
    }

    @Override
    public void onResponse(Call<CreateItemResponse> call, Response<CreateItemResponse> response) {
        progressBar.setVisibility(View.GONE);
        if (response.isSuccessful()) {
            Utils.showToast(context, context.getResources().getString(R.string.successful_create_item));
        } else {
            Utils.showToast(context, "Create Item Error!");
        }
    }

    @Override
    public void onFailure(Call<CreateItemResponse> call, Throwable t) {
        Utils.showToast(context, context.getResources().getString(R.string.connection_failure));
        // TODO: do logging
    }
}
