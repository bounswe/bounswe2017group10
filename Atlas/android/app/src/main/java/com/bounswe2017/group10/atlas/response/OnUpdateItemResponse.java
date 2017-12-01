package com.bounswe2017.group10.atlas.response;

import android.content.Context;

import com.bounswe2017.group10.atlas.util.Utils;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


public class OnUpdateItemResponse implements Callback<Void> {

    private Context context;

    public OnUpdateItemResponse(Context context) {
        this.context = context;
    }

    @Override
    public void onResponse(Call<Void> call, Response<Void> response) {
        if (response.isSuccessful()) {
            Utils.showToast(context, "Topic successfully edited!");
        }
    }

    @Override
    public void onFailure(Call<Void> call, Throwable t) {

    }
}
