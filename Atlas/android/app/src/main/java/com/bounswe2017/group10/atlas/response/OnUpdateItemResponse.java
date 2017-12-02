package com.bounswe2017.group10.atlas.response;

import android.content.Context;
import android.view.View;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.home.CreateItemFragment;
import com.bounswe2017.group10.atlas.util.Utils;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


public class OnUpdateItemResponse implements Callback<Void> {

    private CreateItemFragment createFragment;
    private ProgressBar progressBar;
    private Context context;

    public OnUpdateItemResponse(CreateItemFragment createFragment, ProgressBar progressBar) {
        this.createFragment = createFragment;
        this.progressBar = progressBar;
        this.context = this.createFragment.getActivity();
    }

    @Override
    public void onResponse(Call<Void> call, Response<Void> response) {
        if (response.isSuccessful()) {
            Utils.showToast(context, context.getString(R.string.succesful_edit_item));
            progressBar.setVisibility(View.GONE);
            createFragment.clearView();
            createFragment.getActivity().onBackPressed();
        } else {
            Utils.showToast(context, context.getString(R.string.error_edit_item));
        }
    }

    @Override
    public void onFailure(Call<Void> call, Throwable t) {
        Utils.showToast(context, context.getString(R.string.connection_failure));
    }
}
