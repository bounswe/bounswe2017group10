package com.bounswe2017.group10.atlas.response;

import android.content.Context;
import android.support.v4.app.Fragment;
import android.view.View;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.ImageRow;
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

    private Fragment createFragment;
    private ProgressBar progressBar;
    private List<ImageRow> mImageRowList;
    private Context context;

    public OnUploadImagesResponse(Fragment createFragment, List<ImageRow> imageRowList, ProgressBar progressBar) {
        this.createFragment = createFragment;
        this.progressBar = progressBar;
        this.mImageRowList = imageRowList;
        this.context = this.createFragment.getActivity();
    }

    @Override
    public void onResponse(Call<Void> call, Response<Void> response) {
        this.progressBar.setVisibility(View.GONE);
        if (response.isSuccessful()) {
            Utils.showToast(context, context.getResources().getString(R.string.successful_create_item));
            clearCreateFragment();
        } else {
            Utils.showToast(context, "Upload image error");
        }
    }

    @Override
    public void onFailure(Call<Void> call, Throwable t) {
        Utils.showToast(context, context.getResources().getString(R.string.connection_failure));
        // TODO: do logging
    }

    /**
     * Cleare text fields and image list views in CreateFragment.
     */
    private void clearCreateFragment() {
        View view = createFragment.getView();
        EditText etTitle = view.findViewById(R.id.title_edittext);
        EditText etDescription = view.findViewById(R.id.description_edittext);
        EditText etContinent = view.findViewById(R.id.continent_edittext);
        EditText etCountry = view.findViewById(R.id.country_edittext);
        EditText etCity = view.findViewById(R.id.city_edittext);

        etTitle.setText("");
        etDescription.setText("");
        etContinent.setText("");
        etCountry.setText("");
        etCity.setText("");
        mImageRowList.clear();
    }
}

