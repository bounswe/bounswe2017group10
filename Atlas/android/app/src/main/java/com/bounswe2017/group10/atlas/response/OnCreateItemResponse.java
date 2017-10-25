package com.bounswe2017.group10.atlas.response;

import android.content.Context;
import android.net.Uri;
import android.view.View;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.home.CreateItemFragment;
import com.bounswe2017.group10.atlas.httpbody.CreateItemResponse;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;
import com.cloudinary.android.MediaManager;

import org.apache.commons.text.RandomStringGenerator;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

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
                int id = response.body().getId();

                RandomStringGenerator generator = new RandomStringGenerator.Builder()
                        .withinRange('a', 'z')
                        .build();
                List<Image> localImages = new ArrayList<>();
                List<String> filenames = new ArrayList<>();
                for (Image img : this.mImageList) {
                    if (Utils.isLocalUrl(img.getUrl())) {
                        filenames.add(img.getUrl());

                        String filename = generator.generate(Constants.CLOUDINARY_IMG_NAME_LENGTH);
                        img.setUrl(Utils.filenameToCloudinaryUrl(filename));
                        localImages.add(img);
                    }
                }

                // share an atomic counter between response handlers
                AtomicInteger uploadCount = new AtomicInteger(0);
                OnCloudinaryUploadResponse respHandler = new OnCloudinaryUploadResponse(
                        context,
                        createFragment,
                        mImageList,
                        progressBar,
                        id,
                        uploadCount,
                        localImages.size()
                );
                // dispatch each request after creating a resp handler for each of them.
                for (int i = 0; i < localImages.size(); ++i) {
                    MediaManager.get()
                            .upload(Uri.parse(filenames.get(i)))
                            .unsigned("wak3gala")
                            .option("public_id", Utils.cloudinaryUrlToFilename(localImages.get(i).getUrl()))
                            .callback(respHandler)
                            .dispatch();
                }
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
