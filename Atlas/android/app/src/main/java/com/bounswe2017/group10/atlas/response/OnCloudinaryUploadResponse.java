package com.bounswe2017.group10.atlas.response;

import android.content.Context;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.home.CreateItemFragment;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.httpbody.ImageUploadRequest;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;
import com.cloudinary.android.callback.ErrorInfo;
import com.cloudinary.android.callback.UploadCallback;

import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;


public class OnCloudinaryUploadResponse implements UploadCallback {

    private AtomicInteger uploadCount;
    private int totalCount;
    private int cultureItemId;
    private Context context;
    private CreateItemFragment createFragment;
    private List<Image> mImageList;
    private ProgressBar progressBar;

    public OnCloudinaryUploadResponse(Context context,
                                      CreateItemFragment createFragment,
                                      List<Image> imageList,
                                      ProgressBar progressBar,
                                      int cultureItemId,
                                      AtomicInteger uploadCount,
                                      int totalCount) {
        this.context = context;
        this.createFragment = createFragment;
        this.mImageList = imageList;
        this.progressBar = progressBar;
        this.cultureItemId = cultureItemId;
        this.uploadCount = uploadCount;
        this.totalCount = totalCount;
    }

    @Override
    public void onStart(String requestId) {

    }

    @Override
    public void onProgress(String requestId, long bytes, long totalBytes) {
        // TODO: show upload percentage to user
    }

    @Override
    public void onSuccess(String requestId, Map resultData) {
        // if all images are uploaded, send create request to server
        if (this.uploadCount.addAndGet(1) == totalCount) {
            // upload image URLs to atlas server.
            String authStr = Utils.getSharedPref(context).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
            APIUtils.serverAPI()
                    .uploadImages(authStr, cultureItemId, new ImageUploadRequest(mImageList))
                    .enqueue(new OnUploadImagesResponse(createFragment, progressBar));
        }
    }

    @Override
    public void onError(String requestId, ErrorInfo error) {
        Utils.showToast(context, context.getString(R.string.image_upload_error));
        // TODO: logging
    }

    @Override
    public void onReschedule(String requestId, ErrorInfo error) {
        Utils.showToast(context, context.getString(R.string.image_upload_error));
        // TODO: logging
    }
}
