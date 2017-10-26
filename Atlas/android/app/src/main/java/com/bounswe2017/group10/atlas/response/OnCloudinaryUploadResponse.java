package com.bounswe2017.group10.atlas.response;

import android.content.Context;
import android.net.Uri;
import android.util.Log;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.home.CreateItemFragment;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.httpbody.ImageUploadRequest;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;
import com.cloudinary.android.MediaManager;
import com.cloudinary.android.callback.ErrorInfo;
import com.cloudinary.android.callback.UploadCallback;

import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;


public class OnCloudinaryUploadResponse implements UploadCallback {

    private int cultureItemId;
    private int indexInImageList;
    private Context context;
    private CreateItemFragment createFragment;
    private List<Image> mImageList;
    private ProgressBar progressBar;

    public OnCloudinaryUploadResponse(Context context,
                                      CreateItemFragment createFragment,
                                      List<Image> imageList,
                                      ProgressBar progressBar,
                                      int cultureItemId,
                                      int indexInImageList) {
        this.context = context;
        this.createFragment = createFragment;
        this.mImageList = imageList;
        this.progressBar = progressBar;
        this.cultureItemId = cultureItemId;
        this.indexInImageList = indexInImageList;
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
        String imageUrl = "http://res.cloudinary.com/" +
                Constants.CLOUDINARY_CLOUD_NAME +
                "/image/upload/v" +
                Integer.toString((int)resultData.get("version")) +
                "/" +
                resultData.get("public_id") +
                "." +
                resultData.get("format");
        Log.d("CLOUDINARY_URL", imageUrl);
        Image img = mImageList.get(indexInImageList);
        img.setUrl(imageUrl);
        mImageList.set(indexInImageList, img);

        // find the index of next local image
        int nextLocalIndex = indexInImageList;
        for (int i = indexInImageList + 1; i < mImageList.size(); ++i) {
            if (Utils.isLocalUrl(mImageList.get(i).getUrl())) {
                nextLocalIndex = i;
                break;
            }
        }
        if (nextLocalIndex == indexInImageList) {  // no more local images
            // send images to server
            String authStr = Utils.getSharedPref(context).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
            APIUtils.serverAPI()
                    .uploadImages(authStr, cultureItemId, new ImageUploadRequest(mImageList))
                    .enqueue(new OnUploadImagesResponse(createFragment, progressBar));
        } else {  // still more local images; then start sending them.
            OnCloudinaryUploadResponse respHandler = new OnCloudinaryUploadResponse(
                    context,
                    createFragment,
                    mImageList,
                    progressBar,
                    cultureItemId,
                    nextLocalIndex
            );
            String fileUrl = mImageList.get(nextLocalIndex).getUrl();
            MediaManager.get()
                    .upload(Uri.parse(fileUrl))
                    .unsigned("wak3gala")
                    .callback(respHandler)
                    .dispatch();
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
