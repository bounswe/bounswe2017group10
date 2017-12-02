package com.bounswe2017.group10.atlas.remote;


import android.content.Context;
import android.net.Uri;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;
import com.cloudinary.android.MediaManager;
import com.cloudinary.android.callback.ErrorInfo;
import com.cloudinary.android.callback.UploadCallback;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Uploader class that uploads all the images given to it to Cloudinary.
 */
public class CloudinaryUploader {

    /**
     * Response callback whose methods will be called when uploading finishes.
     */
    public interface OnResponseCallback {
        /**
         * Callback method that will be called when all the images are uploaded to
         * Cloudinary successfully.
         *
         * @param imageList List of Image objects whose URL fields contain the
         *                  Cloudinary URL of the images.
         */
        void onSuccess(List<Image> imageList);

        /**
         * Callback method that will be called when there is an error during
         * uploading the images to Cloudinary.
         *
         * @param msg Error message
         */
        void onFail(String msg);
    }

    /**
     * Callback object that will incrementally upload all the images to Cloudinary.
     * When an image is uploaded successfully, uploading of the next image will
     * start. When all the images are uploaded, OnResponseCallback.onSuccess is
     * called.
     */
    private UploadCallback mRespHandler = new UploadCallback() {
        @Override
        public void onStart(String requestId) {
        }

        @Override
        public void onProgress(String requestId, long bytes, long totalBytes) {
            // TODO: give feedback to user
        }

        @Override
        public void onSuccess(String requestId, Map resultData) {
            // add image with new URL to return list
            Image img = new Image();
            img.setUrl(getCloudinaryUrl(resultData));
            mUploadedImages.add(img);

            ++mNextIndex;
            boolean moreImagesToUpload = mNextIndex < mImagesToBeUploaded.size();
            if (moreImagesToUpload) {
                // continue uploading
                String fileUrl = mImagesToBeUploaded.get(mNextIndex).getUrl();
                MediaManager.get()
                        .upload(Uri.parse(fileUrl))
                        .unsigned("wak3gala")
                        .callback(mRespHandler)
                        .dispatch();
            } else {
                // return successfully
                mCallback.onSuccess(mUploadedImages);
            }
        }

        @Override
        public void onError(String requestId, ErrorInfo error) {
            mCallback.onFail(mContext.getString(R.string.image_upload_error));
        }

        @Override
        public void onReschedule(String requestId, ErrorInfo error) {
            mCallback.onFail(mContext.getString(R.string.connection_failure));
        }
    };

    private Context mContext;
    private List<Image> mImagesToBeUploaded;
    private List<Image> mUploadedImages;
    private OnResponseCallback mCallback;
    private int mNextIndex;

    /**
     * Constructor.
     *
     * @param context Context that will be used to get error Strings from R.string
     * @param imagesToBeUploaded List of Image objects to upload to Cloudinary.
     * @param callback OnResponseCallback object whose methods will be called upon
     *                 upload finishing.
     */
    public CloudinaryUploader(Context context, List<Image> imagesToBeUploaded, OnResponseCallback callback) {
        this.mContext = context;
        this.mImagesToBeUploaded = imagesToBeUploaded;
        this.mUploadedImages = new ArrayList<>();
        this.mCallback = callback;
        this.mNextIndex = 0;
    }

    /**
     * Start uploading procedure.
     */
    public void startUpload() {
        if (mImagesToBeUploaded.isEmpty()) {
            mCallback.onSuccess(mUploadedImages);
        } else {
            Utils.showToast(mContext, mContext.getString(R.string.uploading_local_images));
            String fileUrl = mImagesToBeUploaded.get(mNextIndex).getUrl();
            MediaManager.get()
                    .upload(Uri.parse(fileUrl))
                    .unsigned("wak3gala")
                    .callback(mRespHandler)
                    .dispatch();
        }
    }

    /**
     * Get the URL of an image that was uploaded to Cloudinary.
     *
     * @param resultData Map object that was returned on UploadCallback.onSuccess
     * @return A String of the form "http://res.cloudinary.com/..."
     */
    private String getCloudinaryUrl(Map resultData) {
        return "http://res.cloudinary.com/" +
                Constants.CLOUDINARY_CLOUD_NAME +
                "/image/upload/v" +
                Integer.toString((int)resultData.get("version")) +
                "/" +
                resultData.get("public_id") +
                "." +
                resultData.get("format");
    }
}
