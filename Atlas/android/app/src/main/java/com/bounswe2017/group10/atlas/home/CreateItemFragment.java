package com.bounswe2017.group10.atlas.home;


import android.Manifest;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.text.InputType;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.ImageListAdapter;
import com.bounswe2017.group10.atlas.adapter.ImageRow;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.response.OnCreateItemResponse;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;

import java.io.IOException;
import java.util.ArrayList;


public class CreateItemFragment extends Fragment {

    private static final String TAG = "CreateItemFragment";
    private static final int FROM_GALLERY = 1;
    private static final int FROM_CAMERA = 2;
    private static final int CAMERA_REQUEST_CODE = 3;

    private ImageListAdapter mAdapter;
    private final ArrayList<ImageRow> mImageRowList = new ArrayList<>();

    private Uri currentPhotoUri = null;

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_create_item, container, false);

        ListView imageListView = view.findViewById(R.id.image_listview);
        Button btnGallery = view.findViewById(R.id.gallery_button);
        Button btnCamera = view.findViewById(R.id.camera_button);
        Button btnUrl = view.findViewById(R.id.url_button);
        Button btnCreate = view.findViewById(R.id.create_button);

        // set ImageListAdapter to imageListView
        mAdapter = new ImageListAdapter(getActivity(), mImageRowList);
        imageListView.setAdapter(mAdapter);

        btnGallery.setOnClickListener((View btnView) -> {
            Intent intent = new Intent();
            intent.setType("image/*");
            intent.setAction(Intent.ACTION_GET_CONTENT);
            startActivityForResult(Intent.createChooser(intent, "Select Image"), FROM_GALLERY);
        });

        btnCamera.setOnClickListener((View btnView) -> {
            boolean cameraPermitted = true;
            boolean writePermitted = true;
            if (android.os.Build.VERSION.SDK_INT >= 23) {
                cameraPermitted = getActivity().checkSelfPermission(Manifest.permission.CAMERA) == PackageManager.PERMISSION_GRANTED;
                writePermitted = getActivity().checkSelfPermission(Manifest.permission.WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED;
                if (!cameraPermitted || !writePermitted) {
                    getActivity().requestPermissions(new String[]{Manifest.permission.CAMERA, Manifest.permission.WRITE_EXTERNAL_STORAGE}, CAMERA_REQUEST_CODE);
                    return;
                }
            }
            try {
                currentPhotoUri = Utils.getNewImageUri(getContext());
            } catch (IOException e) {
                Log.d(TAG, e.toString());
            }
            Log.d(TAG, currentPhotoUri.toString());
            Intent intent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
            intent.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
            intent.putExtra(MediaStore.EXTRA_OUTPUT, currentPhotoUri);

            startActivityForResult(intent, FROM_CAMERA);
        });

        // construct AlertDialog that will be called on url button
        AlertDialog urlAlertDialog = createUrlAlertDialog();
        // set listener to url button
        btnUrl.setOnClickListener((View btnView) -> {
            urlAlertDialog.show();
        });

        // get input fields
        EditText etTitle = view.findViewById(R.id.title_edittext);
        EditText etDescription = view.findViewById(R.id.description_edittext);
        EditText etContinent = view.findViewById(R.id.continent_edittext);
        EditText etCountry = view.findViewById(R.id.country_edittext);
        EditText etCity = view.findViewById(R.id.city_edittext);
        ProgressBar progressBar = view.findViewById(R.id.progress_bar);
        // set listener to create button
        btnCreate.setOnClickListener((View btnView) -> {
            if (etTitle.getText().length() == 0) {
                Utils.showToast(getActivity().getApplicationContext(), getResources().getString(R.string.empty_title));
                return;
            }
            String title = etTitle.getText().toString();
            String description = etDescription.getText().toString();
            String continent = etContinent.getText().toString();
            String country = etCountry.getText().toString();
            String city = etCity.getText().toString();

            CultureItem item = new CultureItem();

            item.setTitle(title);
            if (description.length() != 0)
                item.setDescription(description);
            if (continent.length() != 0)
                item.setContinent(continent);
            if (country.length() != 0)
                item.setCountry(country);
            if (city.length() != 0)
                item.setCity(city);

            item.setPublicAccessibility(true);

            ArrayList<Image> imageList = new ArrayList<>();
            for (ImageRow row : mImageRowList) {
                Image img = new Image();
                img.setUrl(row.getUri().toString());
                imageList.add(img);
            }
            item.setImageList(imageList);
            makeCreateRequest(item, imageList, progressBar);
        });
        return view;
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (resultCode == Activity.RESULT_OK) {
            if (requestCode == FROM_GALLERY) {
                Uri selectedImage = data.getData();
                addImageFromUri(selectedImage);
            } else if (requestCode == FROM_CAMERA) {
                addImageFromUri(currentPhotoUri);
            } else {
                Log.d(TAG, "OnActivityResult wrong requestCode : " + requestCode);
            }
        } else {
            Utils.showToast(getActivity().getApplicationContext(), Integer.toString(resultCode));
        }
    }

    /**
     * Clears text and image views in this fragment.
     */
    public void clearView() {
        View view = this.getView();
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

    /**
     * Makes a new item create request to server with the given item while showing
     * the given progress bar.
     *
     * @param item CultureItem object to be sent to the server.
     * @param progressBar ProgressBar object which will be shown during request execution.
     */
    private void makeCreateRequest(CultureItem item, ArrayList<Image> imageList, ProgressBar progressBar) {
        progressBar.setVisibility(View.VISIBLE);
        Activity activity = getActivity();
        String authStr = Utils.getSharedPref(activity).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
        APIUtils.serverAPI().createItem(authStr, item).enqueue(new OnCreateItemResponse(this, imageList, progressBar));
    }

    /**
     * Adds the image in the given url to image listview on this fragment.
     *
     * If the image in the given URL is already being shown in listview, then it is not
     * added again.
     *
     * @param uri URL of the image.
     */
    private void addImageFromUri(Uri uri) {
        Log.d(TAG, uri.toString());
        ImageRow row = new ImageRow();
        row.setUri(uri);
        if (!mImageRowList.contains(row)) {
            mImageRowList.add(row);
            mAdapter.notifyDataSetChanged();
        }
    }

    /**
     * Construct an AlertDialog that will be used to get image URL from user.
     *
     * @return AlertDialog object which when shown takes a URL input and calls addImageFromUrl.
     */
    private AlertDialog createUrlAlertDialog() {
        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        builder.setTitle(getResources().getString(R.string.enter_url));

        final EditText input = new EditText(getActivity());
        input.setInputType(InputType.TYPE_CLASS_TEXT);
        builder.setView(input);

        builder.setPositiveButton(getResources().getString(R.string.ok),(DialogInterface dialog, int i) -> {
            String url = input.getText().toString();
            Uri uri = null;
            try {
                uri = Uri.parse(url);
            } catch (Exception e) {
                Log.d("IMAGE", e.toString());
            }
            input.setText("");
            addImageFromUri(uri);
        });

        builder.setNegativeButton(getResources().getString(R.string.cancel), (DialogInterface dialog, int i) -> {
            dialog.cancel();
        });

        return builder.create();
    }
}

