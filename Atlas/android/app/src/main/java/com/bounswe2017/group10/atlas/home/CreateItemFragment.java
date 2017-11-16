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
import android.support.v7.widget.RecyclerView;
import android.text.Editable;
import android.text.InputType;
import android.text.TextWatcher;
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
import com.bounswe2017.group10.atlas.adapter.TagListAdapter;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.httpbody.Tag;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.response.OnCreateItemResponse;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;


public class CreateItemFragment extends Fragment {

    private static final String TAG = "CreateItemFragment";
    private static final int FROM_GALLERY = 1;
    private static final int FROM_CAMERA = 2;
    private static final int CAMERA_REQUEST_CODE = 3;

    private ImageListAdapter mImageAdapter;
    private final ArrayList<ImageRow> mImageRowList = new ArrayList<>();

    private TagListAdapter mTagAdapter;
    private final ArrayList<Tag> mTagList = new ArrayList<>();

    private Uri currentPhotoUri = null;

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_create_item, container, false);

        // set adapters
        ListView imageListView = view.findViewById(R.id.image_listview);
        RecyclerView tagRecyclerview = view.findViewById(R.id.tag_recyclerview);
        setAdapters(tagRecyclerview, imageListView);

        // handle tags
        EditText etTags = view.findViewById(R.id.tag_edittext);
        setTagListeners(etTags);

        // handle gallery feature
        Button btnGallery = view.findViewById(R.id.gallery_button);
        setGalleryListener(btnGallery);

        // handle camera feature
        Button btnCamera = view.findViewById(R.id.camera_button);
        setCameraListener(btnCamera);

        // handle url image feature
        Button btnUrl = view.findViewById(R.id.url_button);
        setURLListener(btnUrl);

        // handle item creation
        Button btnCreate = view.findViewById(R.id.create_button);
        EditText etTitle = view.findViewById(R.id.title_edittext);
        EditText etDescription = view.findViewById(R.id.description_edittext);
        EditText etContinent = view.findViewById(R.id.continent_edittext);
        EditText etCountry = view.findViewById(R.id.country_edittext);
        EditText etCity = view.findViewById(R.id.city_edittext);
        ProgressBar progressBar = view.findViewById(R.id.progress_bar);
        setCreateItemListener(btnCreate, etTitle, etDescription, etContinent, etCountry, etCity, progressBar);

        return view;
    }

    /**
     * Sets the adapters required by ListView or RecyclerView objects in this fragment.
     *
     * @param tagRecyclerView RecyclerView object responsible for viewing tags horizontally.
     * @param imageListView ListView object responsible for viewing added images vertically.
     */
    private void setAdapters(RecyclerView tagRecyclerView, ListView imageListView) {
        // set TagListAdapter to tagRecyclerView
        mTagAdapter = new TagListAdapter(getActivity(), mTagList, (List<Tag> tagList, int position) -> {
            tagList.remove(position);
            mTagAdapter.notifyDataSetChanged();
        });
        tagRecyclerView.setAdapter(mTagAdapter);

        // set ImageListAdapter to imageListView
        mImageAdapter = new ImageListAdapter(getActivity(), mImageRowList);
        imageListView.setAdapter(mImageAdapter);
    }

    /**
     * Sets a listener to tag edittext which creates a new Tag object whenever one of the
     * characters in Constants.TAG_SEPARATORS is entered
     *
     * @param etTags EditText object responsible for entering new tags
     */
    private void setTagListeners(EditText etTags) {
        etTags.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {
                // No implementation required for now
            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
                // No implementation required for now
            }

            @Override
            public void afterTextChanged(Editable s) {
                String textEntered = s.toString();
                int len = textEntered.length();
                String lastChar = textEntered.substring(len - 1);
                if (Constants.TAG_SEPARATORS.contains(lastChar)) {
                    // clear edittext
                    s.clear();
                    // add tag if it is not already added
                    String textWithoutSpace = textEntered.substring(0, len - 1);
                    Tag tagToAdd = new Tag(textWithoutSpace);
                    if (!mTagList.contains(tagToAdd)) {
                        mTagList.add(tagToAdd);
                        mTagAdapter.notifyDataSetChanged();
                    }
                }
            }
        });
    }

    /**
     * Sets listener for gallery button. Gallery button opens the local device
     * gallery and picks a single image from it.
     *
     * @param btnGallery Button that opens the local device gallery.
     *
     * TODO: Add support for getting multiple images from gallery at the same time.
     */
    private void setGalleryListener(Button btnGallery) {
        btnGallery.setOnClickListener((View btnView) -> {
            Intent intent = new Intent();
            intent.setType("image/*");
            intent.setAction(Intent.ACTION_GET_CONTENT);
            startActivityForResult(Intent.createChooser(intent, "Select Image"), FROM_GALLERY);
        });
    }

    /**
     * Sets listener for camera button. Camera button opens device camera
     * and captures a single image from it.
     *
     * @param btnCamera Button that opens the camera.
     *
     * TODO: Add support for capturing multiple images one after another, and adding them all.
     */
    private void setCameraListener(Button btnCamera) {
        btnCamera.setOnClickListener((View btnView) -> {
            // request permissions on-the-fly if device has API >= 23
            if (android.os.Build.VERSION.SDK_INT >= 23) {
                boolean cameraPermitted = getActivity().checkSelfPermission(Manifest.permission.CAMERA) == PackageManager.PERMISSION_GRANTED;
                boolean writePermitted = getActivity().checkSelfPermission(Manifest.permission.WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED;
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
            Intent intent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
            intent.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
            intent.putExtra(MediaStore.EXTRA_OUTPUT, currentPhotoUri);

            startActivityForResult(intent, FROM_CAMERA);
        });
    }

    /**
     * Sets listener for URL button. URL button opens a text dialog in which user
     * can enter the URL of a media item from the web.
     *
     * @param btnUrl Button that opens the URL text dialog.
     */
    private void setURLListener(Button btnUrl) {
        // construct AlertDialog that will be called on url button
        AlertDialog urlAlertDialog = createUrlAlertDialog();
        // set listener to url button
        btnUrl.setOnClickListener((View btnView) -> {
            urlAlertDialog.show();
        });
    }

    /**
     * Collects all the information from the input fields, constructs a CultureItem and
     * initiates the item creation request.
     *
     * @param btnCreate Create button which initiates an item creation request.
     * @param etTitle Title EditText.
     * @param etDescription Description EditText.
     * @param etContinent Continent EditText.
     * @param etCountry Country EditText.
     * @param etCity City EditText.
     * @param progressBar ProgressBar object that will be visible until we get a response.
     */
    private void setCreateItemListener(Button btnCreate, EditText etTitle, EditText etDescription,
                                       EditText etContinent, EditText etCountry, EditText etCity,
                                       ProgressBar progressBar) {
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

            item.setTagList(mTagList);
            makeCreateRequest(item, imageList, progressBar);
        });
    }

    /**
     * Decide on actions upon activity result.
     *
     * @param requestCode Request code that was sent with the intent
     * @param resultCode Result code indicating if the action was successful or not.
     * @param data Returned data
     */
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
            Utils.showToast(getActivity().getApplicationContext(), getString(R.string.error_occurred));
        }
    }

    /**
     * Clears all the views in this fragment.
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
        mTagList.clear();
        mImageAdapter.notifyDataSetChanged();
        mTagAdapter.notifyDataSetChanged();
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
            mImageAdapter.notifyDataSetChanged();
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

