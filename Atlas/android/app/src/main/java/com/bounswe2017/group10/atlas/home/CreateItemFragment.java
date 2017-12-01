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
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.ImageListAdapter;
import com.bounswe2017.group10.atlas.adapter.ImageRow;
import com.bounswe2017.group10.atlas.adapter.TagListAdapter;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.httpbody.Tag;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.response.OnCreateItemResponse;
import com.bounswe2017.group10.atlas.response.OnUpdateItemResponse;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.OnMapReadyCallback;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.LatLng;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


public class CreateItemFragment extends Fragment {

    private enum REQUEST_TYPE {
        UPDATE,
        CREATE
    };

    private REQUEST_TYPE mRequestType = REQUEST_TYPE.CREATE;
    private CultureItem mItemToSend = new CultureItem();

    private static final String TAG = "CreateItemFragment";
    public static final int FROM_GALLERY = 1;
    public static final int FROM_CAMERA = 2;
    public static final int FROM_LOCATION = 3;
    public static final int CAMERA_REQUEST_CODE = 4;

    private final ArrayList<String> mAllTagsList = new ArrayList<>();

    private ImageListAdapter mImageAdapter;
    private final ArrayList<ImageRow> mImageRowList = new ArrayList<>();

    private TagListAdapter mTagAdapter;
    private final ArrayList<Tag> mTagList = new ArrayList<>();

    private ArrayAdapter<String> mAutoComplAdapter;

    private Uri currentPhotoUri = null;

    private SupportMapFragment mMapFragment;
    private Button mBtnLocation = null;

    @Override
    public void onActivityCreated(@Nullable Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        // make a single call to get all Tags
        String authStr = Utils.getSharedPref(getActivity()).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
        APIUtils.serverAPI().getAllTags(authStr).enqueue(new Callback<List<Tag>>() {
            @Override
            public void onResponse(Call<List<Tag>> call, Response<List<Tag>> response) {
                if (response.isSuccessful()) {
                    List<Tag> responseList = response.body();
                    for (Tag t : responseList) {
                        mAllTagsList.add(t.getName());
                    }
                    mAutoComplAdapter.notifyDataSetChanged();
                } else {
                    Log.d(TAG, "Error on getting all tags: " + response.errorBody().toString());
                }
            }

            @Override
            public void onFailure(Call<List<Tag>> call, Throwable t) {
                Log.d(TAG, "Connection failure on getting all tags: " + t.toString());
            }
        });
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_create_item, container, false);

        // set adapters
        ListView imageListView = view.findViewById(R.id.image_listview);
        RecyclerView tagRecyclerview = view.findViewById(R.id.tag_recyclerview);
        AutoCompleteTextView etTags = view.findViewById(R.id.tag_auto_comp_textview);
        setAdapters(tagRecyclerview, imageListView, etTags);

        // handle tags
        setTagChoosingListener(etTags);
        setTagEnteringListener(etTags);

        // handle gallery feature
        Button btnGallery = view.findViewById(R.id.gallery_button);
        setGalleryListener(btnGallery);

        // handle camera feature
        Button btnCamera = view.findViewById(R.id.camera_button);
        setCameraListener(btnCamera);

        // handle url image feature
        Button btnUrl = view.findViewById(R.id.url_button);
        setURLListener(btnUrl);

        // handle location feature
        mBtnLocation = view.findViewById(R.id.location_button);
        setLocationListener(mBtnLocation);

        // If there is an argument item, fill the inputs with its data.
        Bundle arguments = getArguments();
        if (arguments != null) {
            this.mRequestType = REQUEST_TYPE.UPDATE;
            mItemToSend = arguments.getParcelable(Constants.CULTURE_ITEM);
            fillInputsWithItem(view);
        }

        return view;
    }

    /**
     * Fills the input fields in this Fragment with the given item
     *
     */
    private void fillInputsWithItem(View view) {
        if (mItemToSend.getTitle() != null) {
            ((TextView)view.findViewById(R.id.title_edittext)).setText(mItemToSend.getTitle());
        }
        if (mItemToSend.getDescription() != null) {
            ((TextView)view.findViewById(R.id.description_edittext)).setText(mItemToSend.getDescription());
        }
        for (Image img : mItemToSend.getImageList()) {
            mImageRowList.add(img.toImageRow());
        }
        mImageAdapter.notifyDataSetChanged();
        mTagList.addAll(mItemToSend.getTagList());
        mTagAdapter.notifyDataSetChanged();
    }


    /**
     * Sets the adapters required by ListView or RecyclerView objects in this fragment.
     *
     * @param tagRecyclerView RecyclerView object responsible for viewing tags horizontally.
     * @param imageListView ListView object responsible for viewing added images vertically.
     */
    private void setAdapters(RecyclerView tagRecyclerView, ListView imageListView, AutoCompleteTextView etTags) {
        // set TagListAdapter to tagRecyclerView
        mTagAdapter = new TagListAdapter(getActivity(), mTagList, (List<Tag> tagList, int position) -> {
            tagList.remove(position);
            mTagAdapter.notifyDataSetChanged();
        });
        tagRecyclerView.setAdapter(mTagAdapter);

        // set ImageListAdapter to imageListView
        mImageAdapter = new ImageListAdapter(getActivity(), mImageRowList);
        imageListView.setAdapter(mImageAdapter);

        // set AutoCompleteTextView String adapter
        etTags.setThreshold(2);
        mAutoComplAdapter = new ArrayAdapter<>(getActivity(), android.R.layout.select_dialog_item, mAllTagsList);
        etTags.setAdapter(mAutoComplAdapter);
    }

    /**
     * Set a listener to AutoCompleteTextView object such that whenever an item is
     * chosen from the dropdown menu, a corresponding tag is automatically created
     * and the input space is cleared.
     *
     * @param etTags AutoCompleteTextView object responsible for entering new tags
     */
    private void setTagChoosingListener(AutoCompleteTextView etTags) {
        etTags.setOnItemClickListener((AdapterView<?> parent, View view, int position, long id) -> {
            String tagStr = (String)parent.getItemAtPosition(position);
            createTag(tagStr);
            etTags.getEditableText().clear();
        });
    }

    /**
     * Sets a listener to tag edittext which creates a new Tag object whenever one of the
     * characters in Constants.TAG_SEPARATORS is entered
     *
     * @param etTags AutoCompleteTextView object responsible for entering new tags
     */
    private void setTagEnteringListener(AutoCompleteTextView etTags) {
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
                if (len == 0) {
                    return;
                }
                String lastChar = textEntered.substring(len - 1);
                if (Constants.TAG_SEPARATORS.contains(lastChar)) {
                    String textWithoutSpace = textEntered.substring(0, len - 1);
                    createTag(textWithoutSpace);
                    // clear edittext
                    s.clear();
                }
            }
        });
    }

    /**
     * Create a new Tag object and add it to the mTagAdapter to show it in RecyclerView.
     *
     * @param tagStr String from which a new Tag will be created.
     */
    private void createTag(String tagStr) {
        // add tag if it is not already added
        if (!tagStr.isEmpty()) {
            Tag tagToAdd = new Tag(tagStr);
            if (!mTagList.contains(tagToAdd)) {
                mTagList.add(tagToAdd);
                mTagAdapter.notifyDataSetChanged();
            }
        }

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
     * Sets listener for location button. Location button opens a PlacePicker to select
     * a place using Google Maps place picker.
     *
     * @param btnLocation Button that opens the location dialog.
     */
    private void setLocationListener(Button btnLocation) {
        btnLocation.setOnClickListener((View v) -> {
            Intent intent = new Intent(getActivity(), GoogleMapsActivity.class);
            startActivityForResult(intent, FROM_LOCATION);
        });
    }

    /**
     * Collects all the information from the input fields, constructs a CultureItem and
     * initiates the item creation request.
     */
    public void makeRequest() {
        View view = getView();
        EditText etTitle = view.findViewById(R.id.title_edittext);
        EditText etDescription = view.findViewById(R.id.description_edittext);
        ProgressBar progressBar = new ProgressBar(getActivity());

        if (etTitle.getText().length() == 0) {
            Utils.showToast(getActivity().getApplicationContext(), getResources().getString(R.string.empty_title));
            return;
        }
        String title = etTitle.getText().toString();
        String description = etDescription.getText().toString();

        mItemToSend.setTitle(title);
        mItemToSend.setDescription(description);
        if (description.length() == 0)
            mItemToSend.setDescription(null);

        mItemToSend.setPublicAccessibility(true);

        ArrayList<Image> imageList = new ArrayList<>();
        for (ImageRow row : mImageRowList) {
            Image img = new Image();
            img.setUrl(row.getUri().toString());
            imageList.add(img);
        }
        mItemToSend.setImageList(imageList);
        mItemToSend.setTagList(mTagList);
        makeCreateRequest(progressBar);
    }

    /**
     * Decide on actions upon activity result.
     *
     * @param requestCode Request code that was sent with the intent
     * @param resultCode Result code indicating if the action_home was successful or not.
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
            } else if (requestCode == FROM_LOCATION) {
                // get data from intent
                String placeName = data.getStringExtra(Constants.LOCATION_NAME);
                LatLng latLng = data.getParcelableExtra(Constants.LATLONG);

                // set data to item
                mItemToSend.setPlaceName(placeName);
                mItemToSend.setLatitude(Utils.roundToDecimals(latLng.latitude, Constants.LATLONG_PRECISION));
                mItemToSend.setLongitude(Utils.roundToDecimals(latLng.longitude, Constants.LATLONG_PRECISION));

                // show data in button
                mBtnLocation.setText(placeName);
            } else {
                Log.d(TAG, "OnActivityResult wrong requestCode : " + requestCode);
            }
        } else if (resultCode == Activity.RESULT_CANCELED){
            // do nothing
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

        etTitle.setText("");
        etDescription.setText("");
        mImageRowList.clear();
        mTagList.clear();
        mImageAdapter.notifyDataSetChanged();
        mTagAdapter.notifyDataSetChanged();
    }

    /**
     * Makes a new item create request to server with the given item while showing
     * the given progress bar.
     *
     * @param progressBar ProgressBar object which will be shown during request execution.
     */
    private void makeCreateRequest(ProgressBar progressBar) {
        progressBar.setVisibility(View.VISIBLE);
        Activity activity = getActivity();
        String authStr = Utils.getSharedPref(activity).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
        if (mRequestType == REQUEST_TYPE.CREATE) {  // create request
            APIUtils.serverAPI().createItem(authStr, mItemToSend).enqueue(new OnCreateItemResponse(this, mItemToSend.getImageList(), progressBar));
        } else if (mRequestType == REQUEST_TYPE.UPDATE){  // edit request
            APIUtils.serverAPI().updateItem(authStr, mItemToSend.getId(), mItemToSend).enqueue(new OnUpdateItemResponse(getActivity()));
        }
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

