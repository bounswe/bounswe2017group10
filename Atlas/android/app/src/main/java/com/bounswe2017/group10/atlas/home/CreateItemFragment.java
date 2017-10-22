package com.bounswe2017.group10.atlas.home;


import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
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

import com.bounswe2017.group10.atlas.R;

import java.util.ArrayList;


public class CreateItemFragment extends Fragment {

    public static final String TAG = "CreateItemFragment";
    private static final int FROM_GALLERY = 1;
    private static final int FROM_URL = 2;
    private static final int FROM_CAMERA = 3;

    private ImageListAdapter mAdapter;
    private final ArrayList<ImageRow> mImageRowList = new ArrayList<>();

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_create_item, container, false);

        ListView imageListView = view.findViewById(R.id.image_listview);
        Button btnGallery = view.findViewById(R.id.gallery_button);
        Button btnUrl = view.findViewById(R.id.url_button);

        // set ImageListAdapter to imageListView
        mAdapter = new ImageListAdapter(getActivity(), mImageRowList);
        imageListView.setAdapter(mAdapter);

        // set listener to gallery button
        btnGallery.setOnClickListener((View btnView) -> {
            Intent intent = new Intent();
            intent.setType("image/*");
            intent.setAction(Intent.ACTION_GET_CONTENT);
            startActivityForResult(Intent.createChooser(intent, "Select Image"), FROM_GALLERY);
        });

        // construct AlertDialog that will be called on url button
        AlertDialog urlAlertDialog = createUrlAlertDialog();
        // set listener to url button
        btnUrl.setOnClickListener((View btnView) -> {
            urlAlertDialog.show();
        });

        // set listener to camera button

        return view;
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (resultCode == Activity.RESULT_OK) {
            if (requestCode == FROM_GALLERY) {

            } else if (requestCode == FROM_CAMERA) {

            } else if (requestCode == FROM_URL) {

            } else {
                Log.d(TAG, "OnActivityResult wrong requestCode : " + requestCode);
            }
        }
    }

    /**
     * Adds the image in the given url to image listview on this fragment.
     *
     * @param url URL of the image.
     */
    private void addImageFromUrl(String url) {
        ImageRow row = new ImageRow();
        row.setUrl(url);
        mImageRowList.add(row);
        mAdapter.notifyDataSetChanged();
    }

    private AlertDialog createUrlAlertDialog() {
        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        builder.setTitle(getResources().getString(R.string.enter_url));
        final EditText input = new EditText(getActivity());
        input.setInputType(InputType.TYPE_CLASS_TEXT);
        builder.setView(input);
        builder.setPositiveButton(getResources().getString(R.string.ok),(DialogInterface dialog, int i) -> {
            String url = input.getText().toString();
            addImageFromUrl(url);
        });
        builder.setNegativeButton(getResources().getString(R.string.cancel), (DialogInterface dialog, int i) -> {
            dialog.cancel();
        });
        return builder.create();
    }
}

