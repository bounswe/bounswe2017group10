package com.bounswe2017.group10.atlas.home;


import android.content.Intent;
import android.support.v4.app.Fragment;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.remote.APIUtils;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

public class ViewItemFragment extends Fragment {

    TextView viewItemTitle;
    TextView viewItemImage;
    TextView viewItemDesc;


    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_view_item, container, false);

        viewItemTitle = view.findViewById(R.id.itemTitle);
        viewItemImage = view.findViewById(R.id.itemImage);
        viewItemDesc = view.findViewById(R.id.itemDesc);

        String authStr = getArguments().getString("authStr", "NO_TOKEN");
        //String itemID = getArguments().getString("id");
        APIUtils.getAPI().getItem(authStr, 3).enqueue(new ViewItemFragment.OnGetItemResponse());



        return view;
    }

    private class OnGetItemResponse implements Callback<CultureItem> {
        @Override
        public void onResponse(Call<CultureItem> call, Response<CultureItem> response) {
            if (response.isSuccessful()) {
                viewItemTitle.setText(response.body().getTitle().toCharArray(),0,response.body().getTitle().length());
                viewItemDesc.setText(response.body().getDescription().toCharArray(),0,response.body().getDescription().length());
                viewItemImage.setText(response.body().getImageUrl().toCharArray(),0,response.body().getImageUrl().length());

                // TODO : we are only showing image url r.now.

            } else {
                // TODO: Error checking
            }
        }

        @Override
        public void onFailure(Call<CultureItem> call, Throwable t) {
            // TODO: Check network connections
        }
    }


}
