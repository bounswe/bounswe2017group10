package com.bounswe2017.group10.atlas.home;


import android.content.Context;
import android.support.v4.app.Fragment;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bumptech.glide.Glide;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

public class ViewItemFragment extends Fragment {

    TextView viewItemTitle;
    ImageView viewItemImage;
    TextView viewItemDesc;



    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_view_item, container, false);

        viewItemTitle = view.findViewById(R.id.itemTitle);
        viewItemImage = view.findViewById(R.id.itemImage);
        viewItemDesc = view.findViewById(R.id.itemDesc);

        String authStr = getArguments().getString(Constants.AUTH_STR, "NO_TOKEN");
        System.out.println("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" + authStr);
        String itemID = getArguments().getString("itemId");
        APIUtils.serverAPI().getItem(authStr, 3).enqueue(new ViewItemFragment.OnGetItemResponse());



        return view;
    }

    private class OnGetItemResponse implements Callback<CultureItem> {
        // TODO: refactor this class to its own file under response package
        @Override
        public void onResponse(Call<CultureItem> call, Response<CultureItem> response) {
            if (response.isSuccessful()) {
                ////////////////////////delete next line after
                System.out.println("TEST/////////" + response.body().getTitle());
                viewItemTitle.setText(response.body().getTitle());
                viewItemDesc.setText(response.body().getDescription());
                //viewItemImage.setText(response.body().getImageList().get(0).getUrl());


                Glide.with(getView())
                        .load(response.body().getImageList().get(0).getUrl())
                        .into(viewItemImage);

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
