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
        System.out.println("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" + authStr);
        //String itemID = getArguments().getString("id");
        String[] sp = authStr.split(" ");
        APIUtils.serverAPI().getItem("JWT " + sp[sp.length - 1], 3).enqueue(new ViewItemFragment.OnGetItemResponse());



        return view;
    }

    private class OnGetItemResponse implements Callback<CultureItem> {
        // TODO: refactor this class to its own file under response package
        @Override
        public void onResponse(Call<CultureItem> call, Response<CultureItem> response) {
            if (response.isSuccessful()) {
                ////////////////////////delete next line after
                System.out.println("TEST/////////" + response.body().getTitle());
                // TODO : we are getting the data from api but i couldnt manage to print it on the screen rn.
                viewItemTitle.setText(response.body().getTitle());
                viewItemDesc.setText(response.body().getDescription());
                viewItemImage.setText(response.body().getImageUrlList().get(0));
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
