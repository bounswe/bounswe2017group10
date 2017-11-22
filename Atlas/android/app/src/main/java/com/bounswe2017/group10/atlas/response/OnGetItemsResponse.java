package com.bounswe2017.group10.atlas.response;

import android.content.Context;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.ListItemsAdapter;
import com.bounswe2017.group10.atlas.adapter.FeedRow;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.httpbody.GetItemsResponse;
import com.bounswe2017.group10.atlas.util.Utils;

import java.util.ArrayList;
import java.util.List;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


public class OnGetItemsResponse implements Callback<GetItemsResponse> {

    public interface GetItemCallback {
        public void onGetItems(List<CultureItem> itemList);
    }

    private Context context;
    private final GetItemCallback mGetItemCallback;

    public OnGetItemsResponse(Context context, GetItemCallback getItemCallback) {
        this.context = context;
        this.mGetItemCallback = getItemCallback;
    }

    @Override
    public void onResponse(Call<GetItemsResponse> call, Response<GetItemsResponse> response) {
        if (response.isSuccessful()) {
            // add all items to given item lists
            List<CultureItem> responseItemList = response.body().getResults();
            this.mGetItemCallback.onGetItems(responseItemList);
        } else {
            Utils.showToast(context, context.getString(R.string.get_item_error));
        }
    }

    @Override
    public void onFailure(Call<GetItemsResponse> call, Throwable t) {
        Utils.showToast(context, context.getResources().getString(R.string.connection_failure));
        // TODO: do logging
    }
}

