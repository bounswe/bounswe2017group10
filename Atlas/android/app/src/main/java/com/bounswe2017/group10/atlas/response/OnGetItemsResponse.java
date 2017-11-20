package com.bounswe2017.group10.atlas.response;

import android.content.Context;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.FeedListAdapter;
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

    private Context context;
    private final ArrayList<CultureItem> mItemList;
    private final ArrayList<FeedRow> mRowList;
    private FeedListAdapter mAdapter;

    public OnGetItemsResponse(Context context, ArrayList<CultureItem> itemList, ArrayList<FeedRow> rowList, FeedListAdapter adapter) {
        this.context = context;
        this.mItemList = itemList;
        this.mRowList = rowList;
        this.mAdapter = adapter;
    }

    @Override
    public void onResponse(Call<GetItemsResponse> call, Response<GetItemsResponse> response) {
        if (response.isSuccessful()) {
            // add all items to given item lists
            List<CultureItem> responseItemList = response.body().getResults();
            for (CultureItem item : responseItemList) {
                mItemList.add(item);
                mRowList.add(item.toFeedRow());
            }
            mAdapter.notifyDataSetChanged();
        } else {
            Utils.showToast(context, "Create Item Error!");
        }
    }

    @Override
    public void onFailure(Call<GetItemsResponse> call, Throwable t) {
        Utils.showToast(context, context.getResources().getString(R.string.connection_failure));
        // TODO: do logging
    }
}

