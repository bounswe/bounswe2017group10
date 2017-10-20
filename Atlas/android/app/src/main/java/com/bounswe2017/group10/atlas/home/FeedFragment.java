package com.bounswe2017.group10.atlas.home;


import android.app.Fragment;
import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.ListFragment;
import android.view.View;
import android.widget.ListView;


import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.auth.SignupFragment;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.remote.APIUtils;

import java.util.ArrayList;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

public class FeedFragment extends ListFragment {

    private final ArrayList<CultureItem> mItemList = new ArrayList<>();
    private final ArrayList<FeedRow> mRowList = new ArrayList<>();
    private FeedArrayAdapter mAdapter;
    private String authStr;

    @Nullable
    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        authStr = getArguments().getString("authStr", "NO_TOKEN");

        // get items 3 and 4
        APIUtils.getAPI().getItem(authStr, 3).enqueue(new OnGetItemResponse());
        APIUtils.getAPI().getItem(authStr, 4).enqueue(new OnGetItemResponse());
        APIUtils.getAPI().getItem(authStr, 5).enqueue(new OnGetItemResponse());

        mAdapter = new FeedArrayAdapter(getActivity(), mRowList);
        setListAdapter(mAdapter);
    }

    @Override
    public void onListItemClick(ListView listView, View view, int pos, long id) {
        // TODO : foreign key of the item that cliked must be found.
        String itemID = "temp";

        ViewItemFragment viewItemFragment = null;

        if (viewItemFragment == null) {
            viewItemFragment = new ViewItemFragment();
        }
        getActivity().getSupportFragmentManager()
                .beginTransaction()
                .replace(R.id., viewItemFragment)
                .addToBackStack(null)
                .commit();
    }

    /**
     * Adds a culture item to current buffers and notifies the adapter of the changes.
     *
     * Culture item itself is added to culture items list. Its corresponding FeedRow object
     * is added to FeedRow list.
     *
     * @param item CultureItem object to be stored.
     */
    private void addCultureItem(CultureItem item) {
        mItemList.add(item);
        mRowList.add(item.toFeedRow());
        mAdapter.notifyDataSetChanged();
    }

    private class OnGetItemResponse implements Callback<CultureItem> {
        @Override
        public void onResponse(Call<CultureItem> call, Response<CultureItem> response) {
            if (response.isSuccessful()) {
                addCultureItem(response.body());
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

