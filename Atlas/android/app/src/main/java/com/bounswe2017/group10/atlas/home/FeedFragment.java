package com.bounswe2017.group10.atlas.home;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v4.app.ListFragment;
import android.support.v4.content.ContextCompat;
import android.support.v4.widget.SwipeRefreshLayout;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.FeedListAdapter;
import com.bounswe2017.group10.atlas.adapter.FeedRow;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.response.OnGetAllItemsResponse;
import com.bounswe2017.group10.atlas.util.Constants;
import com.cloudinary.android.MediaManager;

import java.util.ArrayList;

import static com.bounswe2017.group10.atlas.util.Utils.getSharedPref;

public class FeedFragment extends Fragment {

    private final ArrayList<CultureItem> mItemList = new ArrayList<>();
    private final ArrayList<FeedRow> mRowList = new ArrayList<>();
    private FeedListAdapter mAdapter;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        super.onCreateView(inflater, container, savedInstanceState);
        View view = inflater.inflate(R.layout.fragment_feed, container, false);

        ListView listView = view.findViewById(R.id.feed_listview);

        mAdapter = new FeedListAdapter(getActivity(), mRowList);
        listView.setAdapter(mAdapter);

        // currently, get all items
        String authStr = getSharedPref(getActivity()).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
        OnGetAllItemsResponse respHandler = new OnGetAllItemsResponse(getActivity(), mItemList, mRowList, mAdapter);
        APIUtils.serverAPI().getAllItems(authStr).enqueue(respHandler);

        // item click listener
        listView.setOnItemClickListener((AdapterView<?> adapterView, View itemView, int pos, long arg3) -> {
            // put item to bundle
            Bundle itemBundle = new Bundle();
            itemBundle.putParcelable(Constants.CULTURE_ITEM, mItemList.get(pos));
            // put bundle to fragment
            ViewItemFragment viewItemFragment = new ViewItemFragment();
            viewItemFragment.setArguments(itemBundle);
            // go to fragment
            getActivity().getSupportFragmentManager()
                    .beginTransaction()
                    .replace(R.id.home_container, viewItemFragment)
                    .addToBackStack(null)
                    .commit();
        });

        // set swipe layout listeners
        SwipeRefreshLayout swipeLayout = view.findViewById(R.id.swipe_container);
        swipeLayout.setOnRefreshListener(() -> {
            mItemList.clear();
            mRowList.clear();
            APIUtils.serverAPI().getAllItems(authStr).enqueue(respHandler);
            swipeLayout.setRefreshing(false);
        });
        swipeLayout.setColorSchemeColors(
            ContextCompat.getColor(getContext(), android.R.color.holo_blue_bright),
            ContextCompat.getColor(getContext(), android.R.color.holo_red_light),
            ContextCompat.getColor(getContext(), android.R.color.holo_green_light),
            ContextCompat.getColor(getContext(), android.R.color.holo_orange_light)
        );

        return view;
    }
}

