package com.bounswe2017.group10.atlas.home;

import android.content.Context;
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

import java.util.ArrayList;

import static com.bounswe2017.group10.atlas.util.Utils.getSharedPref;

public class ListItemsFragment extends Fragment {

    private final ArrayList<CultureItem> mItemList = new ArrayList<>();
    private final ArrayList<FeedRow> mRowList = new ArrayList<>();
    private FeedListAdapter mAdapter;
    private RequestStrategy requestStrategy = new FeedStrategy();

    public interface RequestStrategy {
        public void requestItems(Context context, ArrayList<CultureItem> itemList, ArrayList<FeedRow> rowList, FeedListAdapter adapter);
    }


    public static class FeedStrategy implements RequestStrategy {
        public void requestItems(Context context, ArrayList<CultureItem> itemList, ArrayList<FeedRow> rowList, FeedListAdapter adapter) {
            String authStr = getSharedPref(context).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
            OnGetAllItemsResponse respHandler = new OnGetAllItemsResponse(context, itemList, rowList, adapter);
            APIUtils.serverAPI().getAllItems(authStr).enqueue(respHandler);
        }
    }

    public static class OwnItemsStrategy implements RequestStrategy {
        public void requestItems(Context context, ArrayList<CultureItem> itemList, ArrayList<FeedRow> rowList, FeedListAdapter adapter) {
            String authStr = getSharedPref(context).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
            // TODO: make request for getting own items
            // OnGetOwnItemsResponse respHandler = new OnGetOwnItemsResponse(context, itemList, rowList, adapter);
            // APIUtils.serverAPI().getOwnItems(authStr).enqueue(respHandler);
        }
    }

    public void setRequestStrategy(RequestStrategy strategy) {
        this.requestStrategy = strategy;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        super.onCreateView(inflater, container, savedInstanceState);
        View view = inflater.inflate(R.layout.fragment_feed, container, false);

        ListView listView = view.findViewById(R.id.feed_listview);

        mAdapter = new FeedListAdapter(getActivity(), mRowList);
        listView.setAdapter(mAdapter);

        // currently, get all items
        this.requestStrategy.requestItems(getActivity(), mItemList, mRowList, mAdapter);

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
            this.requestStrategy.requestItems(getActivity(), mItemList, mRowList, mAdapter);
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

