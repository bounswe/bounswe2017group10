package com.bounswe2017.group10.atlas.home;

import android.content.Context;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.content.ContextCompat;
import android.support.v4.widget.SwipeRefreshLayout;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.ListView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.FeedListAdapter;
import com.bounswe2017.group10.atlas.adapter.FeedRow;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.response.OnGetItemsResponse;
import com.bounswe2017.group10.atlas.util.Constants;

import java.util.ArrayList;

import static com.bounswe2017.group10.atlas.util.Utils.getSharedPref;

public class ListItemsFragment extends Fragment {

    private final ArrayList<CultureItem> mItemList = new ArrayList<>();
    private final ArrayList<FeedRow> mRowList = new ArrayList<>();
    private FeedListAdapter mAdapter;
    private RequestStrategy requestStrategy = new FeedStrategy();

    /**
     * Interface for different request strategies to be used with this item
     * listing fragment.
     */
    public interface RequestStrategy {
        /**
         * Specify how to request items.
         *
         * @param context Context in which the items will be requested.
         * @param itemList List of CultureItems to which new items will be added.
         * @param rowList List of FeedRows to which FeedRow versions of requested CultureItems will be added.
         * @param adapter Adapter to be notified.
         * @param itemOffset Items [itemOffset:itemOffset + PAGINATION_COUNT) will be requested.
         */
        public void requestItems(Context context,
                                 ArrayList<CultureItem> itemList,
                                 ArrayList<FeedRow> rowList,
                                 FeedListAdapter adapter,
                                 int itemOffset);
    }


    public static class FeedStrategy implements RequestStrategy {
        public void requestItems(Context context, ArrayList<CultureItem> itemList, ArrayList<FeedRow> rowList, FeedListAdapter adapter, int itemOffset) {
            String authStr = getSharedPref(context).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
            OnGetItemsResponse respHandler = new OnGetItemsResponse(context, itemList, rowList, adapter);
            APIUtils.serverAPI().getItems(authStr, Constants.PAGINATION_COUNT, itemOffset).enqueue(respHandler);
        }
    }

    public static class OwnItemsStrategy implements RequestStrategy {
        public void requestItems(Context context, ArrayList<CultureItem> itemList, ArrayList<FeedRow> rowList, FeedListAdapter adapter, int itemOffset) {
            String authStr = getSharedPref(context).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
            // TODO: make request for getting own items
            // OnGetOwnItemsResponse respHandler = new OnGetOwnItemsResponse(context, itemList, rowList, adapter);
            // APIUtils.serverAPI().getOwnItems(authStr).enqueue(respHandler);
        }
    }

    public void setRequestStrategy(RequestStrategy strategy) {
        this.requestStrategy = strategy;
    }

    private boolean firstView = true;
    private int currentOffset = 0;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        super.onCreateView(inflater, container, savedInstanceState);
        View view = inflater.inflate(R.layout.fragment_list_items, container, false);

        ListView listView = view.findViewById(R.id.feed_listview);
        listView.setOnScrollListener(new AbsListView.OnScrollListener() {

            private int currentVisibleItemCount = 0;
            private int currentScrollState = SCROLL_STATE_IDLE;
            private int currentFirstVisibleItem = 0;
            private int totalItem = 0;


            @Override
            public void onScrollStateChanged(AbsListView view, int scrollState) {
                this.currentScrollState = scrollState;
                this.isScrollCompleted();
            }

            @Override
            public void onScroll(AbsListView view, int firstVisibleItem, int visibleItemCount, int totalItemCount) {
                this.currentFirstVisibleItem = firstVisibleItem;
                this.currentVisibleItemCount = visibleItemCount;
                this.totalItem = totalItemCount;
            }

            private void isScrollCompleted() {
                if (totalItem - currentFirstVisibleItem == currentVisibleItemCount && currentScrollState == SCROLL_STATE_IDLE) {
                    requestStrategy.requestItems(getActivity(), mItemList, mRowList, mAdapter, currentOffset);
                    currentOffset += Constants.PAGINATION_COUNT;
                }
            }
        });

        mAdapter = new FeedListAdapter(getActivity(), mRowList);
        listView.setAdapter(mAdapter);

        // currently, get all items
        if (firstView) {
            mItemList.clear();
            mRowList.clear();
            mAdapter.notifyDataSetChanged();
            this.requestStrategy.requestItems(getActivity(), mItemList, mRowList, mAdapter, this.currentOffset);
            this.currentOffset += Constants.PAGINATION_COUNT;
            firstView = false;
        }

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
            this.currentOffset = 0;
            mAdapter.notifyDataSetChanged();
            this.requestStrategy.requestItems(getActivity(), mItemList, mRowList, mAdapter, this.currentOffset);
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

