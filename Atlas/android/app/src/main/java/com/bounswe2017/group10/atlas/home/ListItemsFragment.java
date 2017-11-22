package com.bounswe2017.group10.atlas.home;

import android.content.Context;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.content.ContextCompat;
import android.support.v4.widget.SwipeRefreshLayout;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.ListItemsAdapter;
import com.bounswe2017.group10.atlas.adapter.FeedRow;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.response.OnGetItemsResponse;
import com.bounswe2017.group10.atlas.util.Constants;

import java.util.ArrayList;
import java.util.List;

import static com.bounswe2017.group10.atlas.util.Utils.getSharedPref;

public class ListItemsFragment extends Fragment {

    private final ArrayList<CultureItem> mItemList = new ArrayList<>();
    private final ArrayList<FeedRow> mRowList = new ArrayList<>();
    private ListItemsAdapter mAdapter;
    private RequestStrategy requestStrategy = new FeedStrategy();
    private boolean firstView = true;
    private boolean isLoading = false;
    private boolean isLastPage = false;
    private int currentOffset = 0;
    private SwipeRefreshLayout mSwipeLayout;

    /**
     * Interface for different request strategies to be used with this item
     * listing fragment.
     */
    public interface RequestStrategy {
        /**
         * Specify how to request items.
         *
         * @param context Context in which the items will be requested.
         * @param offset Page offset.
         * @param getItemCallback Callback interface whose onGetItems method will be called with
         *                        the returned CultureItem objects.
         */
        public void requestItems(Context context, int offset, OnGetItemsResponse.GetItemCallback getItemCallback);
    }

    /**
     * RequestStrategy to get items for user feed.
     */
    public static class FeedStrategy implements RequestStrategy {
        public void requestItems(Context context, int offset, OnGetItemsResponse.GetItemCallback getItemCallback) {
            String authStr = getSharedPref(context).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
            OnGetItemsResponse respHandler = new OnGetItemsResponse(context, getItemCallback);
            APIUtils.serverAPI().getItems(authStr, Constants.PAGINATION_COUNT, offset).enqueue(respHandler);
        }
    }

    /**
     * RequestStrategy for getting a user's own items.
     */
    public static class OwnItemsStrategy implements RequestStrategy {
        public void requestItems(Context context, int offset, OnGetItemsResponse.GetItemCallback getItemCallback) {
            String authStr = getSharedPref(context).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
            // TODO: make request for getting own items
            // OnGetOwnItemsResponse respHandler = new OnGetOwnItemsResponse(context, itemList, rowList, adapter);
            // APIUtils.serverAPI().getOwnItems(authStr).enqueue(respHandler);
        }
    }

    /**
     * Set the current RequestStrategy to be used by this Fragment.
     *
     * @param strategy RequestStrategy whose requestItems method will be called when requesting items.
     */
    public void setRequestStrategy(RequestStrategy strategy) {
        this.requestStrategy = strategy;
    }


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        super.onCreateView(inflater, container, savedInstanceState);
        View view = inflater.inflate(R.layout.fragment_list_items, container, false);
        RecyclerView recyclerView = view.findViewById(R.id.list_items_recyclerview);
        this.mSwipeLayout = view.findViewById(R.id.swipe_container);

        OnGetItemsResponse.GetItemCallback getItemCallback = this.createNewItemHandler();

        mAdapter = new ListItemsAdapter(getActivity(), mRowList, (List<FeedRow> rowList, int position) -> {
            // put item to bundle
            Bundle itemBundle = new Bundle();
            itemBundle.putParcelable(Constants.CULTURE_ITEM, mItemList.get(position));
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
        recyclerView.setAdapter(mAdapter);


        this.setOnScrollListener(recyclerView, getItemCallback);

        // get the first page of items
        if (firstView) {
            mItemList.clear();
            mRowList.clear();
            mAdapter.notifyDataSetChanged();
            loadMoreItems(getItemCallback);
            firstView = false;
        }

        // set swipe layout listeners
        this.mSwipeLayout.setOnRefreshListener(() -> {
            mItemList.clear();
            mRowList.clear();
            this.currentOffset = 0;
            mAdapter.notifyDataSetChanged();
            loadMoreItems(getItemCallback);
        });

        this.mSwipeLayout.setColorSchemeColors(
            ContextCompat.getColor(getContext(), android.R.color.holo_blue_bright),
            ContextCompat.getColor(getContext(), android.R.color.holo_red_light),
            ContextCompat.getColor(getContext(), android.R.color.holo_green_light),
            ContextCompat.getColor(getContext(), android.R.color.holo_orange_light)
        );

        return view;
    }

    /**
     * Sets OnScrollListener to RecyclerView object to request more items when it is scrolled to the
     * bottom.
     *
     * @param recyclerView RecyclerView object whose OnScrollListener will be set.
     * @param getItemCallback Callback interface whose onGetItems method will be called upon
     *                        successfully getting more items.
     */
    private void setOnScrollListener(RecyclerView recyclerView, OnGetItemsResponse.GetItemCallback getItemCallback) {
        LinearLayoutManager layoutManager = new LinearLayoutManager(getActivity());
        recyclerView.setLayoutManager(layoutManager);

        recyclerView.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrollStateChanged(RecyclerView recyclerView, int newState) {
                super.onScrollStateChanged(recyclerView, newState);
            }

            @Override
            public void onScrolled(RecyclerView recyclerView, int dx, int dy) {
                super.onScrolled(recyclerView, dx, dy);
                int visibleItemCount = layoutManager.getChildCount();
                int totalItemCount = layoutManager.getItemCount();
                int firstVisibleItemPosition = layoutManager.findFirstVisibleItemPosition();

                if (!isLoading && !isLastPage) {
                    if (visibleItemCount + firstVisibleItemPosition >= totalItemCount &&
                            firstVisibleItemPosition >= 0 &&
                            totalItemCount >= Constants.PAGINATION_COUNT) {
                        loadMoreItems(getItemCallback);
                    }
                }
            }
        });
    }

    /**
     * Creates and returns a new OnGetItemsResponse.GetItemCallback object whose onGetItems method
     * will be called when more items are obtained.
     *
     * @return OnGetItemsResponse.GetItemCallback object.
     */
    private OnGetItemsResponse.GetItemCallback createNewItemHandler() {
        return new OnGetItemsResponse.GetItemCallback() {
            @Override
            public void onGetItems(List<CultureItem> itemList) {
                for (CultureItem item : itemList) {
                    mItemList.add(item);
                    mRowList.add(item.toFeedRow());
                }
                mAdapter.notifyDataSetChanged();
                isLoading = false;
                isLastPage = itemList.size() < Constants.PAGINATION_COUNT;
                mSwipeLayout.setRefreshing(false);
            }
        };
    }

    /**
     * Load more items from the server.
     *
     * @param getItemCallback Callback object whose onGetItems method will be called upon obtaining
     *                        more items.
     */
    private void loadMoreItems(OnGetItemsResponse.GetItemCallback getItemCallback) {
        this.mSwipeLayout.setRefreshing(true);
        this.isLoading = true;
        this.requestStrategy.requestItems(getActivity(), this.currentOffset, getItemCallback);
        this.currentOffset += Constants.PAGINATION_COUNT;
    }
}

