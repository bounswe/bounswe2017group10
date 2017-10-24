package com.bounswe2017.group10.atlas.home;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.ListFragment;
import android.view.View;
import android.widget.ListView;
import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.FeedListAdapter;
import com.bounswe2017.group10.atlas.adapter.FeedRow;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.response.OnGetAllItemsResponse;
import com.bounswe2017.group10.atlas.util.Constants;
import java.util.ArrayList;

import static com.bounswe2017.group10.atlas.util.Utils.getSharedPref;

public class FeedFragment extends ListFragment {

    private final ArrayList<CultureItem> mItemList = new ArrayList<>();
    private final ArrayList<FeedRow> mRowList = new ArrayList<>();
    private FeedListAdapter mAdapter;

    @Nullable
    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        String authStr = getSharedPref(getActivity()).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);

        mAdapter = new FeedListAdapter(getActivity(), mRowList);
        setListAdapter(mAdapter);

        // currently, get all items
        OnGetAllItemsResponse respHandler = new OnGetAllItemsResponse(getActivity(), mItemList, mRowList, mAdapter);
        APIUtils.serverAPI().getAllItems(authStr).enqueue(respHandler);
    }

    @Override
    public void onListItemClick(ListView listView, View view, int pos, long id) {
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
    }
}

