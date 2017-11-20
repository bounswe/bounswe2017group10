package com.bounswe2017.group10.atlas.response;

import android.content.Context;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.CommentAdapter;
import com.bounswe2017.group10.atlas.adapter.CommentRow;
import com.bounswe2017.group10.atlas.adapter.FeedListAdapter;
import com.bounswe2017.group10.atlas.adapter.FeedRow;
import com.bounswe2017.group10.atlas.httpbody.Comment;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.util.Utils;

import java.util.ArrayList;
import java.util.List;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


public class OnGetCommentsResponse implements Callback<List<Comment>> {

    private Context context;
    private final ArrayList<Comment> mItemList;
    private final ArrayList<CommentRow> mRowList;
    private CommentAdapter mAdapter;

    public OnGetCommentsResponse(Context context, ArrayList<Comment> itemList, ArrayList<CommentRow> rowList, CommentAdapter adapter) {
        this.context = context;
        this.mItemList = itemList;
        this.mRowList = rowList;
        this.mAdapter = adapter;
    }

    @Override
    public void onResponse(Call<List<Comment>> call, Response<List<Comment>> response) {
        if (response.isSuccessful()) {
            // add all items to given item lists
            List<Comment> responseItemList = response.body();
            for (Comment item : responseItemList) {
                mItemList.add(item);
                mRowList.add(item.toCommentRow());
            }
            mAdapter.notifyDataSetChanged();
        } else {
            Utils.showToast(context, "Comment Error!");
        }
    }

    @Override
    public void onFailure(Call<List<Comment>> call, Throwable t) {
        Utils.showToast(context, context.getResources().getString(R.string.connection_failure));
        // TODO: do logging
    }
}

