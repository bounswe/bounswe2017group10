package com.bounswe2017.group10.atlas.response;

import android.content.Context;
import android.support.annotation.NonNull;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.CommentAdapter;
import com.bounswe2017.group10.atlas.adapter.CommentRow;
import com.bounswe2017.group10.atlas.httpbody.Comment;
import com.bounswe2017.group10.atlas.util.Utils;

import java.util.ArrayList;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


public class OnPostCommentResponse implements Callback<Comment> {

    private Context context;
    private final ArrayList<Comment> mItemList;
    private final ArrayList<CommentRow> mRowList;
    private CommentAdapter mAdapter;

    public OnPostCommentResponse(Context context, ArrayList<Comment> itemList, ArrayList<CommentRow> rowList, CommentAdapter adapter) {
        this.context = context;
        this.mItemList = itemList;
        this.mRowList = rowList;
        this.mAdapter = adapter;
    }


    @Override
    public void onResponse(@NonNull Call<Comment> call, @NonNull Response<Comment> response) {
        if (response.isSuccessful()) {
            // add all items to given item lists
            Comment responseComment = response.body();
            mItemList.add(0,responseComment);
            assert responseComment != null;
            mRowList.add(0,responseComment.toCommentRow());

            mAdapter.notifyDataSetChanged();
        } else {
            Utils.showToast(context, "Comment Error!");
        }
    }

    @Override
    public void onFailure(@NonNull Call<Comment> call, @NonNull Throwable t) {
        Utils.showToast(context, context.getResources().getString(R.string.connection_failure));
    }
}

