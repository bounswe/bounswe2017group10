package com.bounswe2017.group10.atlas.adapter;


import android.content.Context;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bumptech.glide.Glide;
import com.bumptech.glide.request.RequestOptions;

import java.util.ArrayList;
import java.util.List;

import static java.lang.Math.min;
import static java.lang.StrictMath.max;

public class ListItemsAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private Context context;
    private List<FeedRow> rowList;
    private OnItemClickListener listener = null;

    /**
     * RecyclerViews don't support item click listeners natively. We need to
     * come up with our own interface.
     */
    public interface OnItemClickListener {
        void onItemClick(List<FeedRow> rowList, int position);
    }

    /**
     * ViewHolder class that implements the ViewHolder pattern for a more
     * efficient adapter.
     */
    private static class ViewHolder extends RecyclerView.ViewHolder {
        TextView etTitle;
        TextView etDescr;
        ImageView imIcon;
        TextView etTag1;
        TextView etTag2;
        TextView etTag3;
        View view;

        ViewHolder(View v) {
            super(v);
            this.etTitle = v.findViewById(R.id.title_textview);
            this.etDescr = v.findViewById(R.id.description_textview);
            this.imIcon = v.findViewById(R.id.icon_imageview);
            this.etTag1 = v.findViewById(R.id.tag1);
            this.etTag2 = v.findViewById(R.id.tag2);
            this.etTag3 = v.findViewById(R.id.tag3);
            this.view = v;
        }

        void bind(List<FeedRow> rowList, int position, Context context, OnItemClickListener listener) {
            FeedRow row = rowList.get(position);
            List<String> tagList = row.getTagList();
            TextView[] tagArr = {etTag1, etTag2, etTag3};
            int num_tags = tagList.size();
            int num_hidden = max(tagArr.length - num_tags, 0);
            for (int i = 0; i < num_hidden; ++i) {
                tagArr[i].setVisibility(View.INVISIBLE);
            }
            for (int i = num_hidden, j = 0; i < tagArr.length; ++i,++j) {
                tagArr[i].setVisibility(View.VISIBLE);
                tagArr[i].setText(tagList.get(j));
            }
            etTitle.setText(row.getTitle());
            etDescr.setText(row.getDescription());

            Glide.with(context)
                .load(row.getImageUrl())
                .apply(new RequestOptions()
                        .placeholder(R.drawable.help)
                        .error(R.drawable.help)
                        .fallback(R.drawable.help))
                .into(imIcon);

            if (listener != null) {
                this.view.setOnClickListener((View v) -> {
                    listener.onItemClick(rowList, position);
                });
            }
        }
    }

    /**
     * TagListAdapter constructor
     * @param context Context in which this adapter object will be used.
     * @param rowList List of FeedRow objects that this adapter object will be responsible to
     *                send to RecyclerView objects this adapter is attached to.
     * @param listener Listener object whose onItemClick method will be called
     *                 when a given item is clicked. If null, then nothing happens.
     */
    public ListItemsAdapter(Context context, List<FeedRow> rowList, OnItemClickListener listener) {
        this.context = context;
        this.rowList = rowList;
        this.listener = listener;

    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.list_item_layout, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(RecyclerView.ViewHolder holder, int position) {
        ((ViewHolder)holder).bind(this.rowList, position, context, listener);
    }

    @Override
    public int getItemCount() {
        return this.rowList.size();
    }
}
