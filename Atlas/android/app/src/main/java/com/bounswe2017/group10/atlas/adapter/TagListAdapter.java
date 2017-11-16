package com.bounswe2017.group10.atlas.adapter;


import android.content.Context;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.Tag;

import java.util.List;

/**
 * A Tag adapter class to be used with a RecyclerView object.
 */
public class TagListAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private Context context;
    private List<Tag> tagList;
    private OnItemClickListener listener = null;

    /**
     * RecyclerViews don't support item click listeners natively. We need to
     * come up with our own interface.
     */
    public interface OnItemClickListener {
        void onItemClick(List<Tag> tagList, int position);
    }

    /**
     * ViewHolder class that implements the ViewHolder pattern for a more
     * efficient adapter.
     */
    public static class ViewHolder extends RecyclerView.ViewHolder {
        TextView textView;

        ViewHolder(View v) {
            super(v);
            this.textView = v.findViewById(R.id.tag_textview);
        }

        void bind(List<Tag> tagList, int position, OnItemClickListener listener) {
            textView.setText(tagList.get(position).getName());
            if (listener != null) {
                textView.setOnClickListener((View v) -> {
                    listener.onItemClick(tagList, position);
                });
            }
        }
    }

    /**
     * TagListAdapter constructor
     * @param context Context in which this adapter object will be used.
     * @param tagList List of tags that this adapter object will be responsible to
     *                send to RecyclerView objects this adapter is attached to.
     * @param listener Listener object whose onItemClick method will be called
     *                 when a given item is clicked. If null, then nothing happens.
     */
    public TagListAdapter(Context context, List<Tag> tagList, OnItemClickListener listener) {
        this.context = context;
        this.tagList = tagList;
        this.listener = listener;
    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.tag_recyclerview_item, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(RecyclerView.ViewHolder holder, int position) {
        ((ViewHolder)holder).bind(this.tagList, position, listener);
    }

    @Override
    public int getItemCount() {
        return this.tagList.size();
    }
}
