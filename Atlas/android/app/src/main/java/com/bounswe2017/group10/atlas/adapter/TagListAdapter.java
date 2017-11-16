package com.bounswe2017.group10.atlas.adapter;


import android.content.Context;
import android.support.v7.widget.RecyclerView;
import android.text.Layout;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.Tag;

import java.util.List;

public class TagListAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private Context context;
    private List<Tag> tagList;
    private View view;
    private ViewHolder viewHolder;
    private OnItemClickListener listener = null;

    public interface OnItemClickListener {
        void onItemClick(List<Tag> tagList, int position);
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        public TextView textView;

        public ViewHolder(View v) {
            super(v);
            this.textView = v.findViewById(R.id.tag_textview);
        }

        public void bind(List<Tag> tagList, int position, OnItemClickListener listener) {
            textView.setText(tagList.get(position).getName());
            if (listener != null) {
                textView.setOnClickListener((View v) -> {
                    listener.onItemClick(tagList, position);
                });
            }
        }
    }

    public TagListAdapter(Context context, List<Tag> tagList, OnItemClickListener listener) {
        this.context = context;
        this.tagList = tagList;
        this.listener = listener;
    }


    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        this.view = LayoutInflater.from(context).inflate(R.layout.tag_recyclerview_item, parent, false);
        this.viewHolder = new ViewHolder(view);
        return this.viewHolder;
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
