package com.bounswe2017.group10.atlas.adapter;


import android.content.Context;
import android.support.v7.widget.RecyclerView;
import android.util.DisplayMetrics;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;

import com.bounswe2017.group10.atlas.R;
import com.bumptech.glide.Glide;
import com.bumptech.glide.request.RequestOptions;

import java.util.ArrayList;
import java.util.List;

public class ImageListAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {
    private final Context context;
    private final ArrayList<ImageRow> imgList;
    private OnItemClickListener listener;
    private boolean isCenterCropped = true;

    public ImageListAdapter(Context context, ArrayList<ImageRow> imgList, OnItemClickListener listener) {
        this.context = context;
        this.imgList = imgList;
        this.listener = listener;
    }

    public void setCenterCropped(boolean isCenterCropped) {
        this.isCenterCropped = isCenterCropped;
    }

    /**
     * RecyclerViews don't support item click listeners natively. We need to
     * come up with our own interface.
     */
    public interface OnItemClickListener {
        void onItemClick(List<ImageRow> rowList, int position);
    }

    static class ViewHolder extends RecyclerView.ViewHolder {
        ImageView img;
        View view;

        ViewHolder(View v) {
            super(v);
            this.img = v.findViewById(R.id.image);
            this.view = v;
        }

        void bind(List<ImageRow> rowList, int position, boolean isCenterCropped, Context context, OnItemClickListener listener) {
            ImageRow row = rowList.get(position);
            img.setContentDescription(row.getUri().toString());

            RequestOptions options = new RequestOptions();
            options = options.placeholder(R.drawable.ic_crop_original_black_48dp)
                    .error(R.drawable.ic_crop_original_black_48dp)
                    .fallback(R.drawable.ic_crop_original_black_48dp);
            if (isCenterCropped) {
                options = options.centerCrop();
            }
            Glide.with(context)
                    .load(row.getUri().toString())
                    .apply(options)
                    .into(img);

            if (listener != null) {
                this.view.setOnClickListener((View v) -> {
                    listener.onItemClick(rowList, position);
                });
            }
        }
    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.image_list_item, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(RecyclerView.ViewHolder holder, int position) {
        ((ViewHolder)holder).bind(this.imgList, position, isCenterCropped, context, listener);
    }

    @Override
    public int getItemCount() {
        return this.imgList.size();
    }
}

