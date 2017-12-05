package com.bounswe2017.group10.atlas.adapter;


import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;

import com.bounswe2017.group10.atlas.R;
import com.bumptech.glide.Glide;
import com.bumptech.glide.request.RequestOptions;

import java.util.ArrayList;

public class ImageListAdapter extends ArrayAdapter<ImageRow> {
    private final Context context;
    private final ArrayList<ImageRow> items;
    private LayoutInflater inflater;

    public ImageListAdapter(Context context, ArrayList<ImageRow> items) {
        super(context, -1, items);
        this.context = context;
        this.items = items;
        this.inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    }

    static class ViewHolder {
        ImageView img;
    }

    @Override
    public View getView(int pos, View convertView, ViewGroup parent) {
        ViewHolder holder;

        if (convertView == null) {
            convertView = inflater.inflate(R.layout.image_list_item, parent, false);
            holder = new ViewHolder();
            holder.img = convertView.findViewById(R.id.image);
            convertView.setTag(holder);
        } else {
            holder = (ViewHolder) convertView.getTag();
        }

        ImageRow row = items.get(pos);
        // set content description to Uri
        holder.img.setContentDescription(row.getUri().toString());
        Glide.with(context)
                .load(row.getUri())
                .apply(new RequestOptions()
                        .centerCrop()
                        .placeholder(R.drawable.ic_crop_original_black_48dp)
                        .error(R.drawable.ic_crop_original_black_48dp)
                        .fallback(R.drawable.ic_crop_original_black_48dp))
                .into(holder.img);

        return convertView;
    }
}

