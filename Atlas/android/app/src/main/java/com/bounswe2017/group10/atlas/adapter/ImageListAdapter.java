package com.bounswe2017.group10.atlas.adapter;


import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;

import com.bounswe2017.group10.atlas.R;
import com.bumptech.glide.Glide;

import java.util.ArrayList;

public class ImageListAdapter extends ArrayAdapter<ImageRow> {
    private final Context context;
    private final ArrayList<ImageRow> items;

    public ImageListAdapter(Context context, ArrayList<ImageRow> items) {
        super(context, -1, items);
        this.context = context;
        this.items = items;
    }

    @Override
    public View getView(int pos, View convertView, ViewGroup parent) {
        LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        View rowView = inflater.inflate(R.layout.image_list_item, parent, false);
        ImageView image = rowView.findViewById(R.id.image);

        ImageRow row = items.get(pos);
        Glide.with(context).load(row.getUrl()).into(image);

        return rowView;
    }
}

