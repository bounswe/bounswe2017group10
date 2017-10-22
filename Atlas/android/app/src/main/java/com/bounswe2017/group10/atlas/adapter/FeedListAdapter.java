package com.bounswe2017.group10.atlas.adapter;


import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bumptech.glide.Glide;
import com.bumptech.glide.request.RequestOptions;

import java.util.ArrayList;

public class FeedListAdapter extends ArrayAdapter<FeedRow> {
    private final Context context;
    private final ArrayList<FeedRow> items;

    public FeedListAdapter(Context context, ArrayList<FeedRow> items) {
        super(context, -1, items);
        this.context = context;
        this.items = items;
    }

    @Override
    public View getView(int pos, View convertView, ViewGroup parent) {
        LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        View rowView = inflater.inflate(R.layout.feed_list_item, parent, false);
        TextView etTitle = rowView.findViewById(R.id.title);
        TextView etDescr = rowView.findViewById(R.id.description);
        ImageView imIcon = rowView.findViewById(R.id.icon);

        FeedRow row = items.get(pos);
        etTitle.setText(row.getTitle());
        etDescr.setText(row.getDescription());
        Glide.with(context)
                .load(row.getImageUrl())
                .apply(new RequestOptions()
                        .placeholder(R.drawable.help)
                        .error(R.drawable.help)
                        .fallback(R.drawable.help))
                .into(imIcon);

        return rowView;
    }
}
