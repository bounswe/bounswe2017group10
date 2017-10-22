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
    private LayoutInflater inflater;


    public FeedListAdapter(Context context, ArrayList<FeedRow> items) {
        super(context, -1, items);
        this.context = context;
        this.items = items;
        this.inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    }

    static class ViewHolder {
        private TextView etTitle;
        private TextView etDescr;
        private ImageView imIcon;
    }

    @Override
    public View getView(int pos, View convertView, ViewGroup parent) {
        ViewHolder holder;

        if (convertView == null) {
            convertView = inflater.inflate(R.layout.feed_list_item, parent, false);
            holder = new ViewHolder();
            holder.etTitle = convertView.findViewById(R.id.title_textview);
            holder.etDescr = convertView.findViewById(R.id.description_textview);
            holder.imIcon = convertView.findViewById(R.id.icon_imageview);
            convertView.setTag(holder);
        } else {
            holder = (ViewHolder) convertView.getTag();
        }

        FeedRow row = items.get(pos);
        holder.etTitle.setText(row.getTitle());
        holder.etDescr.setText(row.getDescription());
        Glide.with(context)
                .load(row.getImageUrl())
                .apply(new RequestOptions()
                        .placeholder(R.drawable.help)
                        .error(R.drawable.help)
                        .fallback(R.drawable.help))
                .into(holder.imIcon);

        return convertView;
    }
}
