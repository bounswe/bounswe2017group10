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
import java.util.List;

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
        private TextView etTag1;
        private TextView etTag2;
        private TextView etTag3;
    }

    @Override
    public View getView(int pos, View convertView, ViewGroup parent) {
        ViewHolder holder;

        if (convertView == null) {
            convertView = inflater.inflate(R.layout.list_item_layout, parent, false);
            holder = new ViewHolder();
            holder.etTitle = convertView.findViewById(R.id.title_textview);
            holder.etDescr = convertView.findViewById(R.id.description_textview);
            holder.imIcon = convertView.findViewById(R.id.icon_imageview);
            holder.etTag1 = convertView.findViewById(R.id.tag1);
            holder.etTag2 = convertView.findViewById(R.id.tag2);
            holder.etTag3 = convertView.findViewById(R.id.tag3);

            convertView.setTag(holder);
        } else {
            holder = (ViewHolder) convertView.getTag();
        }

        FeedRow row = items.get(pos);
        List<String> tagList = row.getTagList();
        TextView[] tagArr = {holder.etTag1, holder.etTag2, holder.etTag3};
        int i = 0;
        for (; i < tagList.size(); ++i) {
            tagArr[i].setText(tagList.get(i));
        }
        for (; i < tagArr.length; ++i) {
            tagArr[i].setVisibility(View.GONE);
        }
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
