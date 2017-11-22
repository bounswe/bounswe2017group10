package com.bounswe2017.group10.atlas.adapter;


import android.content.Context;
import android.support.annotation.NonNull;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;

import java.util.ArrayList;

public class CommentAdapter extends ArrayAdapter<CommentRow> {
    private final Context context;
    private final ArrayList<CommentRow> items;
    private LayoutInflater inflater;




    public CommentAdapter(Context context, ArrayList<CommentRow> items) {
        super(context, -1, items);
        this.context = context;
        this.items = items;
        this.inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    }

    private static class ViewHolder {
        private TextView nameH;
        private TextView dateH;
        private TextView textH;
    }

    @NonNull
    @Override
    public View getView(int pos, View convertView, @NonNull ViewGroup parent) {
        ViewHolder holder;

        if (convertView == null) {
            convertView = inflater.inflate(R.layout.comment_item, parent, false);
            holder = new ViewHolder();
            holder.nameH = convertView.findViewById(R.id.comment_name);
            holder.dateH = convertView.findViewById(R.id.comment_date);
            holder.textH = convertView.findViewById(R.id.comment_text);
            convertView.setTag(holder);
        } else {
            holder = (ViewHolder) convertView.getTag();
        }

        CommentRow row = items.get(pos);
        holder.nameH.setText(row.getName());
        holder.dateH.setText(row.getDate());
        holder.textH.setText(row.getText());

        return convertView;
    }
}
