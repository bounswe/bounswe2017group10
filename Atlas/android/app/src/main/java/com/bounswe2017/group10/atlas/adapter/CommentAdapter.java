package com.bounswe2017.group10.atlas.adapter;


import android.content.Context;
import android.icu.text.SimpleDateFormat;
import android.os.Build;
import android.support.annotation.NonNull;
import android.support.annotation.RequiresApi;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bumptech.glide.Glide;
import com.bumptech.glide.request.RequestOptions;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.TimeZone;
import java.util.concurrent.TimeUnit;

public class CommentAdapter extends ArrayAdapter<CommentRow> {
    private final Context context;
    private final ArrayList<CommentRow> items;
    private LayoutInflater inflater;

    public static int time_zone = TimeZone.getDefault().getRawOffset();


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
        private ImageView avatarH;
    }

    @RequiresApi(api = Build.VERSION_CODES.N)
    @Override
    public View getView(int pos,View convertView, ViewGroup parent) {
        ViewHolder holder;

        if (convertView == null) {
            convertView = inflater.inflate(R.layout.comment_item, parent, false);
            holder = new ViewHolder();
            holder.nameH = convertView.findViewById(R.id.commenter_name);
            holder.dateH = convertView.findViewById(R.id.comment_date);
            holder.textH = convertView.findViewById(R.id.comment_text);
            holder.avatarH = convertView.findViewById(R.id.comment_avatar);
            convertView.setTag(holder);
        } else {
            holder = (ViewHolder) convertView.getTag();
        }

        CommentRow row = items.get(pos);
        holder.nameH.setText(row.getName());
        holder.dateH.setText(getDateString(row.getDate()));
        holder.textH.setText(row.getText());

        Glide.with(context)
                .load(row.getAvatar())
                .apply(new RequestOptions()
                        .error(R.drawable.help))
                .into(holder.avatarH);

        return convertView;
    }

    @Override
    public int getCount() {
        return this.items.size();
    }

    @RequiresApi(api = Build.VERSION_CODES.N)
    public String getDateString(String date) {


        // Custom date format
        SimpleDateFormat inputFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        Date dateObj = new Date();
        Date  now = new Date();

        try {
            dateObj = inputFormat.parse(date);
        } catch (ParseException e) {
            e.printStackTrace();
        }

        //Time zone difference
        //1000 is for preventing negative diff values.
        dateObj.setTime(dateObj.getTime() + time_zone - 1000);

        long diff = now.getTime() - dateObj.getTime();
        long seconds = TimeUnit.MILLISECONDS.toSeconds(diff);
        long minutes = TimeUnit.MILLISECONDS.toMinutes(diff);
        long hours = TimeUnit.MILLISECONDS.toHours(diff);
        long days = TimeUnit.MILLISECONDS.toDays(diff);

        if((int) seconds == 1) return context.getString(R.string.second_ago);
        else if(seconds < 60) return Long.toString(seconds) + " " + context.getString(R.string.seconds_ago);
        else if((int)minutes == 1) return context.getString(R.string.minute_ago);
        else if(minutes < 60) return Long.toString(minutes) + " " + context.getString(R.string.minutes_ago);
        else if((int)hours == 1) return context.getString(R.string.hour_ago);
        else if(hours < 24) return Long.toString(hours) + " " + context.getString(R.string.hours_ago);
        else if((int)days == 1) return context.getString(R.string.yesterday);
        else if(days < 30) return Long.toString(days) + " " + context.getString(R.string.days_ago);
        else return format.format(dateObj);

        //return format.format(dateObj) + "\n" +  date + "\n" + time_zone;
    }
}
