package com.bounswe2017.group10.atlas.adapter;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.ListView;

/**
 * Created by mutas on 19.11.2017.
 * NoScrollListView extends ListView by overwriting only onMeasure method.
 * This version adjusts its height according to its content.
 * Thus there is no scroll ability in this one.
 * This version is useful when the page has already a scroll view and second one would mess it up.
 */

public class NoScrollListView extends ListView {

    public NoScrollListView(Context context) {
        super(context);
    }

    public NoScrollListView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public NoScrollListView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public NoScrollListView(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
    }

    @Override
    public void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        int expandSpec = MeasureSpec.makeMeasureSpec(Integer.MAX_VALUE >> 2,
                MeasureSpec.AT_MOST);
        super.onMeasure(widthMeasureSpec, expandSpec);
    }
}
