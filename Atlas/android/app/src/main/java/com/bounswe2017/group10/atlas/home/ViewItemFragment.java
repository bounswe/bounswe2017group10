package com.bounswe2017.group10.atlas.home;

import android.support.v4.app.Fragment;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bumptech.glide.Glide;

public class ViewItemFragment extends Fragment {

    TextView viewItemTitle;
    ImageView viewItemImage;
    TextView viewItemDesc;
    CultureItem item;

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_view_item, container, false);

        viewItemTitle = view.findViewById(R.id.itemTitle);
        viewItemImage = view.findViewById(R.id.itemImage);
        viewItemDesc = view.findViewById(R.id.itemDesc);
        String authStr = getArguments().getString(Constants.AUTH_STR, "NO_TOKEN");
        viewItemTitle.setText(item.getTitle());
        viewItemDesc.setText(item.getDescription());

        //if (!item.getImageList().isEmpty()) {
        try {
            Glide.with(this)
                    .load(item.getImageList().get(0).getUrl())
                    .into(viewItemImage);
        } catch (Exception e) {
            e.printStackTrace();
        }
        //}

        return view;
    }

    public void setObj (CultureItem itemC) {
        this.item = itemC;
    }
}
