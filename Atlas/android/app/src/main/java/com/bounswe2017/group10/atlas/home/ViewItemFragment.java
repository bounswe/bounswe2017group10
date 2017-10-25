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
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bumptech.glide.Glide;

import java.util.List;

public class ViewItemFragment extends Fragment {

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_view_item, container, false);

        CultureItem item = getArguments().getParcelable(Constants.CULTURE_ITEM);

        TextView viewItemTitle = view.findViewById(R.id.itemTitle);
        ImageView viewItemImage = view.findViewById(R.id.itemImage);
        TextView viewItemDesc = view.findViewById(R.id.itemDesc);
        viewItemTitle.setText(item.getTitle());
        viewItemDesc.setText(item.getDescription());

        List<Image> imageList = item.getImageList();

        // currently simply load the first image
        if (imageList.size() != 0) {
            Glide.with(getActivity())
                    .load(item.getImageList().get(0).getUrl())
                    .into(viewItemImage);
        }

        return view;
    }

}
