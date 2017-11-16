package com.bounswe2017.group10.atlas.home;

import android.net.Uri;
import android.support.v4.app.Fragment;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.Gallery;
import android.widget.ImageView;
import android.widget.TextView;
import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.ImageListAdapter;
import com.bounswe2017.group10.atlas.adapter.ImageRow;
import com.bounswe2017.group10.atlas.adapter.TagListAdapter;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.httpbody.Tag;
import com.bounswe2017.group10.atlas.util.Constants;

import java.util.ArrayList;
import java.util.List;

public class ViewItemFragment extends Fragment {

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_view_item, container, false);
        CultureItem item = getArguments().getParcelable(Constants.CULTURE_ITEM);

        RecyclerView tagRecyclerview = view.findViewById(R.id.tag_recyclerview);
        List<Tag> tagList = item.getTagList();
        TagListAdapter tagAdapter = new TagListAdapter(getActivity(), tagList);
        tagRecyclerview.setAdapter(tagAdapter);

        TextView viewItemTitle = view.findViewById(R.id.itemTitle);
        TextView viewItemDesc = view.findViewById(R.id.itemDesc);
        viewItemTitle.setText(item.getTitle());
        viewItemDesc.setText(item.getDescription());

        ArrayList<ImageRow> imageRowList = new ArrayList<>();
        for (Image img : item.getImageList()) {
            ImageRow row = new ImageRow();
            row.setUri(Uri.parse(img.getUrl()));
            imageRowList.add(row);
        }
        Gallery gallery = view.findViewById(R.id.image_gallery);
        gallery.setAdapter(new ImageListAdapter(getActivity(), imageRowList));
        gallery.setOnItemClickListener((AdapterView<?> parent, View imgView, int position, long id) -> {
            // TODO: show image fullscreen
        });

        return view;
    }

}
