package com.bounswe2017.group10.atlas.home;


import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.ListFragment;
import android.view.View;
import android.widget.ListView;


import java.util.ArrayList;

public class FeedFragment extends ListFragment {

    @Nullable
    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);

        final ArrayList<FeedRow> list = new ArrayList<>();
        list.add(new FeedRow("https://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/Good_Food_Display_-_NCI_Visuals_Online.jpg/1200px-Good_Food_Display_-_NCI_Visuals_Online.jpg", "Food", "This is a food description"));
        list.add(new FeedRow("https://ec.europa.eu/sport/sites/sport/files/shutterstock_304691351.jpg", "Sport", "This is a sport description"));

        final FeedArrayAdapter adapter = new FeedArrayAdapter(getActivity(), list);
        setListAdapter(adapter);
    }

    @Override
    public void onListItemClick(ListView listView, View view, int pos, long id) {
        // handle list click
    }
}
