package com.bounswe2017.group10.atlas.home;

import android.net.Uri;
import android.support.v4.app.Fragment;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Gallery;
import android.widget.TextView;
import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.adapter.CommentAdapter;
import com.bounswe2017.group10.atlas.adapter.CommentRow;
import com.bounswe2017.group10.atlas.adapter.ImageListAdapter;
import com.bounswe2017.group10.atlas.adapter.ImageRow;
import com.bounswe2017.group10.atlas.adapter.NoScrollListView;
import com.bounswe2017.group10.atlas.adapter.TagListAdapter;
import com.bounswe2017.group10.atlas.httpbody.Comment;
import com.bounswe2017.group10.atlas.httpbody.CultureItem;
import com.bounswe2017.group10.atlas.httpbody.Image;
import com.bounswe2017.group10.atlas.httpbody.PostCommentRequest;
import com.bounswe2017.group10.atlas.httpbody.Tag;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.response.OnPostCommentResponse;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;

import java.util.ArrayList;
import java.util.List;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

public class ViewItemFragment extends Fragment {

    private CommentAdapter mAdapter;
    private final ArrayList<CommentRow> mRowList = new ArrayList<>();
    private final ArrayList<Comment> mCommentList = new ArrayList<>();
    boolean isFirstTimeClickToEdit = true;

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_view_item, container, false);
        CultureItem item = getArguments().getParcelable(Constants.CULTURE_ITEM);

        RecyclerView tagRecyclerView = view.findViewById(R.id.tag_recyclerview);
        setTags(tagRecyclerView, item);

        TextView ewTitle = view.findViewById(R.id.itemTitle);
        TextView ewDescription = view.findViewById(R.id.itemDesc);
        setText(ewTitle, ewDescription, item);

        Gallery gallery = view.findViewById(R.id.image_gallery);
        setImages(gallery, item);

        Button btnEdit = view.findViewById(R.id.edit_button);
        btnEdit.setOnClickListener((View v) -> {

        });

        Button btnDelete = view.findViewById(R.id.delete_button);
        btnDelete.setOnClickListener((View v) -> {
            String authStr = Utils.getSharedPref(getActivity()).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
            APIUtils.serverAPI().deleteItem(authStr, item.getId()).enqueue(new Callback<Void>() {
                @Override
                public void onResponse(Call<Void> call, Response<Void> response) {
                    if (response.isSuccessful()) {
                        Utils.showToast(getActivity(), getString(R.string.successful_item_delete));
                        getActivity().onBackPressed();
                    } else {
                        Utils.showToast(getActivity(), getString(R.string.unable_to_delete));
                    }
                }

                @Override
                public void onFailure(Call<Void> call, Throwable t) {
                    Utils.showToast(getActivity(), getString(R.string.connection_failure));
                }
            });
        });

        NoScrollListView listView = view.findViewById(R.id.comment_listview);



        mAdapter = new CommentAdapter(getActivity(), mRowList);
        listView.setAdapter(mAdapter);

        assert item != null;
        for(Comment comment : item.getComments()) {
            mCommentList.add(0,comment);
            mRowList.add(0,comment.toCommentRow());
        }

        EditText commentEdit = view.findViewById(R.id.comment_edit);
        Button sendButton = view.findViewById(R.id.comment_send);

        commentEdit.setOnClickListener((View btnView) -> {
            if(isFirstTimeClickToEdit)  {
                commentEdit.setText("");
                isFirstTimeClickToEdit = false;
            }
        });
        sendButton.setOnClickListener((View btnView) -> {
            String text = commentEdit.getText().toString();
            commentEdit.setText("");
            String authStr = Utils.getSharedPref(getActivity()).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
            OnPostCommentResponse respHandler = new OnPostCommentResponse(getActivity(), mCommentList, mRowList, mAdapter);
            Comment pack = new Comment();
            pack.setText(text);
            PostCommentRequest requestBody = new PostCommentRequest();
            requestBody.setComment(pack);
            APIUtils.serverAPI().postComment(authStr,item.getId(), requestBody).enqueue(respHandler);
        });




        return view;
    }

    /**
     * Set the view to show the tags contained in the given CultureItem object.
     *
     * @param tagRecyclerView RecyclerView that is responsible for showing tags.
     * @param item CultureItem object.
     */
    private void setTags(RecyclerView tagRecyclerView, CultureItem item) {
        List<Tag> tagList = item.getTagList();
        TagListAdapter tagAdapter = new TagListAdapter(getActivity(), tagList, null);
        tagRecyclerView.setAdapter(tagAdapter);
    }

    /**
     * Set the view to show item's title and description.
     *
     * @param twTitle TextView responsible for showing the title of the CultureItem.
     * @param twDescription TextView responsible for showing the description of the CultureItem.
     * @param item CultureItem object.
     *
     * TODO: Show other text information of a CultureItem.
     */
    private void setText(TextView twTitle, TextView twDescription, CultureItem item) {
        twTitle.setText(item.getTitle());
        twDescription.setText(item.getDescription());
    }

    /**
     * Set the view to show the media items of the given CultureItem.
     *
     * @param gallery Gallery object responsible for showing all the media items of a given CultureItem.
     * @param item CultureItem object.
     */
    private void setImages(Gallery gallery, CultureItem item) {
        ArrayList<ImageRow> imageRowList = new ArrayList<>();
        for (Image img : item.getImageList()) {
            ImageRow row = new ImageRow();
            row.setUri(Uri.parse(img.getUrl()));
            imageRowList.add(row);
        }
        gallery.setAdapter(new ImageListAdapter(getActivity(), imageRowList));
        gallery.setOnItemClickListener((AdapterView<?> parent, View imgView, int position, long id) -> {
            // TODO: show image fullscreen
        });
    }

}
