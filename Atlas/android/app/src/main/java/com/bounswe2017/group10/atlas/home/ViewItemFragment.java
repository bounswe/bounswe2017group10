package com.bounswe2017.group10.atlas.home;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.support.v4.app.Fragment;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v7.widget.RecyclerView;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
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

    private CultureItem mItem;
    private CommentAdapter mAdapter;
    private final ArrayList<CommentRow> mRowList = new ArrayList<>();
    private final ArrayList<Comment> mCommentList = new ArrayList<>();
    boolean isFirstTimeClickToEdit = true;
    private Activity mActivity;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        setHasOptionsMenu(true);
        super.onCreate(savedInstanceState);
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_view_item, container, false);
        mItem = getArguments().getParcelable(Constants.CULTURE_ITEM);
        mActivity = getActivity();

        RecyclerView tagRecyclerView = view.findViewById(R.id.tag_recyclerview);
        setTags(tagRecyclerView, mItem);

        TextView ewTitle = view.findViewById(R.id.itemTitle);
        TextView ewDescription = view.findViewById(R.id.itemDesc);
        setText(ewTitle, ewDescription, mItem);

        Gallery gallery = view.findViewById(R.id.image_gallery);
        setImages(gallery, mItem);

        NoScrollListView listView = view.findViewById(R.id.comment_listview);

        mAdapter = new CommentAdapter(mActivity, mRowList);
        listView.setAdapter(mAdapter);

        for(Comment comment : mItem.getComments()) {
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
            String authStr = Utils.getSharedPref(mActivity).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
            OnPostCommentResponse respHandler = new OnPostCommentResponse(mActivity, mCommentList, mRowList, mAdapter);
            Comment pack = new Comment();
            pack.setText(text);
            PostCommentRequest requestBody = new PostCommentRequest();
            requestBody.setComment(pack);
            APIUtils.serverAPI().postComment(authStr,mItem.getId(), requestBody).enqueue(respHandler);
        });

        return view;
    }

    /**
     * Set the buttons in the action bar. Show edit/delete buttons only if the
     * current user is the creator of the item.
     *
     * @param menu Menu object.
     * @param inflater MenuInflater object.
     */
    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        inflater.inflate(R.menu.action_view_item, menu);

        long currentUserId = Utils.getSharedPref(mActivity).getLong(Constants.USER_ID, -1);
        if (currentUserId != mItem.getUser()) {
            menu.findItem(R.id.action_edit).setVisible(false);
            menu.findItem(R.id.action_delete).setVisible(false);
        }

        super.onCreateOptionsMenu(menu, inflater);
    }

    /**
     * Select actions to perform when one of the buttons in the action bar is clicked.
     *
     * @param item MenuItem object (button).
     * @return True if we can handle the request; delegate to superclass if not.
     */
    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.action_favorite:
                // TODO: Favorite item feature
                return true;
            case R.id.action_edit:
                Intent intent = new Intent(mActivity, CreateItemActivity.class);
                intent.putExtra(Constants.CULTURE_ITEM, mItem);
                intent.putExtra(Constants.CREATE_STRATEGY, Constants.EDIT);
                startActivity(intent);
                return true;
            case R.id.action_delete:
                deleteItem();
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    /**
     * Delete the current item and exit the current Fragment.
     */
    private void deleteItem() {
        String authStr = Utils.getSharedPref(mActivity).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
        APIUtils.serverAPI().deleteItem(authStr, mItem.getId()).enqueue(new Callback<Void>() {
            @Override
            public void onResponse(Call<Void> call, Response<Void> response) {
                if (response.isSuccessful()) {
                    Utils.showToast(mActivity, getString(R.string.successful_item_delete));
                    mActivity.onBackPressed();
                } else {
                    Utils.showToast(mActivity, getString(R.string.unable_to_delete));
                }
            }

            @Override
            public void onFailure(Call<Void> call, Throwable t) {
                Utils.showToast(mActivity, getString(R.string.connection_failure));
            }
        });
    }

    /**
     * Set the view to show the tags contained in the given CultureItem object.
     *
     * @param tagRecyclerView RecyclerView that is responsible for showing tags.
     * @param item CultureItem object.
     */
    private void setTags(RecyclerView tagRecyclerView, CultureItem item) {
        List<Tag> tagList = item.getTagList();
        TagListAdapter tagAdapter = new TagListAdapter(mActivity, tagList, null);
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
        gallery.setAdapter(new ImageListAdapter(mActivity, imageRowList));
        gallery.setOnItemClickListener((AdapterView<?> parent, View imgView, int position, long id) -> {
            // TODO: show image fullscreen
        });
    }

}
