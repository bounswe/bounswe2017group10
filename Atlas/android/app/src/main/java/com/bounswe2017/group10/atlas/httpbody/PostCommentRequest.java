package com.bounswe2017.group10.atlas.httpbody;


import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;

public class PostCommentRequest {

    @SerializedName("comment")
    @Expose
    private Comment comment;

    public Comment getComment() {
        return comment;
    }

    public void setComment(Comment comment) {
        this.comment = comment;
    }
}
