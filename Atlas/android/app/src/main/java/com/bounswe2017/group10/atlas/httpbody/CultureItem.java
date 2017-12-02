package com.bounswe2017.group10.atlas.httpbody;

import android.os.Parcel;
import android.os.Parcelable;

import com.bounswe2017.group10.atlas.adapter.FeedRow;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;
import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;

import java.util.ArrayList;
import java.util.List;

public class CultureItem implements Parcelable {

    @SerializedName("id")
    @Expose
    private long id;

    @SerializedName("user")
    @Expose
    private long user;

    @SerializedName("title")
    @Expose
    private String title;

    @SerializedName("description")
    @Expose
    private String description;

    @SerializedName("place_name")
    @Expose
    private String placeName;

    @SerializedName("start_year")
    @Expose
    private Integer startYear;

    @SerializedName("end_year")
    @Expose
    private Integer endYear;

    @SerializedName("latitude")
    @Expose
    private String latitude;

    @SerializedName("longitude")
    @Expose
    private String longitude;

    @SerializedName("images")
    @Expose
    private ArrayList<Image> imageList;

    @SerializedName("tags")
    @Expose
    private ArrayList<Tag> tagList;

    @SerializedName("comments")
    @Expose
    private ArrayList<Comment> commentList;

    @SerializedName("public_accessibility")
    @Expose
    private boolean publicAccessibility;

    public CultureItem() {
        this.imageList = new ArrayList<>();
        this.tagList = new ArrayList<>();
        this.commentList = new ArrayList<>();
        this.publicAccessibility = true;
    }

    @SuppressWarnings("unchecked")
    public CultureItem(Parcel in) {
        this.id = in.readLong();
        this.user = in.readLong();
        this.title = in.readString();
        this.description = in.readString();
        this.placeName = in.readString();
        this.startYear = (Integer)in.readSerializable();
        this.endYear = (Integer)in.readSerializable();
        this.latitude = in.readString();
        this.longitude = in.readString();
        this.imageList = (ArrayList<Image>)in.readSerializable();
        this.tagList = (ArrayList<Tag>)in.readSerializable();
        this.commentList = (ArrayList<Comment>)in.readSerializable();
        this.publicAccessibility = in.readByte() != 0;
    }

    @Override
    public void writeToParcel(Parcel out, int flags) {
        out.writeLong(this.id);
        out.writeLong(this.user);
        out.writeString(this.title);
        out.writeString(this.description);
        out.writeString(this.placeName);
        out.writeSerializable(this.startYear);
        out.writeSerializable(this.endYear);
        out.writeString(this.latitude);
        out.writeString(this.longitude);
        out.writeSerializable(this.imageList);
        out.writeSerializable(this.tagList);
        out.writeSerializable(this.commentList);
        out.writeByte((byte) (this.publicAccessibility ? 1 : 0));
    }

    @Override
    public int describeContents() {
        return this.hashCode();
    }

    public static final Parcelable.Creator<CultureItem> CREATOR =
        new Parcelable.Creator<CultureItem>() {
            @Override
            public CultureItem createFromParcel(Parcel in) {
                return new CultureItem(in);
            }

            @Override
            public CultureItem[] newArray(int count) {
                return new CultureItem[count];
            }
        };

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public long getUser() {
        return user;
    }

    public void setUser(long user) {
        this.user = user;
    }


    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getPlaceName() {
        return placeName;
    }

    public void setPlaceName(String placeName) {
        this.placeName = placeName;
    }

    public Integer getStartYear() {
        return startYear;
    }

    public void setStartYear(int startYear) {
        this.startYear = startYear;
    }

    public Integer getEndYear() {
        return endYear;
    }

    public void setEndYear(int endYear) {
        this.endYear = endYear;
    }

    public void setStartYear(Integer startYear) {
        this.startYear = startYear;
    }

    public void setEndYear(Integer endYear) {
        this.endYear = endYear;
    }

    public String getLatitude() {
        return latitude;
    }

    public void setLatitude(String latitude) {
        this.latitude = latitude;
    }

    public String getLongitude() {
        return longitude;
    }

    public void setLongitude(String longitude) {
        this.longitude = longitude;
    }

    public boolean isPublicAccessibility() {
        return publicAccessibility;
    }

    public void setPublicAccessibility(boolean publicAccessibility) {
        this.publicAccessibility = publicAccessibility;
    }

    public ArrayList<Image> getImageList() {
        return imageList;
    }

    public void setImageList(ArrayList<Image> imageList) {
        this.imageList = imageList;
    }

    public ArrayList<Tag> getTagList() {
        return tagList;
    }

    public void setTagList(ArrayList<Tag> tagList) {
        this.tagList = tagList;
    }

    public Boolean getPublicAccessibility() {
        return publicAccessibility;
    }

    public void setPublicAccessibility(Boolean publicAccessibility) {
        this.publicAccessibility = publicAccessibility;
    }

    public FeedRow toFeedRow() {
        List<Image> imgList = this.getImageList();
        String url = null;
        if (imgList.size() != 0) {
            url = imgList.get(0).getUrl();
        }

        List<String> tagList = new ArrayList<>();
        for (Tag t : this.getTagList()) {
            tagList.add(t.getName());
        }
        String year = null;
        if (startYear != null && endYear != null) {
            year = FeedRow.toYearFormat(getStartYear(), getEndYear());
        }
        return new FeedRow(url, getTitle(), getDescription(), getPlaceName(), year, tagList);
    }

    public ArrayList<Comment> getCommentList() {
        return commentList;
    }

    public void setCommentList(ArrayList<Comment> commentList) {
        this.commentList = commentList;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof CultureItem)) {
            return false;
        }
        CultureItem other = (CultureItem)obj;

        // everything must be equal
        return this.id == other.id &&
                this.user == other.user &&
                Utils.objectEquals(this.title, other.title) &&
                Utils.objectEquals(this.description, other.description) &&
                Utils.objectEquals(this.placeName, other.placeName) &&
                Utils.objectEquals(this.startYear, other.startYear) &&
                Utils.objectEquals(this.endYear, other.endYear) &&
                Utils.objectEquals(this.latitude, other.latitude) &&
                Utils.objectEquals(this.longitude, other.longitude) &&
                Utils.objectEquals(this.imageList, other.imageList) &&
                Utils.objectEquals(this.tagList, other.tagList) &&
                Utils.objectEquals(this.commentList, other.commentList) &&
                this.publicAccessibility == other.publicAccessibility;
    }
}
