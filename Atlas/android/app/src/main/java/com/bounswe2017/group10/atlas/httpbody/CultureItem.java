package com.bounswe2017.group10.atlas.httpbody;

import android.os.Parcel;
import android.os.Parcelable;

import com.bounswe2017.group10.atlas.adapter.FeedRow;
import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;

import java.util.ArrayList;
import java.util.List;

public class CultureItem implements Parcelable {

    @SerializedName("id")
    @Expose
    private long id;

    @SerializedName("country")
    @Expose
    private String country;

    @SerializedName("title")
    @Expose
    private String title;

    @SerializedName("description")
    @Expose
    private String description;

    @SerializedName("continent")
    @Expose
    private String continent;

    @SerializedName("city")
    @Expose
    private String city;

    @SerializedName("images")
    @Expose
    private ArrayList<Image> imageList;

    @SerializedName("tags")
    @Expose
    private ArrayList<Tag> tagList;

    @SerializedName("public_accessibility")
    @Expose
    private Boolean publicAccessibility;

    @SerializedName("comments")
    @Expose
    private ArrayList<Comment> comments;

    public CultureItem() {}

    @SuppressWarnings("unchecked")
    public CultureItem(Parcel in) {
        this.country = in.readString();
        this.title = in.readString();
        this.description = in.readString();
        this.continent = in.readString();
        this.city = in.readString();
        this.imageList = (ArrayList<Image>)in.readSerializable();
        this.publicAccessibility = in.readByte() != 0;

    }

    @Override
    public void writeToParcel(Parcel out, int flags) {
        out.writeString(this.country);
        out.writeString(this.title);
        out.writeString(this.description);
        out.writeString(this.continent);
        out.writeString(this.city);
        out.writeSerializable(this.imageList);
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

    public String getCountry() {
        return country;
    }

    public void setCountry(String country) {
        this.country = country;
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

    public String getContinent() {
        return continent;
    }

    public void setContinent(String continent) {
        this.continent = continent;
    }

    public String getCity() {
        return city;
    }

    public void setCity(String city) {
        this.city = city;
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
        return new FeedRow(url, getTitle(), getDescription(), tagList);
    }

    public ArrayList<Comment> getComments() {
        return comments;
    }

    public void setComments(ArrayList<Comment> comments) {
        this.comments = comments;
    }
}
