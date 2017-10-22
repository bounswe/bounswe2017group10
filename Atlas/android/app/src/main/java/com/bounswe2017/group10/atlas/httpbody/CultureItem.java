package com.bounswe2017.group10.atlas.httpbody;

import com.bounswe2017.group10.atlas.adapter.FeedRow;
import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;

import java.util.List;

public class CultureItem {

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

    @SerializedName("image_url")
    @Expose
    private List<String> imageUrlList;

    @SerializedName("public_accessibility")
    @Expose
    private Boolean publicAccessibility;

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

    public List<String> getImageUrlList() {
        return imageUrlList;
    }

    public void setImageUrlList(List<String> imageUrlList) {
        this.imageUrlList = imageUrlList;
    }

    public Boolean getPublicAccessibility() {
        return publicAccessibility;
    }

    public void setPublicAccessibility(Boolean publicAccessibility) {
        this.publicAccessibility = publicAccessibility;
    }

    public FeedRow toFeedRow() {
        List<String> imageUrlList = getImageUrlList();
        if (imageUrlList == null) {
            return new FeedRow("","","");
        }
        return new FeedRow(imageUrlList.get(0), getTitle(), getDescription());
    }

}
