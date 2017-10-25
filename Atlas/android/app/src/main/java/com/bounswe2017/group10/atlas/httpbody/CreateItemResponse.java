package com.bounswe2017.group10.atlas.httpbody;


import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;

public class CreateItemResponse {

    @SerializedName("id")
    @Expose
    private int id;

    @SerializedName("created_time")
    @Expose
    private String createdTime;

    @SerializedName("updated_time")
    @Expose
    private String updatedTime;

    public String getCreatedTime() {
        return createdTime;
    }

    public void setCreatedTime(String createdTime) {
        this.createdTime = createdTime;
    }

    public String getUpdatedTime() {
        return updatedTime;
    }

    public void setUpdatedTime(String updatedTime) {
        this.updatedTime = updatedTime;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }
}
