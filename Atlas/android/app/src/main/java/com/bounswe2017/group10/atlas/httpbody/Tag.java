package com.bounswe2017.group10.atlas.httpbody;

import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;

import java.io.Serializable;

public class Tag implements Serializable {

    @SerializedName("name")
    @Expose
    private String name;

    public Tag(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Tag) {
            Tag tagObj = (Tag)obj;
            return this.getName().equals(tagObj.getName());
        }
        return false;
    }
}
