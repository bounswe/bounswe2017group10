package com.bounswe2017.group10.atlas.httpbody;


import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;

import java.util.List;

public class GetItemsResponse {

    @SerializedName("count")
    @Expose
    private long count;

    @SerializedName("previous")
    @Expose
    private String previous;

    @SerializedName("next")
    @Expose
    private String next;

    @SerializedName("results")
    @Expose
    private List<CultureItem> results;

    public long getCount() {
        return count;
    }

    public void setCount(long count) {
        this.count = count;
    }

    public String getPrevious() {
        return previous;
    }

    public void setPrevious(String previous) {
        this.previous = previous;
    }

    public String getNext() {
        return next;
    }

    public void setNext(String next) {
        this.next = next;
    }

    public List<CultureItem> getResults() {
        return results;
    }

    public void setResults(List<CultureItem> results) {
        this.results = results;
    }
}
