package com.bounswe2017.group10.atlas.adapter;


/**
 * This class represents a single row of user feed. Data stored inside this object
 * is mapped to row view.
 */
public class CommentRow {
    private String name;
    private String date;
    private String text;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public CommentRow() {

    }

    public CommentRow(String name, String date, String text) {
        this.name = name;
        this.date = date;
        this.text = text;
    }

}
