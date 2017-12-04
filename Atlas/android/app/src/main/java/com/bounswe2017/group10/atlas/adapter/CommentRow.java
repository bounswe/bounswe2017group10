package com.bounswe2017.group10.atlas.adapter;


/**
 * This class represents a single row of user feed. Data stored inside this object
 * is mapped to row view.
 */
public class CommentRow {
    private String name;
    private String date;
    private String text;
    private String avatar;

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

        String[] parts = date.split("[.]");
        if(parts.length == 2) {
            date = parts[0];
        }
        parts = date.split("T");
        if(parts.length == 2) {
            date = "" + parts[0] + " " + parts[1];
        }
        this.date = date;

    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getAvatar() {
        return avatar;
    }

    public void setAvatar(String avatar) {
        this.avatar = avatar;
    }

    public CommentRow() {

    }

    public CommentRow(String name, String date, String text, String avatar) {
        this.name = name;

        String[] parts = date.split("[.]");
        if(parts.length == 2) {
            date = parts[0];
        }
        parts = date.split("T");
        if(parts.length == 2) {
            date = "" + parts[0] + " " + parts[1];
        }
        this.date = date;

        this.date = date;
        this.text = text;
        this.avatar = avatar;
    }

}
