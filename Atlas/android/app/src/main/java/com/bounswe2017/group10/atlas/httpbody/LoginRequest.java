package com.bounswe2017.group10.atlas.httpbody;


import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;

public class LoginRequest {

    @SerializedName("username_or_email")
    @Expose
    private String usernameOrEmail;

    @SerializedName("password")
    @Expose
    private String password;

    public String getUsernameOrEmail() {
        return usernameOrEmail;
    }

    public void setUsernameOrEmail(String usernameOrEmail) {
        this.usernameOrEmail = usernameOrEmail;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof LoginRequest) {
            LoginRequest ref = (LoginRequest)obj;
            return this.getUsernameOrEmail().equals(ref.getUsernameOrEmail()) &&
                   this.getPassword().equals(ref.getPassword());
        } else {
            return false;
        }
    }
}
