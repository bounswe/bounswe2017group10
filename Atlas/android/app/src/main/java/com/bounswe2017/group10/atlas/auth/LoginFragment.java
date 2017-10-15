package com.bounswe2017.group10.atlas.auth;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;

import com.bounswe2017.group10.atlas.R;

import retrofit2.HttpException;


public class LoginFragment extends Fragment {
    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_login, container, false);

        // set listeners
        Button btnForgotPassword = view.findViewById(R.id.forgot_password_button);
        Button btnLoginRequest = view.findViewById(R.id.login_request_button);

        btnForgotPassword.setOnClickListener((View btnView) -> {
            // TODO: Implement forgot password functionality
        });
        btnLoginRequest.setOnClickListener((View btnView) -> {
            // collect input from text fields
            // validate inputs
            // construct json containing user info
            String body = "login_json";
            try {
                String responseJson = AuthManager.login(body);
                // go to another activity
            } catch (HttpException e) {
                // error handling
            }
        });
        return view;
    }
}
