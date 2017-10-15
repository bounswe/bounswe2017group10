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


public class SignupFragment extends Fragment {
    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_signup, container, false);

        Button btnBirthDate = view.findViewById(R.id.birthdate_button);
        Button btnSignUpRequest = view.findViewById(R.id.signup_request_button);

        btnBirthDate.setOnClickListener((View btnView) -> {
            // show DatePicker
            // display the picked date as button text
        });
        btnSignUpRequest.setOnClickListener((View btnView) -> {
            // collect input from text fields
            // validate inputs
            // construct json containing user info
            String body = "signup_json";
            try {
                String responseJson = AuthManager.signup(body);
                // if success, log user in.
                body = "login_json";
                responseJson = AuthManager.login(body);
                // go to another activity
            } catch (HttpException e) {
                // error handling
            }
        });
        return view;
    }
}
