package com.bounswe2017.group10.atlas.auth;


import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.Toast;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupResponse;
import com.bounswe2017.group10.atlas.remote.ResponseCallback;


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
            SignupRequest signupRequest = new SignupRequest();
            AuthManager.signup(signupRequest, new OnSignupSuccess(signupRequest), new OnSignupFailure());
        });
        return view;
    }

    /**
     * Implement the tasks to perform upon successful signup request.
     */
    class OnSignupSuccess implements ResponseCallback<SignupResponse> {
        private SignupRequest signupRequest;

        public OnSignupSuccess(SignupRequest request) {
            this.signupRequest = request;
        }

        public void onResponse(SignupResponse response) {
            Toast.makeText(getActivity().getApplicationContext(), "Successfully signed up", Toast.LENGTH_LONG).show();
            LoginRequest loginRequest = new LoginRequest();
            loginRequest.setUsername(signupRequest.getUsername());
            loginRequest.setPassword(signupRequest.getPassword());
            AuthManager.login(loginRequest, new OnImmediateLoginSuccess(), new OnImmediateLoginFailure());
        }
    }

    /**
     * Implement the tasks to perform upon failed signup request.
     */
    class OnSignupFailure implements ResponseCallback<SignupResponse> {
        public void onResponse(SignupResponse response) {
            Toast.makeText(getActivity().getApplicationContext(), "Couldn't sign up", Toast.LENGTH_LONG).show();
        }
    }

    /**
     * Implement the tasks to perform upon successful log in request after a successful signup request.
     */
    class OnImmediateLoginSuccess implements ResponseCallback<LoginResponse> {
        public void onResponse(LoginResponse response) {
            // handle login success view updates
            Toast.makeText(getActivity().getApplicationContext(), "Successfully logged in", Toast.LENGTH_LONG).show();
        }
    }

    /**
     * Implement the tasks to perform upon failed log in request after a successful signup request.
     *
     * This should happen very rarely, if at all.
     */
    class OnImmediateLoginFailure implements  ResponseCallback<LoginResponse> {
        public void onResponse(LoginResponse response) {
            // handle login failure view updates
            Toast.makeText(getActivity().getApplicationContext(), "Couldn't log in", Toast.LENGTH_LONG).show();
        }
    }
}
