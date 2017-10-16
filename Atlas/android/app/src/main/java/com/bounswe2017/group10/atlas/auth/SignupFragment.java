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
import com.bounswe2017.group10.atlas.remote.APIUtils;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


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
            APIUtils.getAPI().signup(signupRequest).enqueue(new OnSignupResponse(signupRequest));
        });
        return view;
    }

    /**
     * Implement retrofit response callback interface to be used for signup requests.
     */
    private class OnSignupResponse implements Callback<SignupResponse> {
        private SignupRequest origRequest;

        OnSignupResponse(SignupRequest origRequest) {
            this.origRequest = origRequest;
        }

        @Override
        public void onResponse(Call<SignupResponse> call, Response<SignupResponse> response) {
            if (response.isSuccessful()) {
                Toast.makeText(getActivity().getApplicationContext(), "Successfully signed up", Toast.LENGTH_LONG).show();
                LoginRequest loginRequest = new LoginRequest();
                loginRequest.setUsername(origRequest.getUsername());
                loginRequest.setPassword(origRequest.getPassword());
                APIUtils.getAPI().login(loginRequest).enqueue(new OnLoginResponse());
            } else {
                Toast.makeText(getActivity().getApplicationContext(), "Couldn't sign up", Toast.LENGTH_LONG).show();
            }
        }

        @Override
        public void onFailure(Call<SignupResponse> call, Throwable t) {
        }
    }

    /**
     * Implement retrofit response callback interface to be used for login requests
     * made after signup requests.
     */
    private class OnLoginResponse implements Callback<LoginResponse> {
        @Override
        public void onResponse(Call<LoginResponse> call, Response<LoginResponse> response) {
            if (response.isSuccessful()) {
                Toast.makeText(getActivity().getApplicationContext(), "Successfully logged in", Toast.LENGTH_LONG).show();
            } else {
                Toast.makeText(getActivity().getApplicationContext(), "Couldn't log in", Toast.LENGTH_LONG).show();
            }
        }

        @Override
        public void onFailure(Call<LoginResponse> call, Throwable t) {

        }
    }
}
