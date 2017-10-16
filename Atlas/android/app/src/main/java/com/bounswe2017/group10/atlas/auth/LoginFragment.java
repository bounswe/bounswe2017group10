package com.bounswe2017.group10.atlas.auth;

import android.app.ProgressDialog;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ProgressBar;
import android.widget.Toast;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.remote.APIUtils;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


public class LoginFragment extends Fragment {

    private static final String TAG = "LoginFragment";

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_login, container, false);

        // input fields
        EditText etUsernameEmail = view.findViewById(R.id.username_email_edittext);
        EditText etPassword = view.findViewById(R.id.login_pw_edittext);

        // set listeners
        Button btnForgotPassword = view.findViewById(R.id.forgot_pw_button);
        Button btnLoginRequest = view.findViewById(R.id.login_request_button);

        btnForgotPassword.setOnClickListener((View btnView) -> {
            // TODO: Implement forgot password functionality
        });
        btnLoginRequest.setOnClickListener((View btnView) -> {
            // collect input from text fields
            String usernameOrEmail = etUsernameEmail.getText().toString();
            String pw = etPassword.getText().toString();

            // validate inputs
            if (usernameOrEmail.length() == 0) {
                Toast.makeText(
                        getActivity().getApplicationContext(),
                        R.string.empty_username_email_field,
                        Toast.LENGTH_SHORT).show();
                return;
            }
            if (pw.length() == 0) {
                Toast.makeText(
                        getActivity().getApplicationContext(),
                        R.string.empty_password,
                        Toast.LENGTH_SHORT).show();
                return;
            }

            // construct request body
            LoginRequest loginRequest = new LoginRequest();
            loginRequest.setUsernameOrEmail(usernameOrEmail);
            loginRequest.setPassword(pw);

            // send async login request
            ProgressBar progress = view.findViewById(R.id.login_progress_bar);
            progress.setVisibility(View.VISIBLE);
            APIUtils.getAPI().login(loginRequest).enqueue(new OnLoginResponse(progress));
        });
        return view;
    }

    /**
     * Implement retrofit response callback interface to be used for login requests.
     */
    private class OnLoginResponse implements Callback<LoginResponse> {
        private ProgressBar progress;

        public OnLoginResponse(ProgressBar progress) {
            this.progress = progress;
        }

        @Override
        public void onResponse(Call<LoginResponse> call, Response<LoginResponse> response) {
            progress.setVisibility(View.GONE);
            if (response.isSuccessful()) {
                String token = response.body().getToken();
                // TODO: Do something with token
                Toast.makeText(getActivity().getApplicationContext(), "Successfully logged in", Toast.LENGTH_SHORT).show();
            } else {
                // TODO: Implement separate response code checks
                Toast.makeText(getActivity().getApplicationContext(), "Couldn't log in", Toast.LENGTH_SHORT).show();
            }
        }

        @Override
        public void onFailure(Call<LoginResponse> call, Throwable t) {
            progress.setVisibility(View.GONE);
            Log.d(TAG, "LOGIN connection failure: " + t.toString());
            Log.d(TAG, "LOGIN connection failure isExecuted: " + call.isExecuted());
            Log.d(TAG, "LOGIN connection failure isCanceled: " + call.isCanceled());
        }
    }
}
