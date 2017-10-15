package com.bounswe2017.group10.atlas.auth;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.remote.ResponseCallback;


public class LoginFragment extends Fragment {

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
            String usernameEmail = etUsernameEmail.getText().toString();
            String pw = etPassword.getText().toString();

            // validate inputs
            if (usernameEmail.length() == 0) {
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

            // construct reuqest body
            LoginRequest loginRequest = new LoginRequest();
            if (usernameEmail.contains("@")) {
                loginRequest.setEmail(usernameEmail);
            } else {
                loginRequest.setUsername(usernameEmail);
            }
            loginRequest.setPassword(pw);

            // send async login request
            AuthManager.login(loginRequest, new OnLoginSuccess(), new OnLoginFailure());
        });
        return view;
    }

    /**
     * Implement the tasks to perform upon successful log in request.
     */
    class OnLoginSuccess implements ResponseCallback<LoginResponse> {
        public void onResponse(LoginResponse response) {
            // handle login success view updates
            Toast.makeText(getActivity().getApplicationContext(), "Successfully logged in", Toast.LENGTH_LONG).show();
        }
    }

    /**
     * Implement the tasks to perform upon failed log in request.
     */
    class OnLoginFailure implements  ResponseCallback<LoginResponse> {
        public void onResponse(LoginResponse response) {
            // handle login failure view updates
            Toast.makeText(getActivity().getApplicationContext(), "Couldn't log in", Toast.LENGTH_LONG).show();
        }
    }
}
