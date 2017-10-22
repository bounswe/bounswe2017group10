package com.bounswe2017.group10.atlas.auth;


import android.content.Context;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ProgressBar;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.response.OnLoginResponse;


import static com.bounswe2017.group10.atlas.util.Utils.showToast;


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
            Context appContext = getActivity().getApplicationContext();
            if (usernameOrEmail.length() == 0) {
                showToast(appContext, getResources().getString(R.string.empty_username_email_field));
                return;
            } else if (pw.length() == 0) {
                showToast(appContext, getResources().getString(R.string.empty_password));
                return;
            }

            // construct request body
            LoginRequest loginRequest = new LoginRequest();
            loginRequest.setUsernameOrEmail(usernameOrEmail);
            loginRequest.setPassword(pw);

            // send async login request
            ProgressBar progress = view.findViewById(R.id.login_progress_bar);
            progress.setVisibility(View.VISIBLE);
            OnLoginResponse loginHandler = new OnLoginResponse(getActivity(), progress);
            APIUtils.serverAPI().login(loginRequest).enqueue(loginHandler);
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
                startHomeActivity(token);
            } else if (response.code() == 400) {
                Toast.makeText(getActivity().getApplicationContext(), R.string.wrong_credentials, Toast.LENGTH_SHORT).show();
            }
        }

        @Override
        public void onFailure(Call<LoginResponse> call, Throwable t) {
            progress.setVisibility(View.GONE);
            Toast.makeText(getActivity().getApplicationContext(), R.string.connection_failure, Toast.LENGTH_SHORT).show();
            Log.d(TAG, "LOGIN connection failure: " + t.toString());
            Log.d(TAG, "LOGIN connection failure isExecuted: " + call.isExecuted());
            Log.d(TAG, "LOGIN connection failure isCanceled: " + call.isCanceled());
        }
    }

    /**
     * Start home activity with the given token.
     *
     * @param token Access token obtained from the server.
     */
    private void startHomeActivity(String token) {
        Intent intent = new Intent(getActivity(), HomeActivity.class).putExtra("token", token);
        intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK|Intent.FLAG_ACTIVITY_NEW_TASK);
        startActivity(intent);
    }
}
