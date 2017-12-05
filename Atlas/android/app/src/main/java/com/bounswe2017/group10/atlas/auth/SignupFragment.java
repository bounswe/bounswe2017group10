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
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.response.OnSignupResponse;


import static com.bounswe2017.group10.atlas.util.Utils.showToast;


public class SignupFragment extends Fragment {

    private ProgressBar progress;
    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_signup, container, false);

        // input fields
        EditText sUsernameEmail = view.findViewById(R.id.username_edittext);
        EditText sEmail = view.findViewById(R.id.email_edittext);
        EditText sPassword = view.findViewById(R.id.password_pw_edittext);
        EditText sConfirmPassword = view.findViewById(R.id.confirm_pw_edittext);

        Button btnSignUpRequest = view.findViewById(R.id.signup_request_button);
        btnSignUpRequest.setOnClickListener((View btnView) -> {

            String usernameOrEmail = sUsernameEmail.getText().toString();
            String pw = sPassword.getText().toString();
            String email = sEmail.getText().toString();
            String confirmPw = sConfirmPassword.getText().toString();

            // validate inputs
            Context appContext = getActivity().getApplicationContext();
            if (usernameOrEmail.length() == 0) {
                showToast(appContext, getResources().getString(R.string.empty_username_email_field));
                return;
            } else if (pw.length() == 0) {
                showToast(appContext, getResources().getString(R.string.empty_password));
                return;
            } else if (email.length() == 0) {
                showToast(appContext, getResources().getString(R.string.empty_email));
                return;
            } else if (confirmPw.length() == 0) {
                showToast(appContext, getResources().getString(R.string.empty_password));
                return;
            } else if (!confirmPw.equals(pw)) {
                showToast(appContext, getResources().getString(R.string.different_confirm_password));
                return;
            }

            // construct json containing user info and send signup request
            SignupRequest signupRequest = new SignupRequest();
            signupRequest.setUsername(usernameOrEmail);
            signupRequest.setEmail(email);
            signupRequest.setPassword(pw);
            signupRequest.setConfirmPassword(confirmPw);
            progress = view.findViewById(R.id.signup_progress_bar);
            progress.setVisibility(View.VISIBLE);
            OnSignupResponse signupHandler = new OnSignupResponse(getActivity(), progress, signupRequest);
            APIUtils.serverAPI().signup(signupRequest).enqueue(signupHandler);
        });
        return view;
    }
}
