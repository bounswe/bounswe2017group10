package com.bounswe2017.group10.atlas.auth;


import android.content.Context;
import android.content.Intent;
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
import com.bounswe2017.group10.atlas.home.HomeActivity;
import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupResponse;
import com.bounswe2017.group10.atlas.remote.APIUtils;

import org.json.JSONObject;


import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

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
        EditText sFirstName = view.findViewById(R.id.firstname_edittext);
        EditText sLastName = view.findViewById(R.id.lastname_edittext);


        Button btnSignUpRequest = view.findViewById(R.id.signup_request_button);
        btnSignUpRequest.setOnClickListener((View btnView) -> {

            String usernameOrEmail = sUsernameEmail.getText().toString();
            String pw = sPassword.getText().toString();
            String email = sEmail.getText().toString();
            String confirmPw = sConfirmPassword.getText().toString();
            String firstname = sFirstName.getText().toString();
            String lastname = sLastName.getText().toString();

            // validate inputs
            Context appContext = getActivity().getApplicationContext();
            if (usernameOrEmail.length() == 0) {
                showToast(appContext, getResources().getString(R.string.empty_username_email_field));
                return;
            } else if (pw.length() == 0) {
                showToast(appContext, getResources().getString(R.string.empty_password));
                return;
            } else if (firstname.length() == 0) {
                showToast(appContext, getResources().getString(R.string.empty_firstname));
                return;
            } else if (lastname.length() == 0) {
                showToast(appContext, getResources().getString(R.string.empty_lastname));
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
            signupRequest.setFirstname(firstname);
            signupRequest.setLastname(lastname);
            progress = view.findViewById(R.id.signup_progress_bar);
            progress.setVisibility(View.VISIBLE);
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
                progress.setVisibility(View.INVISIBLE);
                LoginRequest loginRequest = new LoginRequest();
                loginRequest.setUsernameOrEmail(origRequest.getUsername());
                loginRequest.setPassword(origRequest.getPassword());
                APIUtils.getAPI().login(loginRequest).enqueue(new OnLoginResponse());
            } else {
                try {
                    JSONObject jObjError = new JSONObject(response.errorBody().string());
                    if(jObjError.has("username"))
                        showToast(getContext(), jObjError.getString("username"));
                    else if(jObjError.has("email"))
                        showToast(getContext(), jObjError.getString("email"));
                    else if(jObjError.has("non_field_errors"))
                        showToast(getContext(), jObjError.getString("non_field_errors"));
                    else if(jObjError.has("firstname"))
                        showToast(getContext(), jObjError.getString("firstname"));
                    else if(jObjError.has("lastname"))
                        showToast(getContext(), jObjError.getString("lastname"));
                    else
                        showToast(getContext(),"couldn't signup");
                } catch (Exception e) {
                    showToast(getContext(), e.getMessage());
                }
                progress.setVisibility(View.INVISIBLE);
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
                showToast(getActivity().getApplicationContext(), "Successfully logged in.");
                startHomeActivity(response.body().getToken());
            } else {
                showToast(getActivity().getApplicationContext(), "Couldn't log in");
            }
        }

        @Override
        public void onFailure(Call<LoginResponse> call, Throwable t) {

        }
    }

    private void startHomeActivity(String token) {
        Intent intent = new Intent(getActivity(), HomeActivity.class).putExtra("token", token);
        startActivity(intent);
    }
}
