package com.bounswe2017.group10.atlas.auth;


import android.app.Activity;
import android.app.DatePickerDialog;
import android.app.Dialog;
import android.content.Intent;
import android.os.Bundle;
import android.provider.SyncStateContract;
import android.support.annotation.Nullable;
import android.support.v4.app.DialogFragment;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.DatePicker;
import android.widget.EditText;
import android.widget.ProgressBar;
import android.widget.Toast;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.home.HomeActivity;
import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupResponse;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.util.DateUtils;

import org.json.JSONObject;

import java.util.Calendar;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;


public class SignupFragment extends Fragment {

    private static final int BIRTHDATE_PICKER_CODE = 0;

    private Button btnBirthDate;
    private Button btnSignUpRequest;
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


        //btnBirthDate = view.findViewById(R.id.birthdate_button);
        btnSignUpRequest = view.findViewById(R.id.signup_request_button);
        /*
        btnBirthDate.setOnClickListener((View btnView) -> {
            DialogFragment dateDialog = new DatePickerFragment();
            // TODO: send the date written in the button to date picker dialog
            // TODO: if no date is set yet, send the current date
            dateDialog.setTargetFragment(this, BIRTHDATE_PICKER_CODE);
            dateDialog.show(getFragmentManager(), "datePicker");
        });
        */
        btnSignUpRequest.setOnClickListener((View btnView) -> {

            String usernameOrEmail = sUsernameEmail.getText().toString();
            String pw = sPassword.getText().toString();
            String email = sEmail.getText().toString();
            String confirmPw = sConfirmPassword.getText().toString();
            String firstname = sFirstName.getText().toString();
            String lastname = sLastName.getText().toString();

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
            if (firstname.length() == 0) {
                Toast.makeText(
                        getActivity().getApplicationContext(),
                        R.string.empty_firstname,
                        Toast.LENGTH_SHORT).show();
                return;
            }
            if (lastname.length() == 0) {
                Toast.makeText(
                        getActivity().getApplicationContext(),
                        R.string.empty_lastname,
                        Toast.LENGTH_SHORT).show();
                return;
            }
            if (email.length() == 0) {
                Toast.makeText(
                        getActivity().getApplicationContext(),
                        R.string.empty_email,
                        Toast.LENGTH_SHORT).show();
                return;
            }
            if (confirmPw.length() == 0) {
                Toast.makeText(
                        getActivity().getApplicationContext(),
                        R.string.empty_password,
                        Toast.LENGTH_SHORT).show();
                return;
            }
            if (!confirmPw.equals(pw)) {
                Toast.makeText(
                        getActivity().getApplicationContext(),
                        R.string.different_confirm_password,
                        Toast.LENGTH_SHORT).show();
                return;
            }
            // collect input from text fields
            // validate inputs
            // construct json containing user info
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

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);

        if (requestCode == BIRTHDATE_PICKER_CODE) {
            if (resultCode == Activity.RESULT_OK) {
                Bundle bundle = data.getExtras();
                if (bundle.containsKey("date")) {
                    String date = bundle.getString("date");
                    btnBirthDate.setText(date);
                }
            } else {
                // TODO: Handle Result not OK
            }
        }
    }
    /*
    public static class DatePickerFragment extends DialogFragment
                                implements DatePickerDialog.OnDateSetListener {
        @Override
        public Dialog onCreateDialog(Bundle savedInstanceState) {
            // Use the current date as the default date in the picker
            // TODO: Set the date sent in the bundle
            final Calendar c = Calendar.getInstance();
            int year = c.get(Calendar.YEAR);
            int month = c.get(Calendar.MONTH);
            int day = c.get(Calendar.DAY_OF_MONTH);

            // Create a new instance of DatePickerDialog and return it
            return new DatePickerDialog(getActivity(), this, year, month, day);
        }

        public void onDateSet(DatePicker view, int year, int month, int day) {
            // display the picked date as button text
            Bundle bundle = new Bundle();
            bundle.putString("date", DateUtils.toStandardDate(year, month, day));
            Intent intent = new Intent().putExtras(bundle);
            getTargetFragment().onActivityResult(getTargetRequestCode(), Activity.RESULT_OK, intent);
        }
    }
    */

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
                progress.setVisibility(View.INVISIBLE);
                LoginRequest loginRequest = new LoginRequest();
                loginRequest.setUsernameOrEmail(origRequest.getUsername());
                loginRequest.setPassword(origRequest.getPassword());
                APIUtils.getAPI().login(loginRequest).enqueue(new OnLoginResponse());
            } else {
                try {
                    JSONObject jObjError = new JSONObject(response.errorBody().string());
                    if(jObjError.has("username"))
                        Toast.makeText(getContext(), jObjError.getString("username"), Toast.LENGTH_LONG).show();
                    else if(jObjError.has("email"))
                        Toast.makeText(getContext(), jObjError.getString("email"), Toast.LENGTH_LONG).show();
                    else if(jObjError.has("non_field_errors"))
                        Toast.makeText(getContext(), jObjError.getString("non_field_errors"), Toast.LENGTH_LONG).show();
                    else if(jObjError.has("firstname"))
                        Toast.makeText(getContext(), jObjError.getString("firstname"), Toast.LENGTH_LONG).show();
                    else if(jObjError.has("lastname"))
                        Toast.makeText(getContext(), jObjError.getString("lastname"), Toast.LENGTH_LONG).show();
                    else
                        Toast.makeText(getContext(),"couldn't signup", Toast.LENGTH_LONG).show();
                } catch (Exception e) {
                    Toast.makeText(getContext(), e.getMessage(), Toast.LENGTH_LONG).show();
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
                Toast.makeText(getActivity().getApplicationContext(), "Successfully logged in.", Toast.LENGTH_LONG).show();
                startHomeActivity(response.body().getToken());
            } else {
                Toast.makeText(getActivity().getApplicationContext(), "Couldn't log in", Toast.LENGTH_LONG).show();
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
