package com.bounswe2017.group10.atlas.profile;

import android.app.Activity;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.UpdateProfileRequest;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

import static com.bounswe2017.group10.atlas.util.Utils.logout;

public class EditProfileActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_edit_profile);

        SharedPreferences pref = Utils.getSharedPref(this);
        String firstName = pref.getString(Constants.FIRSTNAME, "");
        String lastName = pref.getString(Constants.LASTNAME, "");
        String email = pref.getString(Constants.EMAIL, "");
        String image = pref.getString(Constants.PROFILE_PICTURE,"");
        String authStr = pref.getString(Constants.AUTH_STR,"");
        String username = pref.getString(Constants.USERNAME,"");

        Activity mActivity = this;

        EditText usernameEditText = findViewById(R.id.edit_profile_username_edittext);
        usernameEditText.setText(username);
        usernameEditText.setFocusable(false);
        usernameEditText.setEnabled(false);

        EditText emailEditText = findViewById(R.id.edit_profile_email_edittext);
        emailEditText.setText(email);

        EditText fNameEditText = findViewById(R.id.edit_profile_firstname_edittext);
        fNameEditText.setText(firstName);

        EditText lNameEditText = findViewById(R.id.edit_profile_lastname_edittext);
        lNameEditText.setText(lastName);

        EditText ppEditText = findViewById(R.id.edit_profile_pp_edittext);
        ppEditText.setText(image);

        EditText oldPEditText = findViewById(R.id.edit_profile_oldp_edittext);
        EditText newPEditText = findViewById(R.id.edit_profile_newp_edittext);
        EditText conPEditText = findViewById(R.id.edit_profile_confp_edittext);

        Button updateButton = findViewById(R.id.edit_profile_button);
        updateButton.setOnClickListener((View btnView)->{
            UpdateProfileRequest updateBody = new UpdateProfileRequest();
            if(!emailEditText.getText().toString().equals(email))
                updateBody.setEmail(emailEditText.getText().toString());
            if(!fNameEditText.getText().toString().equals(firstName))
                updateBody.setFirstName(fNameEditText.getText().toString());
            if(!lNameEditText.getText().toString().equals(lastName))
                updateBody.setLastName(lNameEditText.getText().toString());
            if(!ppEditText.getText().toString().equals(image))
                updateBody.setProfilePicture(ppEditText.getText().toString());
            if(!oldPEditText.getText().toString().equals(""))
                updateBody.setOldPassword(oldPEditText.getText().toString());
            if(!newPEditText.getText().toString().equals(""))
                updateBody.setPassword(newPEditText.getText().toString());
            if(!conPEditText.getText().toString().equals(""))
                updateBody.setConfirmPassword(conPEditText.getText().toString());

            APIUtils.serverAPI().updateProfile(authStr,username, updateBody).enqueue(new Callback<Void>() {
                @Override
                public void onResponse(Call<Void> call, Response<Void> response) {
                    if (response.isSuccessful()) {
                        Utils.showToast(mActivity, getString(R.string.edit_successful));

                        SharedPreferences.Editor editor = Utils.getSharedPrefEditor(getApplicationContext());
                        if(updateBody.getFirstName() != null)
                            editor.putString(Constants.FIRSTNAME, updateBody.getFirstName()).apply();
                        if(updateBody.getLastName() != null)
                            editor.putString(Constants.LASTNAME, updateBody.getLastName()).apply();
                        if(updateBody.getEmail() != null)
                            editor.putString(Constants.EMAIL, updateBody.getEmail()).apply();
                        if(updateBody.getProfilePicture() != null)
                            editor.putString(Constants.PROFILE_PICTURE,updateBody.getProfilePicture()).apply();
                        if(updateBody.getPassword() != null)
                            logout(getApplicationContext());
                        else {
                            Intent intent = new Intent(mActivity, ProfileActivity.class);
                            startActivity(intent);
                        }

                    } else {
                        Utils.showToast(mActivity, getString(R.string.error_occurred) +
                                " " + response.message());
                    }


                }

                @Override
                public void onFailure(Call<Void> call, Throwable t) {
                    Utils.showToast(mActivity, getString(R.string.connection_failure));
                }
            });


        });


    }
}
