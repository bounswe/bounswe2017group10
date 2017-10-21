package com.bounswe2017.group10.atlas.auth;

import android.app.Activity;
import android.app.Instrumentation;
import android.content.Intent;
import android.support.test.InstrumentationRegistry;
import android.support.test.rule.ActivityTestRule;
import android.support.test.runner.AndroidJUnit4;
import android.support.v4.app.Fragment;
import android.widget.ProgressBar;


import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.home.HomeActivity;
import com.bounswe2017.group10.atlas.httpbody.LoginResponse;
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupResponse;
import com.bounswe2017.group10.atlas.remote.APIManager;
import com.bounswe2017.group10.atlas.remote.APIUtils;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.runner.RunWith;
import org.mockito.Mockito;


import java.lang.reflect.Field;

import retrofit2.Call;
import retrofit2.Response;

import static org.mockito.Mockito.mock;


@RunWith(AndroidJUnit4.class)
public class OnSuccessfulSignupResponseTest {

    @Rule
    public ActivityTestRule<AuthActivity> mActivityRule = new ActivityTestRule<>(AuthActivity.class, true, false);

    private OnSignupResponse onResponse;
    private Call<SignupResponse> call;
    private Response<SignupResponse> response;
    private SignupRequest request;

    @Before
    public void init() throws Exception {
        request = new SignupRequest();
        request.setUsername("tomhanks");
        request.setFirstname("Tom");
        request.setLastname("Hanks");
        request.setEmail("tom_hanks@abc.com");
        request.setPassword("Abcd1234");
        request.setConfirmPassword("Abcd1234");

        SignupResponse signupResp = new SignupResponse();
        signupResp.setFirstname(request.getFirstname());
        signupResp.setLastname(request.getLastname());
        signupResp.setEmail(request.getEmail());
        signupResp.setDateCreated("2017-10-21T18:10:11.147134Z");
        signupResp.setDateModified("2017-10-21T18:10:11.147134Z");

        call = mock(Call.class);
        response = retrofit2.Response.success(signupResp);
    }

    @Test
    public void testSignupResponse() {
        Fragment fragment = new SignupFragment();
        Intent intent = new Intent();
        mActivityRule.launchActivity(intent);
        mActivityRule.getActivity().getSupportFragmentManager().beginTransaction().replace(R.id.auth_container, fragment).commit();

        Activity authActivity = mActivityRule.getActivity();
        ProgressBar progress = new ProgressBar(authActivity);
        onResponse = new OnSignupResponse(authActivity, progress, request);
        onResponse.onResponse(call, response);

        // TODO : check if a login request with correct credentials has been made
    }

}
