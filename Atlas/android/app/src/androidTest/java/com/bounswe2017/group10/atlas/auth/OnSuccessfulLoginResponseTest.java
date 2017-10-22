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
import com.bounswe2017.group10.atlas.response.OnLoginResponse;
import com.bounswe2017.group10.atlas.util.Constants;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.runner.RunWith;


import retrofit2.Call;
import retrofit2.Response;

import static org.mockito.Mockito.mock;


@RunWith(AndroidJUnit4.class)
public class OnSuccessfulLoginResponseTest {

    private static final String token = "ANSTHIASNOTHAOSNUTHAOSUh";

    @Rule
    public ActivityTestRule<AuthActivity> mActivityRule = new ActivityTestRule<>(AuthActivity.class, true, false);

    private OnLoginResponse onResponse;
    private Call<LoginResponse> call;
    private Response<LoginResponse> response;

    @Before
    public void init() throws Exception {
        LoginResponse loginResp = new LoginResponse();
        loginResp.setToken(token);
        call = mock(Call.class);
        response = retrofit2.Response.success(loginResp);
    }

    @Test
    public void testLoginResponse() {
        Fragment fragment = new LoginFragment();
        Intent intent = new Intent();
        mActivityRule.launchActivity(intent);
        mActivityRule.getActivity().getSupportFragmentManager().beginTransaction().replace(R.id.auth_container, fragment).commit();

        Activity authActivity = mActivityRule.getActivity();
        ProgressBar progress = new ProgressBar(authActivity);
        onResponse = new OnLoginResponse(authActivity, progress);

        Instrumentation.ActivityMonitor monitor = InstrumentationRegistry.getInstrumentation().addMonitor(HomeActivity.class.getName(), null, false);
        onResponse.onResponse(call, response);
        Activity nextActivity = InstrumentationRegistry.getInstrumentation().waitForMonitor(monitor);

        // check if current activity is home activity
        assertEquals(HomeActivity.class.getName(), nextActivity.getClass().getName());

        // check if token is sent correctly via intent
        assertEquals(token, nextActivity.getIntent().getStringExtra(Constants.AUTH_STR));
    }

}
