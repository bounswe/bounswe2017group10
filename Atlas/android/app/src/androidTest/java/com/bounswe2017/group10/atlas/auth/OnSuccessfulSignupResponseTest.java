package com.bounswe2017.group10.atlas.auth;

import android.app.Activity;
import android.content.Intent;
import android.support.test.rule.ActivityTestRule;
import android.support.test.runner.AndroidJUnit4;
import android.support.v4.app.Fragment;
import android.widget.ProgressBar;


import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.httpbody.LoginRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupRequest;
import com.bounswe2017.group10.atlas.httpbody.SignupResponse;
import com.bounswe2017.group10.atlas.remote.API;
import com.bounswe2017.group10.atlas.remote.APIUtils;
import com.bounswe2017.group10.atlas.remote.RetrofitBuilder;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;


import retrofit2.Call;
import retrofit2.Response;
import retrofit2.converter.gson.GsonConverterFactory;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;


@RunWith(AndroidJUnit4.class)
public class OnSuccessfulSignupResponseTest {

    @Rule
    public ActivityTestRule<AuthActivity> mActivityRule = new ActivityTestRule<>(AuthActivity.class, true, false);

    private OnSignupResponse onResponse;
    private Call<SignupResponse> call;
    private Response<SignupResponse> response;
    private SignupRequest signupRequest;

    @Before
    public void init() throws Exception {
        signupRequest = new SignupRequest();
        signupRequest.setUsername("tomhanks");
        signupRequest.setFirstname("Tom");
        signupRequest.setLastname("Hanks");
        signupRequest.setEmail("tom_hanks@abc.com");
        signupRequest.setPassword("Abcd1234");
        signupRequest.setConfirmPassword("Abcd1234");

        SignupResponse signupResp = new SignupResponse();
        signupResp.setFirstname(signupRequest.getFirstname());
        signupResp.setLastname(signupRequest.getLastname());
        signupResp.setEmail(signupRequest.getEmail());
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
        onResponse = new OnSignupResponse(authActivity, progress, signupRequest);

        // create a custom API object to spy on.
        RetrofitBuilder builder = new RetrofitBuilder()
                .baseUrl("http://0.1.2.3:12345")
                .addConverterFactory(GsonConverterFactory.create());
        API api = builder.build().create(API.class);
        APIDelegate delegate = new APIDelegate(api);
        APIDelegate spy = Mockito.spy(delegate);
        APIUtils.setServerAPI(spy);

        onResponse.onResponse(call, response);

        // check if a login request with correct credentials has been made
        LoginRequest request = new LoginRequest();
        request.setUsernameOrEmail(signupRequest.getUsername());
        request.setPassword(signupRequest.getPassword());
        Mockito.verify(spy, times(1)).login(request);
    }

}
