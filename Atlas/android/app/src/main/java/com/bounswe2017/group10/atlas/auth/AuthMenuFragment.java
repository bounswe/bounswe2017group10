package com.bounswe2017.group10.atlas.auth;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;

import com.bounswe2017.group10.atlas.R;


public class AuthMenuFragment extends Fragment {
    private Button btnLogin;
    private Button btnSignup;
    private Button btnLearnMore;

    private Fragment mLoginFragment = null;
    private Fragment mSignupFragment = null;

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_auth_menu, container, false);

        // button declarations
        btnLogin = (Button) view.findViewById(R.id.login_button);
        btnSignup = (Button) view.findViewById(R.id.signup_button);
        btnLearnMore = (Button) view.findViewById(R.id.learn_more_button);

        // set listeners
        btnLogin.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (mLoginFragment == null) {
                    mLoginFragment = new LoginFragment();
                }
                ((AuthActivity)getActivity()).getSupportFragmentManager()
                        .beginTransaction()
                        .replace(R.id.auth_container, mLoginFragment)
                        .addToBackStack(null)
                        .commit();
            }
        });
        btnSignup.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (mSignupFragment == null) {
                    mSignupFragment = new SignupFragment();
                }
                ((AuthActivity)getActivity()).getSupportFragmentManager()
                        .beginTransaction()
                        .replace(R.id.auth_container, mSignupFragment)
                        .addToBackStack(null)
                        .commit();
            }
        });
        btnLearnMore.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                // TODO: display learn more info with a dialog or another fragment.
            }
        });
        return view;
    }
}
