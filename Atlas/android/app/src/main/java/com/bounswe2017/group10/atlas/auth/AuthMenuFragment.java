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
    private Fragment mLoginFragment = null;
    private Fragment mSignupFragment = null;

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_auth_menu, container, false);

        // button declarations
        Button btnLogin = view.findViewById(R.id.login_button);
        Button btnSignup = view.findViewById(R.id.signup_button);
        Button btnLearnMore = view.findViewById(R.id.learn_more_button);

        // set listeners
        btnLogin.setOnClickListener((View btnView) -> {
            if (mLoginFragment == null) {
                mLoginFragment = new LoginFragment();
            }
            getActivity().getSupportFragmentManager()
                    .beginTransaction()
                    .replace(R.id.auth_container, mLoginFragment)
                    .addToBackStack(null)
                    .commit();
        });
        btnSignup.setOnClickListener((View btnView) -> {
            if (mSignupFragment == null) {
                mSignupFragment = new SignupFragment();
            }
            getActivity().getSupportFragmentManager()
                    .beginTransaction()
                    .replace(R.id.auth_container, mSignupFragment)
                    .addToBackStack(null)
                    .commit();
        });
        btnLearnMore.setOnClickListener((View btnView) -> {
            // TODO: display learn more info with a dialog or another fragment.
        });
        return view;
    }
}
