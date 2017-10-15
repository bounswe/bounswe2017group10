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

    public static final String TAG = "AuthMenuFragment";

    private Button btnLogin;
    private Button btnSignup;
    private Button btnLearnMore;

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_auth_menu, container, false);

        // button declarations

        // set listeners

        return view;
    }
}
