package com.bounswe2017.group10.atlas.auth;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.support.v4.view.ViewPager;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.Window;
import android.view.WindowManager;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.util.BlurBuilder;

public class AuthActivity extends AppCompatActivity {

    public static final String TAG = "AuthActivity";

    private AuthPagerAdapter mAuthPagerAdapter;
    private ViewPager mViewPager;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
                             WindowManager.LayoutParams.FLAG_FULLSCREEN);
        setContentView(R.layout.activity_auth);

        mViewPager = (ViewPager) findViewById(R.id.auth_container);

        // blur viewPager background
        Bitmap origBitmap = BitmapFactory.decodeResource(getResources(), R.drawable.background);
        Bitmap blurredBitmap = (new BlurBuilder()).blur(this, origBitmap);
        mViewPager.setBackground(new BitmapDrawable(getResources(), blurredBitmap));

        setupViewPager(mViewPager);
    }

    /**
     * Set up the ViewPager object with all the Fragments that will be shown in this activity.
     *
     * @param viewPager ViewPager of the current activity.
     */
    private void setupViewPager(ViewPager viewPager) {
        mAuthPagerAdapter = new AuthPagerAdapter(getSupportFragmentManager());

        // inflate the 1st fragment by default
        mAuthPagerAdapter.addFragment(new AuthMenuFragment(), AuthMenuFragment.TAG);
        mAuthPagerAdapter.addFragment(new LoginFragment(), LoginFragment.TAG);
        mAuthPagerAdapter.addFragment(new SignupFragment(), SignupFragment.TAG);
        viewPager.setAdapter(mAuthPagerAdapter);
    }

    /**
     * Set the current viewed fragment to the Fragment with the given title.
     *
     * @param fragmentTitle Title of the Fragment. This is generally set to Class.TAG attribute.
     */
    public void setFragment(String fragmentTitle) {
        int index = mAuthPagerAdapter.getIndex(fragmentTitle);
        mViewPager.setCurrentItem(index);
    }
}
