package com.bounswe2017.group10.atlas.auth;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentManager;
import android.os.Bundle;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.widget.FrameLayout;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.util.BlurBuilder;

public class AuthActivity extends FragmentActivity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
                             WindowManager.LayoutParams.FLAG_FULLSCREEN);
        setContentView(R.layout.activity_auth);

        if (savedInstanceState != null) {
            return;
        }
        FrameLayout viewGroup = (FrameLayout) findViewById(R.id.auth_container);

        // blur viewPager background
        Bitmap origBitmap = BitmapFactory.decodeResource(getResources(), R.drawable.background);
        Bitmap blurredBitmap = (new BlurBuilder()).blur(this, origBitmap);
        viewGroup.setBackground(new BitmapDrawable(getResources(), blurredBitmap));

        Fragment authMenuFragment = new AuthMenuFragment();
        getSupportFragmentManager().beginTransaction()
                .add(R.id.auth_container, authMenuFragment)
                .commit();
        //setupViewPager(mViewPager);

    }

    @Override
    public void onBackPressed() {
        FragmentManager fm = getSupportFragmentManager();
        if (fm.getBackStackEntryCount() > 0) {
            fm.popBackStack();
        } else {
            super.onBackPressed();
        }
    }
}
