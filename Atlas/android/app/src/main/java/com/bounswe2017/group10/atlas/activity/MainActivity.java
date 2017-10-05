package com.bounswe2017.group10.atlas.activity;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.Window;
import android.view.WindowManager;
import android.widget.LinearLayout;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.util.BlurBuilder;

public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
                             WindowManager.LayoutParams.FLAG_FULLSCREEN);
        setContentView(R.layout.activity_main);

        LinearLayout mContainerView = (LinearLayout)findViewById(R.id.login_page_layout);
        Bitmap origBitmap = BitmapFactory.decodeResource(getResources(), R.drawable.background);
        Bitmap blurredBitmap = (new BlurBuilder()).blur(this, origBitmap);
        mContainerView.setBackground(new BitmapDrawable(getResources(), blurredBitmap));
    }
}
