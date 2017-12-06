package com.bounswe2017.group10.atlas.profile;

import android.content.Intent;
import android.content.SharedPreferences;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.View;
import android.widget.ImageButton;
import android.widget.TextView;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;
import com.bumptech.glide.Glide;
import com.bumptech.glide.request.RequestOptions;

import static com.bounswe2017.group10.atlas.util.Utils.logout;

public class ProfileActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_profile);

        SharedPreferences pref = Utils.getSharedPref(this);
        String firstName = pref.getString(Constants.FIRSTNAME, "");
        String lastName = pref.getString(Constants.LASTNAME, "");
        String email = pref.getString(Constants.EMAIL, "");

        String image = pref.getString(Constants.PROFILE_PICTURE,"");

        if(image!="") {
            Glide.with(this)
                    .load(image)
                    .apply(new RequestOptions()
                            .placeholder(R.drawable.ic_person_outline_black_48dp)
                            .error(R.drawable.ic_person_outline_black_48dp)
                            .fallback(R.drawable.ic_person_outline_black_48dp))
                    .into((ImageButton) findViewById(R.id.user_profile_photo));
        }

        String nameText = getString(R.string.fullname, firstName, lastName);
        ((TextView) findViewById(R.id.user_profile_name)).setText(nameText);
        ((TextView) findViewById(R.id.user_profile_email)).setText(email);

        TextView logouttext = findViewById(R.id.plogout);
        logouttext.setOnClickListener((View btnview)-> {
            logout(getApplicationContext());
        });

        TextView myItem = findViewById(R.id.pmyitem);
        myItem.setOnClickListener((View btnview)->{
            Intent intent = new Intent(this, OwnItemActivity.class);
            Bundle b = new Bundle();
            b.putInt("type",1);
            intent.putExtras(b);
            startActivity(intent);
        } );

        TextView myFavItems = findViewById(R.id.pmyfavitems);
        myFavItems.setOnClickListener((View btnview)->{
            Intent intent = new Intent(this, OwnItemActivity.class);
            Bundle b = new Bundle();
            b.putInt("type",2);
            intent.putExtras(b);
            startActivity(intent);
        } );

        TextView nearbyItems = findViewById(R.id.pnearbyitems);
        nearbyItems.setOnClickListener((View btnview)->{
            Intent intent = new Intent(this, OwnItemActivity.class);
            Bundle b = new Bundle();
            b.putInt("type",3);
            intent.putExtras(b);
            startActivity(intent);
        } );
    }
}
