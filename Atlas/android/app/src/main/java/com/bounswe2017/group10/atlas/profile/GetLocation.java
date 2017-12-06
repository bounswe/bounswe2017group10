package com.bounswe2017.group10.atlas.profile;

import android.Manifest;
import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.location.Location;
import android.net.Uri;
import android.os.Bundle;
import android.provider.Settings;
import android.support.annotation.NonNull;
import android.support.design.BuildConfig;
import android.support.design.widget.Snackbar;
import android.support.v4.app.ActivityCompat;
import android.support.v7.app.ActionBar;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.SearchView;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.View;

import com.bounswe2017.group10.atlas.home.ListItemsFragment;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.Task;
import com.bounswe2017.group10.atlas.R;

import java.util.Locale;

/**
 * Location sample.
 * <p>
 * Demonstrates use of the Location API to retrieve the last known location for a device.
 */
public class GetLocation extends AppCompatActivity {

    private static final String TAG = GetLocation.class.getSimpleName();

    private static final int REQUEST_PERMISSIONS_REQUEST_CODE = 34;

    /**
     * Provides the entry point to the Fused Location Provider API.
     */
    private FusedLocationProviderClient mFusedLocationClient;

    /**
     * Represents a geographical location.
     */
    protected Location mLastLocation;

    private NearbyItemsFragment mNearbyItemsFragment;
    private ListItemsFragment mSearchItemsFragment;
    private ActionBar mActionBar;
    private SearchView mSearchView;
    private double latitute;
    private double longitute;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_get_location);

        Toolbar toolbar = findViewById(R.id.home_toolbar);
        setSupportActionBar(toolbar);
        this.mActionBar = getSupportActionBar();
        mActionBar.setDisplayHomeAsUpEnabled(true);

        mFusedLocationClient = LocationServices.getFusedLocationProviderClient(this);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action_home bar if it is present.
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.action_home, menu);

        // Searchable configuration
        //SearchManager searchManager = (SearchManager)getSystemService(Context.SEARCH_SERVICE);
        //mSearchView = (SearchView)menu.findItem(R.id.search).getActionView();
        //mSearchView.setSearchableInfo(searchManager.getSearchableInfo(getComponentName()));
        //setupSearchView();

        return super.onCreateOptionsMenu(menu);
    }

    @Override
    @SuppressLint("RestrictedApi")
    public void onBackPressed() {
        /*if (!mSearchView.isIconified()) {
            mSearchView.setQuery("", false);
            mSearchView.setIconified(true);
        }*/
        super.onBackPressed();
    }

    /**
     * Set up the functionality of mNearbyItemsFragment.
     */
    private void setUpNearbyItemsFragment() {
        mNearbyItemsFragment.setRequestStrategy(new NearbyItemsFragment.FeedStrategy());
        getSupportFragmentManager()
                .beginTransaction()
                .replace(R.id.home_container, mNearbyItemsFragment)
                .commit();
    }

    @Override
    public void onStart() {
        super.onStart();

        if (!checkPermissions()) {
            requestPermissions();
        } else {
            getLastLocation();
        }
    }

    /**
     * Provides a simple way of getting a device's location and is well suited for
     * applications that do not require a fine-grained location and that do not need location
     * updates. Gets the best and most recent location currently available, which may be null
     * in rare cases when a location is not available.
     * <p>
     * Note: this method should be called after location permission has been granted.
     */
    @SuppressWarnings("MissingPermission")
    private void getLastLocation() {
        mFusedLocationClient.getLastLocation()
                .addOnCompleteListener(this, new OnCompleteListener<Location>() {
                    @Override
                    public void onComplete(@NonNull Task<Location> task) {
                        if (task.isSuccessful() && task.getResult() != null) {
                            mLastLocation = task.getResult();

                            setTitle("Nearby Heritages");
                            mNearbyItemsFragment = new NearbyItemsFragment();
                            Bundle args = new Bundle();
                            args.putDouble("latitude",mLastLocation.getLatitude());
                            args.putDouble("longitude",mLastLocation.getLongitude());
                            mNearbyItemsFragment.setArguments(args);
                            setUpNearbyItemsFragment();
                        } else {
                            Log.w(TAG, "getLastLocation:exception", task.getException());
                            showSnackbar(getString(R.string.no_location_detected));
                        }
                    }
                });
    }

    /**
     * Shows a {@link Snackbar} using {@code text}.
     *
     * @param text The Snackbar text.
     */
    private void showSnackbar(final String text) {
        View container = findViewById(R.id.home_layout);
        if (container != null) {
            Snackbar.make(container, text, Snackbar.LENGTH_LONG).show();
        }
    }

    /**
     * Shows a {@link Snackbar}.
     *
     * @param mainTextStringId The id for the string resource for the Snackbar text.
     * @param actionStringId   The text of the action item.
     * @param listener         The listener associated with the Snackbar action.
     */
    private void showSnackbar(final int mainTextStringId, final int actionStringId,
                              View.OnClickListener listener) {
        Snackbar.make(findViewById(android.R.id.content),
                getString(mainTextStringId),
                Snackbar.LENGTH_INDEFINITE)
                .setAction(getString(actionStringId), listener).show();
    }

    /**
     * Return the current state of the permissions needed.
     */
    private boolean checkPermissions() {
        int permissionState = ActivityCompat.checkSelfPermission(this,
                Manifest.permission.ACCESS_COARSE_LOCATION);
        return permissionState == PackageManager.PERMISSION_GRANTED;
    }

    private void startLocationPermissionRequest() {
        ActivityCompat.requestPermissions(GetLocation.this,
                new String[]{Manifest.permission.ACCESS_COARSE_LOCATION},
                REQUEST_PERMISSIONS_REQUEST_CODE);
    }

    private void requestPermissions() {
        boolean shouldProvideRationale =
                ActivityCompat.shouldShowRequestPermissionRationale(this,
                        Manifest.permission.ACCESS_COARSE_LOCATION);

        // Provide an additional rationale to the user. This would happen if the user denied the
        // request previously, but didn't check the "Don't ask again" checkbox.
        if (shouldProvideRationale) {
            Log.i(TAG, "Displaying permission rationale to provide additional context.");

            showSnackbar(R.string.permission_rationale, android.R.string.ok,
                    new View.OnClickListener() {
                        @Override
                        public void onClick(View view) {
                            // Request permission
                            startLocationPermissionRequest();
                        }
                    });

        } else {
            Log.i(TAG, "Requesting permission");
            // Request permission. It's possible this can be auto answered if device policy
            // sets the permission in a given state or the user denied the permission
            // previously and checked "Never ask again".
            startLocationPermissionRequest();
        }
    }

    /**
     * Callback received when a permissions request has been completed.
     */
    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions,
                                           @NonNull int[] grantResults) {
        Log.i(TAG, "onRequestPermissionResult");
        if (requestCode == REQUEST_PERMISSIONS_REQUEST_CODE) {
            if (grantResults.length <= 0) {
                // If user interaction was interrupted, the permission request is cancelled and you
                // receive empty arrays.
                Log.i(TAG, "User interaction was cancelled.");
            } else if (grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                // Permission granted.
                getLastLocation();
            } else {
                // Permission denied.

                // Notify the user via a SnackBar that they have rejected a core permission for the
                // app, which makes the Activity useless. In a real app, core permissions would
                // typically be best requested during a welcome-screen flow.

                // Additionally, it is important to remember that a permission might have been
                // rejected without asking the user for permission (device policy or "Never ask
                // again" prompts). Therefore, a user interface affordance is typically implemented
                // when permissions are denied. Otherwise, your app could appear unresponsive to
                // touches or interactions which have required permissions.
                showSnackbar(R.string.permission_denied_explanation, R.string.settings,
                        new View.OnClickListener() {
                            @Override
                            public void onClick(View view) {
                                // Build intent that displays the App settings screen.
                                Intent intent = new Intent();
                                intent.setAction(
                                        Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
                                Uri uri = Uri.fromParts("package",
                                        BuildConfig.APPLICATION_ID, null);
                                intent.setData(uri);
                                intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                                startActivity(intent);
                            }
                        });
            }
        }
    }

    //private void requestSearchResults(String query, OnGetItemsResponse.GetItemCallback getItemCallback) {}

    /**
     * Set up the functionality of mSearchView.
     */
    /*private void setupSearchView() {
        // when search icon is pressed, show mSearchItemsFragment.
        mSearchView.setOnSearchClickListener((View v) -> {
            Fragment currFragment = getSupportFragmentManager().findFragmentById(R.id.home_container);
            if (!currFragment.equals(mSearchItemsFragment)) {
                getSupportFragmentManager()
                        .beginTransaction()
                        .replace(R.id.home_container, mSearchItemsFragment)
                        .addToBackStack(null)
                        .commit();
            }
        });
        // when a query is submitted, clear and load items from mSearchItemsFragment.
        mSearchView.setOnQueryTextListener(new SearchView.OnQueryTextListener() {
            @Override
            public boolean onQueryTextSubmit(String query) {
                mSearchItemsFragment.clearItems();
                mSearchItemsFragment.loadMoreItems();
                return true;
            }

            @Override
            public boolean onQueryTextChange(String newText) {
                // TODO: implement autocompletion
                return true;
            }
        });
    }*/

    /**
     * Set up the functionality of mSearchItemsFragment. This method sets how mSearchItemsFragment
     * requests its items from the server.
     */
    /*private void setUpSearchFragment() {
        mSearchItemsFragment.setRequestStrategy(new ListItemsFragment.RequestStrategy() {
            @Override
            public void requestItems(Context context, int offset, OnGetItemsResponse.GetItemCallback getItemCallback) {
                // TODO: pagination for search results
                String authStr = Utils.getSharedPref(getApplicationContext()).getString(Constants.AUTH_STR, Constants.NO_AUTH_STR);
                String query = mSearchView.getQuery().toString();
                OnGetItemsResponse respHandler = new OnGetItemsResponse(context, getItemCallback);
                APIUtils.serverAPI().search(authStr, query).enqueue(respHandler);
            }
        });
        mSearchItemsFragment.setRequestImmediately(false);
    }*/
}

