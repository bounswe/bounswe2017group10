package com.bounswe2017.group10.atlas.home;


import android.app.AlertDialog;
import android.app.SearchManager;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.location.Address;
import android.location.Geocoder;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.SearchView;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.View;
import android.view.inputmethod.InputMethodManager;

import com.bounswe2017.group10.atlas.R;
import com.bounswe2017.group10.atlas.remote.GoogleMapsLocationAsyncTask;
import com.bounswe2017.group10.atlas.util.Constants;
import com.bounswe2017.group10.atlas.util.Utils;
import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.OnMapReadyCallback;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;

import java.util.List;
import java.util.Locale;

public class GoogleMapsActivity extends AppCompatActivity implements OnMapReadyCallback, GoogleMapsLocationAsyncTask.ResultCallback {

    private static final int MAX_SEARCH_RESULTS = 5;
    private static final float DEFAULT_SEARCH_ZOOM = 8f;

    private GoogleMap mGoogleMap = null;
    private SearchView mSearchView;
    private Geocoder mGeoCoder;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_google_maps);

        mGeoCoder = new Geocoder(this);
        Toolbar toolbar = findViewById(R.id.google_maps_toolbar);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayShowTitleEnabled(false);

        SupportMapFragment mapFragment = (SupportMapFragment)getSupportFragmentManager().findFragmentById(R.id.map);

        mapFragment.getMapAsync(this);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action_home bar if it is present.
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.action_google_maps, menu);

        // Searchable configuration
        SearchManager searchManager = (SearchManager)getSystemService(Context.SEARCH_SERVICE);
        mSearchView = (SearchView)menu.findItem(R.id.search).getActionView();
        mSearchView.setSearchableInfo(searchManager.getSearchableInfo(getComponentName()));
        setupSearchView();

        return super.onCreateOptionsMenu(menu);
    }

    /**
     * If search bar is open, close it instead of going back.
     */
    @Override
    public void onBackPressed() {
        if (!mSearchView.isIconified()) {
            mSearchView.setQuery("", false);
            mSearchView.setIconified(true);
        } else {
            super.onBackPressed();
        }
    }

    /**
     * Set up the functionality of mSearchView.
     */
    private void setupSearchView() {
        // when a query is submitted, clear and load items from mSearchItemsFragment.
        mSearchView.setOnQueryTextListener(new SearchView.OnQueryTextListener() {

            @Override
            public boolean onQueryTextSubmit(String query) {
                // close the keyboard
                View v = getCurrentFocus();
                if (v != null) {
                    InputMethodManager imm = (InputMethodManager)getSystemService(Context.INPUT_METHOD_SERVICE);
                    imm.hideSoftInputFromWindow(v.getWindowToken(), 0);
                }

                // search the location
                new GoogleMapsLocationAsyncTask(mGeoCoder, MAX_SEARCH_RESULTS, GoogleMapsActivity.this).execute(query);
                return true;
            }
            @Override
            public boolean onQueryTextChange(String newText) {
                // do nothing
                return true;
            }
        });
    }

    /**
     * Callback method that will be called by GoogleMapsLocationAsyncTask when it
     * completes its task.
     *
     * @param resultList List of results for the given query.
     */
    @Override
    public void onDataRetrieved(List<Address> resultList) {
        if (resultList == null) {
            Utils.showToast(this, getString(R.string.connection_failure));
        } else if (resultList.isEmpty()) {
            Utils.showToast(this, getString(R.string.unable_get_location));
        } else {
            // show all the markers on the map
            mGoogleMap.clear();
            for (Address address : resultList) {
                addMarker(address);
            }

            // change the camera to the first result
            Address firstResult = resultList.get(0);
            LatLng cameraCenter = new LatLng(firstResult.getLatitude(), firstResult.getLongitude());
            mGoogleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(cameraCenter, DEFAULT_SEARCH_ZOOM));
        }
    }

    /**
     * Add a new marker on the given location to mGoogleMap.
     *
     * @param address Address object containing information about a location.
     *
     * @return Marker object representing the location on the map.
     */
    public Marker addMarker(Address address) {
        MarkerOptions options = new MarkerOptions();
        double lat = address.getLatitude();
        double lon = address.getLongitude();

        options.position(new LatLng(lat, lon));

        if (address.getFeatureName() == null) {
            options.title(getString(R.string.lat_long_location, lat, lon));
        } else {
            options.title(address.getFeatureName());
        }

        return mGoogleMap.addMarker(options);
    }

    /**
     * Callback method that will be called when SupportMapFragment is ready and can be
     * used.
     *
     * @param googleMap GoogleMap object that handles settings objects on GoogleMap fragment.
     */
    @Override
    public void onMapReady(GoogleMap googleMap) {
        this.mGoogleMap = googleMap;

        mGoogleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(new LatLng(39, 35), 4.5f));

        // show information about location
        mGoogleMap.setOnMarkerClickListener((Marker marker) -> {
            showMarkerInformation(marker);
            return false;
        });

        // add a marker when map is clicked and show information
        mGoogleMap.setOnMapClickListener((LatLng latLng) -> {
            Address address = new Address(Locale.getDefault());
            address.setLatitude(latLng.latitude);
            address.setLongitude(latLng.longitude);

            Marker newMarker = addMarker(address);
            showMarkerInformation(newMarker);
        });
    }

    /**
     * Show information related to location
     *
     * @param marker Marker object that contains LatLang and name about its location.
     */
    private void showMarkerInformation(Marker marker) {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle(marker.getTitle());
        builder.setPositiveButton(R.string.ok, (DialogInterface dialog, int which) -> {
            Intent resultData = new Intent();
            resultData.putExtra(Constants.LOCATION_NAME, marker.getTitle());
            resultData.putExtra(Constants.LATLONG, marker.getPosition());
            setResult(RESULT_OK, resultData);
            finish();
        });
        builder.setNegativeButton(R.string.cancel, null);
        builder.create().show();
    }
}
