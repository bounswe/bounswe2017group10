package com.bounswe2017.group10.atlas.remote;


import android.location.Address;
import android.location.Geocoder;
import android.os.AsyncTask;
import android.util.Log;

import java.io.IOException;
import java.util.List;

/**
 * AsyncTask that will request address from Google Maps API from a given
 * location name in the background.
 */
public class GoogleMapsLocationAsyncTask extends AsyncTask<String, Integer, List<Address>> {

    public interface ResultCallback {
        void onDataRetrieved(List<Address> resultList);
    }

    private Geocoder mGeocoder;
    private int maxNumAddress;
    private ResultCallback mCallback;
    private final static String TAG = "GoogleMapsLocationAsync";

    /**
     * Constructor.
     *
     * @param geocoder Geocoder object that will be used to request address data.
     * @param maxNumAddresses Maximum number of addresses to return.
     */
    public GoogleMapsLocationAsyncTask(Geocoder geocoder, int maxNumAddresses, ResultCallback callback) {
        this.mGeocoder = geocoder;
        this.maxNumAddress = maxNumAddresses;
        this.mCallback = callback;
    }

    @Override
    protected List<Address> doInBackground(String... strings) {
        int count = strings.length;
        if (count != 1) {
            throw new IllegalArgumentException("Can request only one location");
        }
        String query = strings[0];
        List<Address> addressList = null;
        try {
            addressList = mGeocoder.getFromLocationName(query, maxNumAddress);
        } catch (IOException e) {
            Log.d(TAG, "IOException when getting location from Google Maps: " + e.getMessage());
        }
        return addressList;
    }

    @Override
    protected void onPostExecute(List<Address> addressList) {
        super.onPostExecute(addressList);
        this.mCallback.onDataRetrieved(addressList);
    }
}
