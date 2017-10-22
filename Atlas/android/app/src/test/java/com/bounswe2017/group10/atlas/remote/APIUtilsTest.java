package com.bounswe2017.group10.atlas.remote;


import org.junit.Test;

import retrofit2.Retrofit;

import static org.junit.Assert.*;

public class APIUtilsTest {

    @Test
    public void testServerAPI() {
        API api = APIUtils.serverAPI();
        assertNotNull(api);
    }

    @Test
    public void testSetServerAPI() {
        API newAPI = new RetrofitBuilder().baseUrl("http://localhost").build().create(API.class);
        APIUtils.setServerAPI(newAPI);
        // we should get the exact same reference
        assertTrue(newAPI == APIUtils.serverAPI());
    }
}
