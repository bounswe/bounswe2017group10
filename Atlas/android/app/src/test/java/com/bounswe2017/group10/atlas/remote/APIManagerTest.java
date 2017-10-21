package com.bounswe2017.group10.atlas.remote;

import org.junit.Before;
import org.junit.Test;


import static org.junit.Assert.*;

public class APIManagerTest {

    private APIManager apiManager;
    private final String baseUrl = "http://12.12.12.12:242/";

    @Before
    public void init() {
        apiManager = new APIManager(baseUrl);
    }

    @Test
    public void testGetBaseUrl() {
        assertEquals(baseUrl, apiManager.getBaseUrl());
    }

    @Test
    public void testGetAPI() {
        // check that we get an API
        API api = apiManager.getAPI();
        assertNotNull(api);
        // we get the same API object
        assertTrue(api == apiManager.getAPI());
    }
}
