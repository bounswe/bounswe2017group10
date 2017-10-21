package com.bounswe2017.group10.atlas.remote;



import org.junit.Before;
import org.junit.Test;

import retrofit2.Converter;
import retrofit2.Retrofit;
import retrofit2.converter.gson.GsonConverterFactory;

import static org.junit.Assert.*;

public class RetrofitBuilderTest {

    private final String TEST_URL = "http://12.24.72.76:81/";
    private GsonConverterFactory factory;
    private Retrofit retrofit;

    @Before
    public void init() {
        factory = GsonConverterFactory.create();
        retrofit = new RetrofitBuilder()
                .baseUrl(TEST_URL)
                .addConverterFactory(factory)
                .build();
    }

    @Test
    public void testRetrofitBuilder() {
        // we get a retrofit object.
        assertNotNull(retrofit);
        // urls are the same.
        assertEquals(TEST_URL, retrofit.baseUrl().toString());
        // assert that our factory is in place
        boolean inside = false;
        for (Converter.Factory currFactory : retrofit.converterFactories()) {
            if (factory == currFactory)
                inside = true;
        }
        assertTrue(inside);
    }
}
