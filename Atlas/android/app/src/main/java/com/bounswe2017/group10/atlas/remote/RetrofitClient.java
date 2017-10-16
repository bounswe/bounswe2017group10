package com.bounswe2017.group10.atlas.remote;

import okhttp3.OkHttpClient;
import okhttp3.logging.HttpLoggingInterceptor;
import retrofit2.Retrofit;
import retrofit2.converter.gson.GsonConverterFactory;


/**
 * Maintain a single retrofit client that uses GsonConverterFactory to parse
 * POJOs to/from JSON strings automatically.
 */
public class RetrofitClient {

    private static Retrofit retrofit = null;

    public static Retrofit getClient(String baseUrl) {
        if (retrofit == null) {
            HttpLoggingInterceptor interceptor = new HttpLoggingInterceptor();
            interceptor.setLevel(HttpLoggingInterceptor.Level.BODY);
            OkHttpClient interceptorClient = new OkHttpClient.Builder().addInterceptor(interceptor).build();

            retrofit = new Retrofit.Builder()
                    .baseUrl(baseUrl)
                    .client(interceptorClient)
                    .addConverterFactory(GsonConverterFactory.create())
                    .build();
        }
        return retrofit;
    }
}
