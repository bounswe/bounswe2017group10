package com.bounswe2017.group10.atlas.remote;


import java.util.concurrent.TimeUnit;

import okhttp3.OkHttpClient;
import okhttp3.logging.HttpLoggingInterceptor;
import retrofit2.Converter;
import retrofit2.Retrofit;


/**
 * Builder class to build retrofit instances.
 */
public class RetrofitBuilder {
    private Retrofit.Builder retrofitBuilder;
    private OkHttpClient.Builder okhttpBuilder;

    public RetrofitBuilder() {
        this.retrofitBuilder = new Retrofit.Builder();
        this.okhttpBuilder = new OkHttpClient.Builder();
    }

    /**
     * Set server baseURL.
     *
     * @param baseUrl URL.
     * @return This object.
     */
    public RetrofitBuilder baseUrl(String baseUrl) {
        this.retrofitBuilder.baseUrl(baseUrl);
        return this;
    }

    /**
     * Set read from server timeout.
     * @param timeout Timeout amount.
     * @param unit Timeout unit.
     * @return This object.
     */
    public RetrofitBuilder readTimeout(long timeout, TimeUnit unit) {
        this.okhttpBuilder.readTimeout(timeout, unit);
        return this;
    }

    /**
     * Set write from server timeout.
     * @param timeout Timeout amount.
     * @param unit Timeout unit.
     * @return This object.
     */
    public RetrofitBuilder writeTimeout(long timeout, TimeUnit unit) {
        this.okhttpBuilder.writeTimeout(timeout, unit);
        return this;
    }

    /**
     * Set connect from server timeout.
     * @param timeout Timeout amount.
     * @param unit Timeout unit.
     * @return This object.
     */
    public RetrofitBuilder connectTimeout(long timeout, TimeUnit unit) {
        this.okhttpBuilder.connectTimeout(timeout, unit);
        return this;
    }

    /**
     * Add converter factory to be used for converting to/from POJO classes and JSON bodies.
     * @param factory converter factory object.
     * @return This object.
     */
    public RetrofitBuilder addConverterFactory(Converter.Factory factory) {
        this.retrofitBuilder.addConverterFactory(factory);
        return this;
    }

    /**
     * Add interceptor to monitor HTTP traffic.
     */
    public RetrofitBuilder addInterceptor() {
        HttpLoggingInterceptor interceptor = new HttpLoggingInterceptor();
        interceptor.setLevel(HttpLoggingInterceptor.Level.BODY);
        this.okhttpBuilder.addInterceptor(interceptor);
        return this;
    }

    /**
     * Build the object.
     * @return Built Retrofit object.
     */
    public Retrofit build() {
        return retrofitBuilder.client(okhttpBuilder.build()).build();
    }
}
