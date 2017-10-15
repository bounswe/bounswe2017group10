package com.bounswe2017.group10.atlas.auth;


import retrofit2.HttpException;

/**
 * Manager class for making login and signup requests and handling responses.
 */
public class AuthManager {

    /**
     * Makes a login request with the given json as HTTP body. Any response code other than
     * HTTP 2xx codes throws an exception with a corresponding error message.
     *
     * @param jsonBody Json to be sent as HTTP body.
     * @return Response body.
     */
    public static String login(String jsonBody) throws HttpException {
        return "";
    }

    /**
     * Makes a signup request with the given json as HTTP body. Any response code other than
     * HTTP 2xx codes throws an exception with a corresponding error message.
     *
     * @param jsonBody Json to be sent as HTTP body.
     * @return Response body.
     */
    public static String signup(String jsonBody) throws HttpException {
        return "";
    }
}
