package com.bounswe2017.group10.atlas.remote;


import org.junit.Test;
import static org.junit.Assert.*;

public class APIUtilsTest {

    @Test
    public void testServerAPI() {
        API api = APIUtils.serverAPI();
        assertNotNull(api);
    }
}
