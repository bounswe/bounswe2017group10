package com.bounswe.models;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import static java.lang.System.*;

public class MediaItemTests {

  @Test
  public void getId_whenInstanceCreated_thenGetId() {

    try
    {
      //int nIteration = 0;
      //int nMaxNumberOfIteration = 100;

      //while(nIteration < nMaxNumberOfIteration)
      {
        // Given
        User cUser = new User("Name", "Surname");
        CulturalHeritage cCulturalHeritage = new CulturalHeritage(cUser,
                                                                  "Title",
                                                                  "Description",
                                                                  "Continent",
                                                                  "City"
                                                                  );

        MediaItem cMediaItem = new MediaItem(cUser, cCulturalHeritage, "url");


        // Then
        assertThat(cMediaItem.getUrl()).isEqualTo("url");
        assertThat(cMediaItem.getId()).isEqualTo(1);
        //System.out.println(cMediaItem.getId());

        //nIteration++;
      }

    }
    catch(Exception e)
    {
      assertTrue("Exception caught",false);
    }
  }

}
