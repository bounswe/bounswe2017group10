package com.bounswe.models;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class MediaItemTests {

  @Test
  public void getId_whenInstanceCreated_thenGetId() {

    try
    {
      long nIteration = 1;
      long nMaxNumberOfIteration = 100;

      MediaItem cFirstMediaItem = new MediaItem(new User("Name", "Surname"),
                                                new CulturalHeritage( new User("Name", "Surname"),
                                                                      "Title",
                                                                      "Description",
                                                                      "Continent",
                                                                      "City"
                                                                      ),
                                                "Url");

      long cInitialId = cFirstMediaItem.getId();

      while(nIteration < nMaxNumberOfIteration)
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
        final long nInitialId = cMediaItem.getId();

        // Then
        assertThat(cMediaItem.getId()).isEqualTo(cInitialId + nIteration);

        nIteration++;
      }

    }
    catch(Exception e)
    {
      assertTrue("Exception caught",false);
    }
  }

  @Test
  public void getUrl_whenSetUrlCalled_thenGetUrl() {

    try
    {
      long nIteration = 1;
      long nMaxNumberOfIteration = 100;

      while(nIteration < nMaxNumberOfIteration)
      {
        // Given
        User cUser = new User("Name", "Surname");
        CulturalHeritage cCulturalHeritage = new CulturalHeritage(cUser,
                                                                  "Title",
                                                                  "Description",
                                                                  "Continent",
                                                                  "City"
                                                                  );

        MediaItem cMediaItem = new MediaItem(cUser, cCulturalHeritage, "");
        cMediaItem.setUrl("Url" + Integer.toString((int)nIteration));

        // Then
        assertThat(cMediaItem.getUrl()).isEqualTo("Url" + Integer.toString((int)nIteration));

        nIteration++;
      }

    }
    catch(Exception e)
    {
      assertTrue("Exception caught",false);
    }
  }

}
