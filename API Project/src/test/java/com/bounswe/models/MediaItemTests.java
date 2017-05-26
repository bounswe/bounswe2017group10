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

  @Test
  public void getCulturalHeritage_whenSetCulturalHeritageCalled_thenGetCulturalHeritage() {

    try
    {
      // Given
      User cUser = new User("Name", "Surname");
      CulturalHeritage cCulturalHeritage1 = new CulturalHeritage(cUser,
                                                                "Title1",
                                                                "Description1",
                                                                "Continent1",
                                                                "City1"
                                                                );

      MediaItem cMediaItem = new MediaItem(cUser, cCulturalHeritage1, "Url");

      CulturalHeritage cCulturalHeritage2 = new CulturalHeritage(cUser,
                                                                "Title2",
                                                                "Description2",
                                                                "Continent2",
                                                                "City2"
                                                                );

      cMediaItem.setCulturalHeritage(cCulturalHeritage2);

      // Then
      assertThat(cMediaItem.getCulturalHeritage().getTitle()).isEqualTo("Title2");
      assertThat(cMediaItem.getCulturalHeritage().getDescription()).isEqualTo("Description2");
      assertThat(cMediaItem.getCulturalHeritage().getContinent()).isEqualTo("Continent2");
      assertThat(cMediaItem.getCulturalHeritage().getCity()).isEqualTo("City2");

    }
    catch(Exception e)
    {
      assertTrue("Exception caught",false);
    }
  }

  @Test
  public void getOwner_whenSetOwnerCalled_thenGetOwner() {

    try
    {
      // Given
      User cUser1 = new User("Name1", "Surname1");

      CulturalHeritage cCulturalHeritage = new CulturalHeritage(cUser1,
                                                                "Title",
                                                                "Description",
                                                                "Continent",
                                                                "City"
                                                                );

      MediaItem cMediaItem = new MediaItem(cUser1, cCulturalHeritage, "Url");

      User cUser2 = new User("Name2", "Surname2");

      cMediaItem.setOwner(cUser2);

      // Then
      assertThat(cMediaItem.getOwner().getName()).isEqualTo("Name2 Surname2");

    }
    catch(Exception e)
    {
      assertTrue("Exception caught",false);
    }
  }


}
