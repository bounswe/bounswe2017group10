package com.bounswe.controllers;
import com.bounswe.models.Greeting;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class GreetingControllerTests {

  @Test
  public void greeting_whenInvokedWithName_thenSaysHelloName() {

    try
    {
      // Given
      GreetingController greetingController = new GreetingController();
      Greeting cGreeting = greetingController.greeting("Name");
      String strGreetingContent = cGreeting.getContent();
      long nGreetingID = cGreeting.getId();

      // Then
      assertThat(strGreetingContent).isEqualTo("Hello, Name!");
      assertThat(nGreetingID).isEqualTo(1);
    }
    catch(Exception e)
    {
      assertTrue("Exception caught",false);
    }
  }

}
