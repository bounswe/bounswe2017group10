import com.bounswe.models.Greeting;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class GreetingTests{

  @Test
  public void getId_whenInvokedByInstanceCreation_thenGetID() {

    // Given
    long nIteration = 1;
    long nMaxNumberOfIteration = 100;

    try
    {
      //Then
      while(nIteration < nMaxNumberOfIteration)
      {
          Greeting cGreeting = new Greeting(nIteration, "Name");
          long nGreetingID = cGreeting.getId();
          assertThat(nGreetingID).isEqualTo(nIteration);
          nIteration++;

      }
    }
    catch(Exception e)
    {
      assertTrue("Exception caught",false);
    }
  }

  public void getContent_whenInvokedByInstanceCreation_thenGetContent() {

    // Given
    String[] strNameList = {"Name1", "Name2", "Name3", "Name4", "Name5"};
    long nIteration = 0;
    long nMaxNumberOfIteration = strNameList.length;

    try
    {
      while(nIteration < nMaxNumberOfIteration)
      {
        Greeting cGreeting = new Greeting(nIteration, strNameList[(int)nIteration]);
        assertThat(cGreeting.getContent()).isEqualTo(strNameList[(int)nIteration]);
      }
    }
    catch(Exception e)
    {
      assertTrue("Exception caught",false);
    }
  }
}
