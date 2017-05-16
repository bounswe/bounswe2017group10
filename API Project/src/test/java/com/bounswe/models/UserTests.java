import com.bounswe.models.User;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class UserTests{
  //to test constructor, getName, setFirstName, setLastName methods.
  @Test
  public void getName_whenInvokedByInstanceCreation_thenGetName() {

    try
    {
      long nIteration = 1;
      long nMaxNumberOfIteration = 50;

      while(nIteration < nMaxNumberOfIteration){

        User nUser = new User();

        assertThat(nUser.getName()).isEqualTo("default default");

        User tUser = new User("Name"+Integer.toString((int)nIteration), "Surname"+Integer.toString((int)nIteration));

        assertThat(tUser.getName()).isEqualTo("Name"+Integer.toString((int)nIteration)+" Surname"+Integer.toString((int)nIteration));

        tUser.setFirstName("firstname" + Integer.toString((int)nIteration));
        tUser.setLastName("lastname" + Integer.toString((int)nIteration));

        assertThat(tUser.getName()).isEqualTo("firstname" + Integer.toString((int)nIteration)+" lastname" + Integer.toString((int)nIteration));

        nIteration++;
      }
    }
    catch(Exception e)
    {
      assertTrue("Exception caught",false);
    }
  }
  //to test constructor, getPassword, setPassword methods.
  @Test
  public void getPassword_whenInvokedByInstanceCreation_thenGetPassword() {

    try
    {
      long nIteration = 1;
      long nMaxNumberOfIteration = 50;

      while(nIteration < nMaxNumberOfIteration){

        User tUser = new User("Name"+Integer.toString((int)nIteration),"Surname"+Integer.toString((int)nIteration),"Email"+Integer.toString((int)nIteration),"UserName"+Integer.toString((int)nIteration),"Password"+Integer.toString((int)nIteration),"Password"+Integer.toString((int)nIteration));

        assertThat(tUser.getPassword()).isEqualTo("Password"+Integer.toString((int)nIteration));

        tUser.setPassword("Pass"+Integer.toString((int)nIteration));

        assertThat(tUser.getPassword()).isEqualTo("Pass"+Integer.toString((int)nIteration));

        nIteration++;
      }
    }
    catch(Exception e)
    {
      assertTrue("Exception caught",false);
    }
  }
  //to test constructor, getEmail, setEmail methods.
  @Test
  public void getEmail_whenInvokedByInstanceCreation_thenGetEmail() {

    try
    {
      long nIteration = 1;
      long nMaxNumberOfIteration = 50;

      while(nIteration < nMaxNumberOfIteration){

        User tUser = new User("Name"+Integer.toString((int)nIteration),"Surname"+Integer.toString((int)nIteration),"Email"+Integer.toString((int)nIteration),"UserName"+Integer.toString((int)nIteration),"Password"+Integer.toString((int)nIteration),"Password"+Integer.toString((int)nIteration));

        assertThat(tUser.getEmail()).isEqualTo("Email"+Integer.toString((int)nIteration));

        tUser.setEmail("email"+Integer.toString((int)nIteration));

        assertThat(tUser.getEmail()).isEqualTo("email"+Integer.toString((int)nIteration));

        nIteration++;
      }
    }
    catch(Exception e)
    {
      assertTrue("Exception caught",false);
    }
  }
}
