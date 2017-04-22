package hello;

import java.util.concurrent.atomic.AtomicLong;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.http.ResponseEntity;
import org.springframework.http.HttpStatus;
import java.util.ArrayList;
import com.google.maps.*;
import com.google.maps.model.*;
import com.google.maps.errors.*;
import java.util.Map;

@RestController
public class GreetingController {
  private static final String template = "Hello, %s!";
  private final AtomicLong counter = new AtomicLong();
  private ArrayList<User> users = new ArrayList<>();

  @RequestMapping("/get-users")
  public ArrayList<User> getUsers() {
    return this.users;
  }

  @RequestMapping("/add-user")
  public User addUser(@RequestParam(value="firstName", defaultValue="") String firstName,
                      @RequestParam(value="lastName", defaultValue="") String lastName) {
    User user = new User(firstName, lastName);
    this.users.add(user);
    return user;
  }

  @RequestMapping("/format-address")
  public Failable<Address> formatAddress(
    @RequestParam(value="address", defaultValue= "1600 Amphitheatre Parkway Mountain View, CA 94043") String address
  ) {
    String apiKey = this.getMapsApiKey();
    GeoApiContext context = new GeoApiContext().setApiKey(apiKey);
    try {
      GeocodingResult[] results =  GeocodingApi.geocode(context, address).await();
      if(results.length == 0) throw new Exception("No results could be found for provided address");
      return new Failable(new Address(results[0].formattedAddress));
    } catch(Exception e) {
      return new Failable(e.getMessage(), 500);
    }
  }

  @RequestMapping("/greeting")
  public Greeting greeting(@RequestParam(value="name", defaultValue="World") String name) {
    return new Greeting(counter.incrementAndGet(), "Hello, " + name);
  }

  private String getMapsApiKey() {
    return System.getenv().get("MAPS_API_KEY");
  }
}
