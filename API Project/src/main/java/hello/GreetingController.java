package hello;

import java.util.concurrent.atomic.AtomicLong;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import java.util.ArrayList;

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

  @RequestMapping("/greeting")
  public Greeting greeting(@RequestParam(value="name", defaultValue="World") String name) {
      return new Greeting(counter.incrementAndGet(),
                          String.format(template, name));
  }
}
