package hello;

public class User {
  private String firstName;
  private String lastName;

  public User(String firstName, String lastName) {
    this.firstName = firstName;
    this.lastName = lastName;
  }

  public String getName() {
    return this.firstName + this.lastName;
  }
}
