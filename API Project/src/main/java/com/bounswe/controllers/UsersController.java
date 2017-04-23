package com.bounswe.controllers;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import java.util.ArrayList;
import com.bounswe.models.User;

@RestController
public class UsersController {
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
}

