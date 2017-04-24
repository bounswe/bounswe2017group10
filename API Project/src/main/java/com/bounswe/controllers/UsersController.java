package com.bounswe.controllers;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import java.util.ArrayList;
import java.lang.Iterable;
import com.bounswe.models.User;
import com.bounswe.services.UserService;

@RestController
public class UsersController {
  private ArrayList<User> users = new ArrayList<>();
  private UserService userService;

  @Autowired
  public UsersController(UserService userService) {
    this.userService = userService;
  }

  @GetMapping("/users")
  public ArrayList<User> getUsers() {
    return this.userService.getAllUsers();
  }

  @RequestMapping("/test")
  public Long test() {
    Long count = this.userService.getCount();
    System.out.println("Count:");
    System.out.println(count);
    return count;
  }

  @PostMapping("/users")
  public User addUser(@RequestBody User user) {
    
    this.userService.save(user);
    return user;
  }
}

