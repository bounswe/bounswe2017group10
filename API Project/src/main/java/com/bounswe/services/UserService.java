package com.bounswe.services;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import java.util.ArrayList;
import java.util.Iterator;
import java.lang.Iterable;
import com.bounswe.repository.UserRepository;
import com.bounswe.models.User;

@Service
public class UserService {
// Methods of the repository can be found here:
// https://docs.spring.io/spring-data/commons/docs/current/api/org/springframework/data/repository/CrudRepository.html
  private UserRepository userRepository;

  @Autowired
  public UserService(UserRepository userRepository) {
    this.userRepository = userRepository;
  }

  public Long getCount() {
    return this.userRepository.count();
  }

  public void save(User user) {
    this.userRepository.save(user);
  }

  public ArrayList<User> getAllUsers() {
    Iterator<User> it = this.userRepository.findAll().iterator();
    ArrayList<User> users = new ArrayList<>();

    // There are literally no faster way: http://stackoverflow.com/a/10117051/3138171
    while(it.hasNext()) users.add(it.next());
    return users;
  }
}
