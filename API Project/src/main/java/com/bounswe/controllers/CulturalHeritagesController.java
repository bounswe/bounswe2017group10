package com.bounswe.controllers;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;


import java.util.ArrayList;
import java.util.Date;

import com.bounswe.models.CulturalHeritage;
import com.bounswe.models.User;
import com.bounswe.services.UserService;
import com.bounswe.services.CulturalHeritageService;

@RestController
public class CulturalHeritagesController {
  private CulturalHeritageService culturalHeritageService;
  private UserService userService;

  @Autowired
  public CulturalHeritagesController(CulturalHeritageService culturalHeritageService, UserService userService) {
    this.culturalHeritageService = culturalHeritageService;
    this.userService = userService;
  }

  @GetMapping("/cultural-heritages")
  public ArrayList<CulturalHeritage> getCulturalHeritages() {
    return this.culturalHeritageService.findAll();
  }

  @PostMapping("users/{userId}/cultural-heritages")
  public CulturalHeritage addCulturalHeritage(
      @PathVariable(value="userId") String userId, @RequestBody CulturalHeritage culturalHeritage
    ) {
    try {
      User user = this.userService.findOne(Long.parseLong(userId));

      culturalHeritage.setOwner(user);
      this.culturalHeritageService.save(culturalHeritage);
      return culturalHeritage;
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }
}


