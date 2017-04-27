package com.bounswe.controllers;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.DeleteMapping;

import java.util.ArrayList;
import java.util.Date;

import com.bounswe.models.User;
import com.bounswe.models.MediaItem;
import com.bounswe.models.CulturalHeritage;
import com.bounswe.services.UserService;
import com.bounswe.services.MediaItemService;
import com.bounswe.services.CulturalHeritageService;

@RestController
public class MediaItemController {
  private MediaItemService mediaItemService;
  private CulturalHeritageService culturalHeritageService;
  private UserService userService;
  
  @Autowired
  public MediaItemController(MediaItemService mediaItemService, CulturalHeritageService culturalHeritageService, UserService userService){
    this.mediaItemService = mediaItemService;
    this.culturalHeritageService = culturalHeritageService;
    this.userService = userService;
  }

  @GetMapping("/media-items")
  public ArrayList<MediaItem> getMediaItems() {
	  
    return this.mediaItemService.findAll();
  }

  @PostMapping("users/{userID}/cultural-heritages/{culturalHeritageID}/media-items")
  public MediaItem addMediaItem(
		  @PathVariable(value="userID") String userID,
		  @PathVariable(value="culturalHeritageID") String culturalHeritageID,
		  @RequestBody MediaItem mediaItem) {
    try {
      User owner = this.userService.findOne(Long.parseLong(userID));
      CulturalHeritage culturalHeritageItem = this.culturalHeritageService.findOne(Long.parseLong(culturalHeritageID));
      
      mediaItem.setOwner(owner);
      mediaItem.setCulturalHeritage(culturalHeritageItem);
      this.mediaItemService.save(mediaItem);
      return mediaItem;
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }
}
