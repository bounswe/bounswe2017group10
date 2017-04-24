package com.bounswe.controllers;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.Date;

import com.bounswe.models.MediaItem;
import com.bounswe.services.MediaItemService;

@RestController
public class MediaItemController {
  private MediaItemService mediaItemService;

  @Autowired
  public MediaItemController(MediaItemService mediaItemService{
    this.mediaItemService = mediaItemService;
  }

  @RequestMapping("/get-media-items")
  public ArrayList<MediaItem> getMediaItems() {
    return this.mediaItemService.findAll();
  }

  @RequestMapping("users/{culturalHeritageID}/add-media-item")
  public CulturalHeritage addCulturalHeritage(
      @PathVariable(value="userId") final Long userId,
      @RequestParam(value="title", defaultValue="") String title,
      @RequestParam(value="description", defaultValue="") String description,
      @RequestParam(value="continent", defaultValue="") String continent,
      @RequestParam(value="city", defaultValue="") String city
    ) {
    try {
      User user = this.userService.findOne(userId);

      CulturalHeritage culturalHeritage = new CulturalHeritage(user, title, description, continent, city, new Date());
      this.culturalHeritageService.save(culturalHeritage);
      return culturalHeritage;
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }
}
