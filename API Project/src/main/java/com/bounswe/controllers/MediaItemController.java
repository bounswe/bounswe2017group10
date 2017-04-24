package com.bounswe.controllers;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.Date;

import com.bounswe.models.MediaItem;
import com.bounswe.models.CulturalHeritage;
import com.bounswe.services.MediaItemService;
import com.bounswe.services.CulturalHeritageService;

@RestController
public class MediaItemController {
  private MediaItemService mediaItemService;
  private CulturalHeritageService culturalHeritageService;

  @Autowired
  public MediaItemController(MediaItemService mediaItemService){
    this.mediaItemService = mediaItemService;
  }

  @RequestMapping("/get-media-items")
  public ArrayList<MediaItem> getMediaItems() {
    return this.mediaItemService.findAll();
  }

  @RequestMapping("users/{culturalHeritageID}/add-media-item")
  public MediaItem addMediaItem(
      @PathVariable(value="culturalHeritageID") final Long culturalHeritageID,
      @RequestParam(value="url", defaultValue="") String url) {
    try {
      CulturalHeritage culturalHeritage = this.culturalHeritageService.findOne(culturalHeritageID);

      MediaItem mediaItem = new MediaItem(culturalHeritage, url);
      this.mediaItemService.save(mediaItem);
      return mediaItem;
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }
}
