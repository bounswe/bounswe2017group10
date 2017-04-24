package com.bounswe.services;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import java.util.ArrayList;
import java.util.Iterator;
import java.lang.Iterable;
import com.bounswe.repository.MediaItemRepository;
import com.bounswe.models.MediaItem;

@Service
public class MediaItemService {
  private MediaItemRepository mediaItemRepository;

  @Autowired
  public MediaItemService(MediaItemRepository mediaItemRepository) {
    this.mediaItemRepository = mediaItemRepository;
  }

  public Long getCount() {
    return this.mediaItemRepository.count();
  }

  public void save(MediaItem mediaItem) {
    this.mediaItemRepository.save(mediaItem);
  }

  public ArrayList<MediaItem> findAll() {
    Iterator<MediaItem> it = this.mediaItemRepository.findAll().iterator();
    ArrayList<MediaItem> mediaItems = new ArrayList<>();

    while(it.hasNext()) mediaItems.add(it.next());
    return mediaItems;
  }
}