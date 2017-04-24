package com.bounswe.repository;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;
import com.bounswe.models.MediaItem;

@Repository
public interface MediaItemRepository extends CrudRepository<MediaItem, Long> {

}