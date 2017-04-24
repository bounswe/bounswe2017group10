package com.bounswe.models;

import java.util.Date;
import java.util.ArrayList;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Column;
import javax.persistence.ManyToOne;
import com.bounswe.models.User;

@Entity
public class CulturalHeritage {
  @Id
  @GeneratedValue
 	private Long id;
  @ManyToOne
 	private User owner;
	private String title;
	private String description;
	//private ArrayList<Comment> comments;
	//private ArrayList<MediaItem> mediaItems;
	private String continent;
	private String city;
	//private ArrayList<Tag> tags;
	//private AdvancedDate startDate;
	//private AdvancedDate endDate;
	private Date createdTime;
	private Date updatedTime;
	

  protected CulturalHeritage() {

  }

  public CulturalHeritage(User owner,String title, String description, String continent, String city,Date createdTime) {
    this.owner=owner;
    this.title = title;
    this.description = description;
    this.continent = continent;
    this.city=city;
    this.createdTime = createdTime;
  }

public String getName() {
    return this.title + ": "+ this.description;
  }
}
