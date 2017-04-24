package com.bounswe.models;
import com.bounswe.models.CulturalHeritage;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Column;
import java.util.Date;

@Entity
public class MediaItem {
	@Id @GeneratedValue private Long id;
	private String url;
	private Date createdTime;
	private CulturalHeritage culturalHeritageItem;
	  //private long id?

 public MediaItem(CulturalHeritage culturalHeritageItem, String url) {
	 this.url = url;
	 this.createdTime = new Date();
	 this.culturalHeritageItem = culturalHeritageItem;
  }
 
 public String GetUrl(){
	 return this.url;
 }
 
 public String GetCreatedTime()
 {
	 return this.createdTime.toString();
 }
 
 public String GetMediaItemasJson(){
	 return "url: "+this.url+ ",Created Time: " + this.GetCreatedTime();
  }
  
  
}