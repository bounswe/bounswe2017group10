package com.bounswe.models;
import com.bounswe.models.CulturalHeritage;
import com.bounswe.models.User;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Column;
import java.util.Date;
import javax.persistence.ManyToOne;

@Entity
public class MediaItem {
	@Id @GeneratedValue private Long id;
	private String url;
	@ManyToOne
	private CulturalHeritage culturalHeritage;
	
	@ManyToOne
	private User owner;
	
	  //private long id?

	protected MediaItem(){
		
	}
	
 public MediaItem(User owner, CulturalHeritage culturalHeritage, String url) {
	 this.url = url;
	 this.culturalHeritage = culturalHeritage;
	 this.owner = owner;
  }
 
 public Long getId() {
		return id;
}
 
 public void setUrl(String url)
 {
	 this.url = url;
 }
 
 public String getUrl(){
	 return this.url;
 }
 
 public void setCulturalHeritage(CulturalHeritage culturalHeritage){
	 this.culturalHeritage = culturalHeritage;
	 
 }
 
 public CulturalHeritage getCulturalHeritage(){
	 return this.culturalHeritage;
 }
 
 public void setOwner(User owner){
	 this.owner = owner;
 }
 
 public User getOwner(){
	 return this.owner;
 }
}