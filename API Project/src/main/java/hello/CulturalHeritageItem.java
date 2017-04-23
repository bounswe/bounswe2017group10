package hello;
import java.util.Date;
import java.util.ArrayList;

public class CulturalHeritageItem {
 	private long id;
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
	

  public CulturalHeritageItem(long id,User owner,String title, String description, String continent, String city,Date createdTime) {
	this.id = id;
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

//Last commit

}
