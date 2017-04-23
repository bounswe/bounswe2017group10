package hello;

import java.util.concurrent.atomic.AtomicLong;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import sun.security.util.Length;

import java.util.ArrayList;
import java.util.Date;
@RestController
public class ItemController{

private final AtomicLong counter = new AtomicLong();
private ArrayList<CulturalHeritageItem> items = new ArrayList<>();

@RequestMapping("/get-items")
public ArrayList<CulturalHeritageItem> getItems() {
    return this.items;
  }

@RequestMapping("/get-item")
public CulturalHeritageItem getItem(@RequestParam(value="id", defaultValue="") String id) {
    return findItemById(id);
}

public CulturalHeritageItem findItemById(String id){
	long lid = Long.parseLong(id);
	for(int i=0;i<items.size();i++){
		if(items.get(i).getId()==lid){
			return items.get(i);
		}
	}
	return null;
}

@RequestMapping("/add-item")
public CulturalHeritageItem addItem(	@RequestParam(value="title", defaultValue="") String title,
					@RequestParam(value="description", defaultValue="") String description,
					@RequestParam(value="continent", defaultValue="") String continent,
					@RequestParam(value="city", defaultValue="") String city
					) {
    CulturalHeritageItem item = new CulturalHeritageItem(counter.incrementAndGet(),new User(), title, description, continent, city,new Date());
    this.items.add(item);
    return item;
  }


}
