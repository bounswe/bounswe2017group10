package com.bounswe.controllers;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.Date;

import com.bounswe.models.CulturalHeritage;
import com.bounswe.models.User;
import com.bounswe.services.UserService;
import com.bounswe.services.CulturalHeritageService;

@RestController
public class CulturalHeritagesController {
  private CulturalHeritageService culturalHeritageService;
  private UserService userService;

  @Autowired
  public CulturalHeritagesController(CulturalHeritageService culturalHeritageService, UserService userService) {
    this.culturalHeritageService = culturalHeritageService;
    this.userService = userService;
  }

  @RequestMapping("/get-cultural-heritages")
  public ArrayList<CulturalHeritage> getCulturalHeritages() {
    return this.culturalHeritageService.findAll();
  }

  @RequestMapping("users/{userId}/add-cultural-heritage")
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


  @RequestMapping("/delete-item/{itemId}")
    public CulturalHeritage deleteItem(@PathVariable(value = "itemId") final Long itemId) {


	try {
      CulturalHeritage deleteditem = this.culturalHeritageService.findOne(itemId);
		ListIterator<CulturalHeritage> itemIt = items.listIterator();

        while(itemIt.hasNext()){
            CulturalHeritage item = itemIt.next();

            if(item.getId() == deletedItem.getId()){
                itemIt.remove();
                return item;
            }
        }

        return item;
       } catch (Exception e) {
            e.printStackTrace();
            return null;
       }
        
       
   }


}


