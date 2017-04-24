package com.bounswe.services;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import java.util.ArrayList;
import java.util.Iterator;
import java.lang.Iterable;
import com.bounswe.repository.CulturalHeritageRepository;
import com.bounswe.models.CulturalHeritage;

@Service
public class CulturalHeritageService {
// Methods of the repository can be found here:
// https://docs.spring.io/spring-data/commons/docs/current/api/org/springframework/data/repository/CrudRepository.html
  private CulturalHeritageRepository culturalHeritageRepository;

  @Autowired
  public CulturalHeritageService(CulturalHeritageRepository culturalHeritageRepository) {
    this.culturalHeritageRepository = culturalHeritageRepository;
  }

  public Long getCount() {
    return this.culturalHeritageRepository.count();
  }

  public void save(CulturalHeritage culturalHeritage) {
    this.culturalHeritageRepository.save(culturalHeritage);
  }

  public ArrayList<CulturalHeritage> findAll() {
    Iterator<CulturalHeritage> it = this.culturalHeritageRepository.findAll().iterator();
    ArrayList<CulturalHeritage> culturalHeritages = new ArrayList<>();

    // There are literally no faster way: http://stackoverflow.com/a/10117051/3138171
    while(it.hasNext()) culturalHeritages.add(it.next());
    return culturalHeritages;
  }

  public void delete(CulturalHeritage culturalHeritage){
	this.culturalHeritageRepository.delete(culturalHeritage);
 }

}

