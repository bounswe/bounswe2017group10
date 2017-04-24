package com.bounswe.repository;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;
import com.bounswe.models.CulturalHeritage;

@Repository
public interface CulturalHeritageRepository extends CrudRepository<CulturalHeritage, Long> {

}

