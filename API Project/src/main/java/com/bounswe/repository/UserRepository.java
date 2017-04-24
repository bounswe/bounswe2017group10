package com.bounswe.repository;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;
import com.bounswe.models.User;

@Repository
public interface UserRepository extends CrudRepository<User, Long> {

}
