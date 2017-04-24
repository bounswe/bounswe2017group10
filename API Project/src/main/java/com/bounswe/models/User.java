package com.bounswe.models;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Column;

@Entity
public class User {
  @Id
  @GeneratedValue
  private Long id;
  private String firstName;
  private String lastName;
  private String email;
  private String userName;
  private String password;

  public User(String firstName, String lastName) {
    this.firstName = firstName;
    this.lastName = lastName;
  }
  
  /* 
  Initializes the user if passwords match, writes "Passwords should match" otherwise.
  Initializes a user with its first name, last name, email, user name and password. 
  Confirm stands for password confirmation, user should enter its password twice, one for password one for confirm password, 
  and both passwords shall match to initialize a user. 
  */
  public User(String firstName, String lastName, String email, String userName, String password, String confirm){
    if(password.equals(confirm)){
      this.firstName = firstName;
      this.lastName = lastName;
      this.email = email;
      this.userName = userName;
      this.password = password;
    }
    else{
      System.out.println("Passwords should match.");
    }
  }
  
  /*
  Setter methods for the user fields are being used to make updates in user informations
  User can update its name(first name, last name, user name), its email address and its password.
  */
  public void setFirstName(String firstName){
    this.firstName = firstName;
  }
  public void setLastName(String lastName){
    this.lastName = lastName;
  }
  public void setEmail (String email){
    this.email = email;
  }
  public void setUserName (String userName){
    this.userName = userName;
  }
  public void setPassword (String password){
    this.password = password;
  }
  
  /*
  Get methods for needed user fields
  */
  public String getEmail(){
    return email;
  }

  public User(){
    this.firstName = "default";
    this.lastName = "default";
  }

  public String getName() {
    return this.firstName +" "+ this.lastName;
  }
  
  public String getPassword(){
	  return this.password;
  }
  
 
  
  
}
