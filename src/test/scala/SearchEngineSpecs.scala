// src/test/scala/SearchEngineSpecs.scala
package milestoneproject
import httpclient.HttpClient._
import searchengine.SearchEngine._
import milestoneproject.LookItUp._
import org.specs2.specification._
import org.specs2.mutable.Specification

object SearchEngineSpecs extends Specification {
  /*******************************************************
  ** Create data to test with
  *******************************************************/
  // Make some searches and fill them with results
  val GameSearch = Search("League of Legends", Seq(
    Result("Top 5 plays", "The best plays of League of Legends")
  ))

  val VideoSearch = Search("Funny", Seq(
    Result("Top 5 funniest moments", "The funniest moments")
  ))

  val ShoppingSearch = Search("Computer", Seq(
    Result("Top 5 best computers", "The best computers")
  ))

  val RandomSearch = Search("Randomness", Seq(
    Result("Top 5 most random things", "The most random things")
  ))

  val RandomSearchUpdate = Search("Randomness", Seq(
    Result("test", "this is used to check if update works"))
  )

  // Create Users
  val Alisha = new User("Alisha", "StrongPassWord", SearchHistory(Seq(GameSearch, GameSearch)))
  val Sarah = new User("Beth", "MoreSecret", SearchHistory(Seq(RandomSearch)))
  val Rando = new User("Rando", "Password123")
  val Abby = new User("Friend", "HelloWorld1", SearchHistory(Seq(GameSearch, GameSearch)))
  val SarahUpdate = new User("Beth", "NewPassword123", SearchHistory(Seq(ShoppingSearch)))

  // Create UserGroups
  val allUsers = new UserGroup(Seq(Alisha, Sarah, Rando, Abby))
  val emptyGroup = new UserGroup(Seq.empty)

  // Define Get/Post testing data
  val getTestURL = "https://httpbin.org/get"
  val postTestURL = "https://httpbin.org/post"
  val postMap = Map("message" -> "hello", "from" -> "Alisha", "to" -> "world")

  // Create API
  class TestAPI extends API
  val testAPI = new TestAPI

  // Create SearchEngines
  val unpopularSearchEngine = new SearchEngine("Unpopular Engine", new UserGroup(Seq(Rando)))
  val smallSearchEngine = new SearchEngine("Small Engine", new UserGroup(Seq(SarahUpdate)))
  val popularSearchEngine = new SearchEngine("Popular Engine", allUsers)

  // Create LookItUp Engine
  val LookItUp = new LookItUp(new UserGroup(Seq(Rando)))


  /*******************************************************
  ** Specs2 Tests
  *******************************************************/

  // Search Tests
  "\nSearchHistory is a Repository of Searchs that" should {

    "Check if empty" in {
      (!Alisha.searchHistory.isEmpty) && (Rando.searchHistory.isEmpty)
    }
    "Check if history contains a Search" in {
      Alisha.searchHistory.contains(GameSearch)
    }
    "Return a Seq of all Search elements" in {
      Alisha.searchHistory.getAll == Seq(GameSearch, GameSearch)
    }
    "Get a Search at the indicated index" in {
      (Alisha.searchHistory.get(1) == Some(GameSearch)) && (Alisha.searchHistory.get(4) == None)
    }
    step(Alisha.searchHistory.create(RandomSearch))
    "Add a new Search to the history" in {
      Alisha.searchHistory.getAll == Seq(GameSearch, GameSearch, RandomSearch)
    }
    step(Alisha.searchHistory.update(RandomSearchUpdate))
    "Update Searches in the history" in {
      (!Alisha.searchHistory.contains(RandomSearch)) && (Alisha.searchHistory.contains(RandomSearchUpdate))
    }
    step(Alisha.searchHistory.delete(ShoppingSearch))
    "Delete searches from the history" in {
      !Alisha.searchHistory.contains(ShoppingSearch)
    }
  }

  // User Tests
  "\nUser holds an identity and searchHistory and" should {

    "Find the User's most frequent search" in {
      (Rando.mostFrequentSearch === "No Search History") &&
      (Alisha.mostFrequentSearch === "League of Legends")
    }
    "Properly formats a string" in {
      (SarahUpdate.toString == s"Beth's Search History\n${SearchHistory(Seq(ShoppingSearch))}") &&
      (Rando.toString == "Rando's Search History\nEmpty")
    }
  }

  // UserGroup Tests
  "\nUserGroup is a Repository of Users that" should {

    "Check if empty" in {
      (!allUsers.isEmpty) && (emptyGroup.isEmpty)
    }
    "Check if group contains a User" in {
      allUsers.contains(Alisha.name)
    }
    "Return a Seq of all User elements" in {
      allUsers.getAll == Seq(Alisha, Sarah, Rando, Abby)
    }
    "Get a User by their name" in {
      (allUsers.get("Alisha") == Some(Alisha)) && (emptyGroup.get("Alisha") == None)
    }
    step(emptyGroup.create(Sarah))
    "Add a new User to the group" in {
      emptyGroup.getAll == Seq(Sarah)
    }
    step(emptyGroup.update(SarahUpdate))
    "Update User in the group" in {
      (emptyGroup.getAll == Seq(SarahUpdate))
    }
    step(emptyGroup.delete(SarahUpdate))
    "Delete User from the group" in {
      !emptyGroup.contains(SarahUpdate.name)
    }
  }

  // SearchEngine Tests
  "\nSearchEngine holds a UserGroup that" should {

    "Return search history from all users" in {
      smallSearchEngine.engineSearchHistory == Seq(ShoppingSearch)
    }
    "Find the SearchEngine's most frequent search" in {
      (unpopularSearchEngine.mostFrequentSearch === "No Searches Found") &&
      (popularSearchEngine.mostFrequentSearch === "League of Legends")
    }
  }

  // API Tests
  "\nAPI allows use of HTTP Client functions that" should {

    "Successfully make a Get request" in {
      testAPI.executeHttpGet(getTestURL).statusCode == 200
    }
    "Successfully make a Post request" in {
      testAPI.executeHttpPost(postTestURL, postMap).statusCode == 200
    }
  }

  // Look It Up Tests
  "\nLookItUp extends searcha engine with a DuckDuckGo API and" should {

    step(LookItUp.userSearch(Rando.name, "test"))
    "userSearch makes a search on DDG and adds it to the user's history" in {
      !LookItUp.engineSearchHistory.isEmpty
    }
  }
}
