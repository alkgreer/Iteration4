// src/main/scala/milestoneproject/LookItUpMockRun.scala
import milestoneproject.LookItUp._
import searchengine.SearchEngine._

object LookItUpMockRun {

  def main(args: Array[String]) {

    // Make some searches and fill them with results
    val GameSearch = Search("League of Legends", Seq(
      Result("Top 5 plays", "The best plays of League of Legends"),
      Result("Top 5 Champions", "The best champions of League of Legends"),
      Result("Builds", "Champion build guides for League of Legends")
    ))

    val VideoSearch = Search("Funny", Seq(
      Result("Top 5 funniest moments", "The funniest moments"),
      Result("Top 5 funniest things", "The funniest things"),
      Result("Funny Comedians", "Comedians whom are funny")
    ))

    val ShoppingSearch = Search("Computer", Seq(
      Result("Top 5 best computers", "The best computers"),
      Result("Top 5 most bought computers", "The most bought computers"),
      Result("How-to-build computers", "Build guides for computers")
    ))

    val RandomSearch = Search("Randomness", Seq(
      Result("Top 5 most random things", "The most random things"),
      Result("Top 5 most random incidents", "The most random incidents")
    ))


    val FinalSearch = Search("Finals", Seq(
      Result("Top 5 final questions", "The most occurying final questions"),
      Result("Top 5 hardest finals", "The hardest taken finals"),
      Result("How-to-pass a final", "Passing guides for finals")
    ))

    val badSearch = Search("lkdflsdnf", Nil)

    //Create users
    val Alisha = new User("Alisha", "StrongPassWord", SearchHistory(Seq(GameSearch, VideoSearch)))
    val Kate = new User("BestFriend", "SecretPass", SearchHistory(Seq(GameSearch, ShoppingSearch, FinalSearch)))
    val Marcus = new User("Marcus", "TypicalPass", SearchHistory(Seq(GameSearch, VideoSearch)))
    val Sarah = new User("Beth", "MoreSecret", SearchHistory(Seq(RandomSearch)))
    val Abby = new User("Friend", "HelloWorld1", SearchHistory(Seq(RandomSearch, GameSearch)))
    val Rando = new User("Rando", "Password123")
    val allUsers = new UserGroup(Seq(Alisha, Kate, Marcus, Sarah, Abby, Rando))

    // Create LookItUp SearchEngine
    val LookItUp = new LookItUp(allUsers)

    /******************************************************
    **                TEST MOCK DATA
    ******************************************************/

    // Print out info on all the users
    // for (user <- LookItUp.users.getAll) println(user)

    // Find each user's most frequent search
    // for (user <- LookItUp.userGroup.getAll) println(s"${user.name}'s most frequent search: ${user.mostFrequentSearch}")

    // Find the most frequent search on the engine
    // println(s"The most frequent search on this engine: ${LookItUp.mostFrequentSearch}")

    // Make a search
    println(Rando)
    LookItUp.userSearch(Rando.name, "testing")
    println(Rando)
  }
}
