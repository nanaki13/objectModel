package bon.jo.objects
import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._
import java.time.LocalDate
import All.Dsl.*
import All.Path./
import bon.jo.words.Phrase.ParsingException
class AllTest extends AnyFlatSpec with should.Matchers {

  val myObj = obj{
      "id" := 1
      "name" := "toto"
      "created" := LocalDate.of(2001,1,1)
      "group" := obj("id" := 1,"name" := "groupe_1") 
      "list" := list[String](1,2,3)
    } 

  "A All instance" should "read it's properties" in {
    (myObj / "group" / "name").asValue.value should be ("groupe_1")
  }

  it should "build json stirng" in {
    println(myObj.toJsonString())
    myObj.toJsonString() should be ("""{"id":1,"name":"toto","created":"2001-01-01","group":{"id":1,"name":"groupe_1"},"list":[1,2,3]}""")
  }

  it should "return updated by path" in {
    val updated = myObj.update("group"/"name","Yo !")
    (updated / "group" / "name").asValue.value should be ("Yo !")
  }

  it should "rename/replace keys" in {
    val updated = myObj.replace("group1","group")
    (updated / "group1" / "name").asValue.value should be ("groupe_1")
  }

  it should "added with other" in {
    val updated = myObj + obj("test" := list[String](All.Value(53)))
    (updated / "test" ).asList.value.head.value should be (53)
  }
  it should " test if contains path" in {
  
    myObj.contains("id") should be (true)
    myObj.contains("qsqsd") should be (false)
  }

  it should " delete a path" in {
    val noGrpName = myObj - ("group"/ "name")
    (noGrpName / "group").contains("name") should be (false)
  }

  "a json string" should " convert into object" in {
    val ob = All("""{ "id" : null , "id2" : "null" }""")
    (ob / "id") should be (All.Empty())
  }

  "a malformed json" should "throw ParsingExcpetion if an invalid json" in {

    a [ParsingException] should be thrownBy {
      All("""{ "id" : toto , "id2" : "null" }""")  
    } 
    a [ParsingException] should be thrownBy {
      println(All("""{ """)  )
    } 
    a [ParsingException] should be thrownBy {
      println(All("""{] """)  )
    } 
  }

}
