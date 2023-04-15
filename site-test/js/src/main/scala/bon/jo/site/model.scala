package bon.jo.site
import scalajs.js
package model:
  trait Page extends js.Object:
    var title : String
    var id : String
  trait Menu extends js.Object:
    var pages : js.Array[Page]
  trait PageContent extends js.Object:
    var content: String
    var pageId : String
