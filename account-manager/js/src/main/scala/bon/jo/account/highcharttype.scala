package bon.jo.account

package highcharttype:
  import scalajs.js
  import scalajs.js.UndefOr
  trait WithType  extends js.Object:
    var `type`:String 
  trait CanEnabel  extends js.Object:
    var enabled : UndefOr[Boolean] 
  trait WithTitle  extends js.Object:
    var title : Text
  trait Text extends js.Object : 
    var text:String 
  trait ChartOption  extends js.Object with WithTitle: 
    var chart : Chart
    var credits : CanEnabel
    var subtitle : Text
    var xAxis : XAxis
    var yAxis : YAxis
    var legend : CanEnabel
    var tooltip : UndefOr[Tooltip]
    var series : js.Array[Series]
  trait AxisLabel extends js.Object with CanEnabel:
    var align:UndefOr[String]
    var allowOverlap:Boolean
    var autoRotation:UndefOr[js.Array[Double]]
    var autoRotationLimit:Double
    var distance:Double

    var format:String
   // var formatter:undefined
    var maxStaggerLines:Double
    var overflow:String
    var padding:Double
    var position3d:String
    var reserveSpace:Boolean
    var rotation:Double
    var skew3d:Boolean
    var staggerLines:Double
    var step:Double
    var style:Style
    var useHTML:false
    var x:Double
    var y:Double
    var zIndex:Double
  trait Chart  extends js.Object with  WithType
  trait XAxis  extends js.Object with  WithType:
    var labels : AxisLabel
  trait YAxis  extends js.Object with  WithTitle :
    var labels : AxisLabel
  trait Tooltip  extends js.Object :
    var pointFormat : UndefOr[String]
  trait Series extends js.Object :
    var name: js.UndefOr[String]
    var data: js.Array[SeriesData]
    var dataLabels: DataLabels
  trait SeriesData extends js.Object :
      var name: js.UndefOr[String]
      var y: js.UndefOr[Double]
      var sliced: js.UndefOr[Boolean]
      var selected: js.UndefOr[Boolean]
  object SeriesData:
    def apply(name : String,y : Double):SeriesData = 
      js.Dynamic.literal(name = name,y=y).asInstanceOf[SeriesData]
  trait DataLabels extends js.Object:
    var enabled : UndefOr[Boolean]
    var rotation: UndefOr[String]
    var color: UndefOr[String]
    var align: UndefOr[String]
    var format: UndefOr[String]
    var y: UndefOr[Double]
    var style : Style
  trait  Style extends js.Object: 
       var fontSize: UndefOr[String]
       var fontFamily: UndefOr[String]
      
  object ChartOption:
    def apply():ChartOption = js.Dynamic.literal().asInstanceOf[ChartOption]


