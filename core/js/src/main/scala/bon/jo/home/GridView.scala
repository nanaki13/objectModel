package bon.jo.home

import org.scalajs.dom.HTMLElement
import org.scalajs.dom.console
import scalajs.js.JSON
import bon.jo.Draw.Grid
import bon.jo.Draw.toAllObj
import org.scalajs.dom.HTMLCanvasElement
import bon.jo.MiniDsl.*
import bon.jo.HtmlPredef.*
import bon.jo.HtmlEvent.events
import org.scalajs.dom.PointerEvent
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.CanvasRenderingContext2D
import bon.jo.Draw.GridValue
import bon.jo.Draw.toGridValueExport
import bon.jo.objects.All.toJsonString
import java.util.Base64
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLDivElement
import org.scalajs.dom.FileReader
import bon.jo.objects.All
import bon.jo.Draw.GridValueExport
import bon.jo.Draw.Positioned
import bon.jo.home.GridViewContext
import bon.jo.home.GridViewContext.*
import bon.jo.home.Color
import bon.jo.home.ProcessEvent.NoActionParam
import bon.jo.HtmlEvent.EventAdder
import bon.jo.Draw.EmptyGridElement
import org.scalajs.dom.TouchList
import org.scalajs.dom.TouchEvent
import bon.jo.Draw.MasterGrid
import bon.jo.Draw.Access
import bon.jo.Draw.AccessVar
import bon.jo.Draw.Moving
import bon.jo.Draw.FrameGrid
import bon.jo.Lvw
import org.scalajs.dom.window
import org.scalajs.dom.URLSearchParams
import scalajs.js.isUndefined
import scala.scalajs.js.URIUtils
import bon.jo.home.ProcessEvent.ActionParam
object GridView extends GridViewOps:

  type Context = GridViewContext
  sealed trait Sel
  case class RectSelect(xIni: Int, yIni: Int, xEnd: Int, yEnd: Int) extends Sel


  def resetFromJsonDataString(res : String): OnBaseDrawFactor[Unit] = 
    val resParsed = JSON.parse(res)
    val din = resParsed.asInstanceOf[scalajs.js.Dynamic]
    val xSize : Int = din.xSize.asInstanceOf
    val ySize : Int = din.ySize.asInstanceOf
    val data : scalajs.js.Array[scalajs.js.Dynamic] = din.data.asInstanceOf
    val sheets : scalajs.js.Array[scalajs.js.Dynamic] =din.sheets.asInstanceOf
    
    val dataS: List[GridValueExport[String]] = data.map{ e =>
      GridValueExport(e.v.asInstanceOf,e.xy.asInstanceOf) 
    }.toList
    resetData(dataS,xSize,ySize)     
    sheets.foreach{
      sheet =>  
        val xSizeSheet : Int = sheet.xSize.asInstanceOf
        val ySizeSheet : Int = sheet.ySize.asInstanceOf
        val xSheet : Int = sheet.x.asInstanceOf
        val ySheet : Int = sheet.y.asInstanceOf
        val movment : String = sheet.m.asInstanceOf

        val gridSheet = 
          if !scalajs.js.isUndefined(sheet.data) then
            val dataSheet : scalajs.js.Array[scalajs.js.Dynamic] = sheet.data.asInstanceOf     
            val dataSheetExport: List[GridValueExport[String]] = dataSheet.map{ e =>
                GridValueExport(e.v.asInstanceOf,e.xy.asInstanceOf) 
              }.toList
            val g = Grid[String](xSizeSheet,ySizeSheet)
            g.resetData(dataSheetExport)
            g
          else 
            val fg = FrameGrid[String](xSizeSheet,ySizeSheet)
            val frames : scalajs.js.Array[scalajs.js.Array[scalajs.js.Dynamic]] = sheet.frames.asInstanceOf  
            frames.zipWithIndex.map((dataSheet,i) => {
              val dataSheetExport: List[GridValueExport[String]] = dataSheet.map{ e =>
                GridValueExport(e.v.asInstanceOf,e.xy.asInstanceOf) 
              }.toList
              if(i != 0) then
                fg.addFrame()
              fg.currentFrame = i
              fg.resetData(dataSheetExport)

            })  

            fg
        val nSheet = new Positioned(xSheet,ySheet,gridSheet) with Access with AccessVar with Moving[String]
        nSheet.moveString(movment,grid.xSize,grid.ySize)
        addSheetToGrid(nSheet)
        tContext match
          case given GridViewContext => addSheetUI(nSheet)
          case _ =>
        
        
    }
    resierCanvasAndDraw(xSize,ySize)

  def showView(data : String): ShowGridContext =
    val myCanvas: HTMLCanvasElement = canvas
    given ctx : ShowGridContext = ShowGridContext(MasterGrid(10,10),20,myCanvas,Color.RGB(200,10,0),div(_class("row"),childs(myCanvas))  )
    resetFromJsonDataString(data)
    var interval = 0
    var count = 0
    interval = org.scalajs.dom.window.setInterval(() => 
      ctx.grid.sheet.foreach{ v =>
        v.v match 
          case e : FrameGrid[_] => e.nextFrame()
          case o =>
        v.move(count)
      }   
      draw()
      count = count+1
      ,100)
    ctx

  def view(): Context =
    
    inline val cntColorStep = 500
    inline val colorNum = 255 * 255 * 255
    inline val colrStep = 360d / 500d

    val gridX = 40
    val gridY = 40
   
    val fact = 10
    val myCanvas: HTMLCanvasElement = canvas
    val root = div(_class("row"))
    given Context = new Context(
       MasterGrid[String](gridX, gridY),
       root,
      myCanvas,
      tillColor(),
      div,
      fact,
      Color.RGB(0, 0, 0),
      DrawProcessEvent,
      NoActionParam,Nil
    )
    val cWidth = gridX * fact
    val cHeight = gridY * fact
    var mousedown = false
    
    def arc: Seq[(Double)] = (for {
      c <- 0 until cntColorStep

    } yield c) map (_ * colrStep)

    def colorLine() = div(
      childs(
        arc
          .map { (c) =>

            val s = s"hsl(${c} 100% 50%)"

            s
          }
          .map { cssClor =>
            val spanc = span
            spanc.style.backgroundColor = cssClor
            spanc.style.width = "1px"
            spanc.style.height = "15px"
            spanc.style.display = "inline-block"
            spanc
          }
      )
    )

    def colorLineCanvas(iniH : Double,
        posConsumer: Double => Unit
    ): (VarValueDouble, HTMLElement) =
      val varval = VarValue(iniH)
      val cursor = div(_class("cursor")) 
      cursor.style.top = "2px"
      cursor.style.left = s"${(iniH/colrStep)-5}px"
      cursor.style.pointerEvents = "none"
      val canvasp = canvas(me(can =>
        can.width = cntColorStep
        can.height = 15
        import bon.jo.HtmlEvent.onmousedownandmove

        can.onmousedownandmove { e =>
          
          given HTMLElement = can
          val xx = xme(e)
          val nH = xx * colrStep
          cursor.style.left = s"${xx-5}px"
          varval.value = nH
          posConsumer(nH)

        }
        val context =
          can.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

        arc
          .map { (c) =>

            val s = s"hsl(${c} 100% 50%)"

            s
          }
          .zipWithIndex
          .foreach { (cssClor, i) =>
            context.beginPath()
            context.fillStyle = cssClor
            context.rect(i, 0, 1, 15)
            context.fill()
            context.closePath()
          }
      ))
      val ret = div(childs(canvasp,cursor))
      ret.classList.add("relative-inline-block")

      (varval, ret)





    val wInput = input(me(_.value = "40"))
    val hInput = input(me(_.value = "40"))
    val whDiv = div(childs(span(_text("width : ")),wInput,span(_text("height : ")),hInput))
    val pxSizeInput= input(me(_.value = "10"))
    
    val pxSizeDiv = div(childs(span(_text("px size : ")),pxSizeInput))
    
    def changeGridSize():Unit = 
      updateGrid(wInput.value.toInt,hInput.value.toInt)
    wInput.events.change( _ => changeGridSize() )
    hInput.events.change( _ => changeGridSize())
    def changePixelSize():Unit = 
      updateFacor(pxSizeInput.value.toInt)
    pxSizeInput.events.change( _ => changePixelSize() )

    val saveColorDiv = div(_class("color-saved"))
    val buttonSaveColor= button(_text("save color"), click(_ => saveColor(saveColorDiv)))
    
    val buttonColorFromDraw =
      button(_text("get all colors"), click(_ => colorFromDraw(saveColorDiv)))
    val rPicker = input(me(_.style.width = "30px"))
    val gPicker = input(me(_.style.width = "30px"))
    val bPicker = input(me(_.style.width = "30px"))
    val colPi = div(childs(context.colorPicker,buttonSaveColor,buttonColorFromDraw,saveColorDiv, div(childs(rPicker, gPicker, bPicker))))


    def colorRGBChage(): OnContextUnit =
      updateColor(
        Color.RGB(rPicker.value.toInt, gPicker.value.toInt, bPicker.value.toInt)
      )

    rPicker.onchange = { _ =>
      colorRGBChage()
    }
    gPicker.onchange = { _ =>
      colorRGBChage()
    }
    bPicker.onchange = { _ =>
      colorRGBChage()
    }
    prepare(myCanvas, cWidth, cHeight)

    given (String => HTMLElement) = i => div(_text(i.toString))
    given PaletteContext = PaletteContext("tool-select", "row", "cell")
    val palette = Palette(2,true, "Draw", "Select","Paste","Eraser")
    val paletteOption = div
 
    palette.listen = {
      case "Draw" =>
        paletteOption.innerHTML=""
        context.actionParam = DrawParam.Pixel
        paletteOption.append( DrawParamSelectionView{
          param => context.actionParam = param
        })
        context.currentProcess = DrawProcessEvent
      case "Select" =>
        paletteOption.innerHTML=""
        context.currentProcess = SelectProcessEvent
      case "Paste" =>
        paletteOption.innerHTML=""
        context.currentProcess = PasteProcessEvent
      case "Eraser" =>
        paletteOption.innerHTML=""
        context.actionParam = NoActionParam
        context.currentProcess = EraseProcessEvent
    }
    palette.select(0)
    
    val parentCanvas = div(_class("parent-c"), childs(myCanvas))
    parentCanvas.onmousemove = e =>
      if mousedown then context.currentProcess.process.tupled(xyInGrid(e))

    EventAdder.touchmove[TouchEvent]( e =>
      if mousedown then context.currentProcess.process.tupled(xyInGrid(e))
    )(using parentCanvas)
    parentCanvas.onmousedown = e =>
      mousedown = true
      context.currentProcess.start.tupled(xyInGrid(e))
    EventAdder.touchstart[TouchEvent]( e =>
      mousedown = true
      context.currentProcess.start.tupled(xyInGrid(e))
    )(using parentCanvas)
    EventAdder.touchend[TouchEvent]( e =>
      if mousedown then
        mousedown = false
        context.currentProcess.end.tupled(xyInGrid(e))
    )(using org.scalajs.dom.document.body)
     EventAdder.mouseup[MouseEvent](e =>
      if mousedown then
        mousedown = false
        context.currentProcess.end.tupled(xyInGrid(e)))(using org.scalajs.dom.document.body)
    EventAdder.click[MouseEvent](e => context.currentProcess.process.tupled(xyInGrid(e)))(using parentCanvas)
    val buttonDeleteSel = button(_text("Delete selections"), click(_ => deleteSele()))
    val buttonRemoveAllSel =
      button(_text("Clear selections"), click(_ => clearSele()))
    val buttonCopySel =
      button(_text("Copy selections"), click(_ => copySel()))
    val buttonApplyColToSelElements =
      button(_text("Apply color"), click(_ => applyCuurentColorToSel()))
    val strokeButton =
      button(_text("Stroke selection"), click(_ => strokeSel()))
    val fillButton =
      button(_text("Fill selection"), click(_ => fillSel()))
     val createSheetButton =
      button(_text("Sheet from selection"), click(_ => sheetFromSel()))
    def selectionPalette():Palette[HTMLElement] =
      
      given PaletteContext = PaletteContext("select-action-select", "row", "cell-large")
      Palette(4,false,buttonDeleteSel,buttonCopySel,buttonApplyColToSelElements, buttonRemoveAllSel, strokeButton, fillButton,createSheetButton)

    val animButton = input(me(_.`type` = "checkbox"))
    var interval = 0
    animButton.events.change(e => 
      if animButton.checked then
        var count = 0
        interval = org.scalajs.dom.window.setInterval(() => 
          context.grid.sheet.foreach{ v =>
            v.v match 
              case e : FrameGrid[_] => e.nextFrame()
              case o =>
            v.move(count)
          }
          context.sheetsMv.foreach(_.redraw())
          draw()
          count = count+1
          ,100)
      else 
        org.scalajs.dom.window.clearInterval(interval)
    )
   
    val toolDiv = div(childs(        whDiv,
        pxSizeDiv,
        div(childs(selectionPalette().root)),
        div(childs(palette.root,paletteOption)),
        colPi,context.sheetViewsDiv,div(_text("anim : "),childs(animButton))))
   

      root.append(
        parentCanvas,toolDiv
      )
    

    def save(): OnHTMLElement =
      val s = context.grid.json().toJsonString()
      val ss = Lvw(s).toStringData
      val aLink = a(
        _text("Download json"),
        me(_.asInstanceOf[scalajs.js.Dynamic].download = "image.json"),
        me(_.href = s"""data:application/json;base64,${Base64.getEncoder
          .encodeToString(s.getBytes)}"""))
      val aLinkData = a(
        _text("Download data"),
        me(_.asInstanceOf[scalajs.js.Dynamic].download = "image.data"),
        me(_.href = s"""data:text/plain;base64,${Base64.getEncoder
          .encodeToString(ss.getBytes)}"""))
      OnHtml().append(div(childs(div(childs(aLink)),div(childs(aLinkData)))))
    def shareLink(): OnHTMLElement =
      val s = context.grid.json().toJsonString()
      val ss = Lvw(s).toStringData
      val aLink = a(_text("link"),
        me( _.href=window.location.host+window.location.pathname+"?q="+URIUtils.encodeURI(ss)))

      OnHtml().append(div(childs(div(childs(aLink)))))

    val buttonSave = button(_text("save"), click(save()(using root)))
    val shareLinkButton = button(_text("share link"), click(shareLink()(using root)))
    val i = input(
      me(_.`type` = "file"),
      me(_.name = "img-json"),
      me(_.id = "img-json"),
      me(_.style.display = "none")
    )
    i.onchange = e => {
      val f: FileReader = new FileReader()
      val tRef = (System.currentTimeMillis)
      f.onloadend = l =>
      
        val Lvw(res) = f.result.toString
        resetFromJsonDataString(res)
        
      
      f.readAsText(i.files(0), "utf-8")
    }
    inline def open(): HTMLDivElement = div(
      childs(
        button(childs(label(_text("Open"), me(_.htmlFor = "img-json")), i))
      )
    )



    lazy val (vRed: VarValueDouble, cRed) = Cursor(0.2,
      f =>
        updateColor(
          Color.RGB(
            (f * 255).round.toInt,
            (vGreen.value * 255).round.toInt,
            (vBlue.value * 255).round.toInt
          )
        ),
      root
    )
    lazy val (vGreen: VarValueDouble, cGreen) = Cursor(0.5,
      f =>
        updateColor(
          Color.RGB(
            (vRed.value * 255).round.toInt,
            (f * 255).round.toInt,
            (vBlue.value * 255).round.toInt
          )
        ),
      root
    )
    lazy val (vBlue: VarValueDouble, cBlues) = Cursor(0.7,
      f =>
        updateColor(
          Color.RGB(
            (vRed.value * 255).round.toInt,
            (vGreen.value * 255).round.toInt,
            (f * 255).round.toInt
          )
        ),
      root
    )

    lazy val (vS: VarValueDouble, cS) = Cursor(1,
      s => updateColor(Color.HSL(vH.value, s * 100, vL.value * 100)),
      root
    )
    lazy val (vL: VarValueDouble, cL) = Cursor(0.5,
      l => updateColor(Color.HSL(vH.value, vS.value * 100, l * 100)),
      root
    )
    lazy val (vH: VarValueDouble, cH) = colorLineCanvas(120,h =>
      updateColor(Color.HSL(h, vS.value * 100, vL.value * 100))
    )
    toolDiv.append(
      div(childs(span(_text("R : ")),cRed)),
      div(childs(span(_text("G : ")),cGreen)),
      div(childs(span(_text("B : ")),cBlues)),
      div(childs(span(_text("H : ")),cH)),
      div(childs(span(_text("S : ")),cS)),
      div(childs(span(_text("L : ")),cL)),
      div(childs(buttonSave,shareLinkButton)),
      open()
    )
    updateColor(Color.HSL(vH.value, vS.value * 100, vL.value * 100))   
    context
