package bon.jo.home

  enum Color:
    case RGB(r: Int, g: Int, b: Int)
    case HSL(h: Double, s: Double, l: Double)
    case Raw(cssColorDef : String)
    override def toString(): String =
      this match
        case RGB(r, g, b) => s"rgb($r,$g,$b)"
        case HSL(h, s, l) => s"hsl(${h} ${s}% ${l}%)"
        case Raw(c) => c

  
