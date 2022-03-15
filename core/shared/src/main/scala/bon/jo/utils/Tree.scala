package bon.jo.utils


sealed trait Tree[+T]:
  def parent:Tree.Node[T]
  def map[B](m : T => B):Tree[B]
  def mapNode[B>: T ](m : Tree.Node[T] => Tree[B]):Tree[B]
  def applyToChilds[B](f : List[T] => List[B]) : Tree[B]
object Tree:
  object RootNode extends Tree.Node[Nothing]:
    def parent:Node[Nothing] = ???
    def childs = Nil
    def add[B](v : Tree[B]):Node[B] = Tree.NodeImpl(List(v))
    def copy[B >: Nothing](childs : List[Tree[B]]):Node[B]=RootNode
  val root : Node[Nothing] = RootNode
  object EndTree:
    def apply[T](values : List[T]):Tree.EndTree[T] = EndTreeImpl(values)
    def apply[T](values : List[T],parent : Node[T]):Tree.EndTree[T] = EndTreeImpl(values,parent)
    def unapply[T](v:Tree[T]):Option[List[T]] = 
      v match
        case r : EndTree[T] => Some(r.values)
        case _ => None
  trait EndTree[+T] extends Tree[T]:
    def values: List[T]
    def add[B >: T](v : B):Tree[B]
    def toNode():Node[T] = Tree.NodeImpl[T](List(this),parent)
    def copy[B >: T ](values : List[B]):EndTree[B]
    def mapNode[B >: T](m : Node[T] => Tree[B]):EndTree[B] = this
    def +[B >: T](o : EndTree[B]): EndTree[B] = Tree.EndTreeImpl(values++o.values)
    def map[B](m : T => B):EndTree[B] = Tree.EndTreeImpl(values.map(m))
    def applyToChilds[B](f : List[T] => List[B]) :EndTree[B] =Tree.EndTreeImpl(f(values))
  object Node:
     def apply[T](values : List[Tree[T]]):Tree.Node[T] = NodeImpl(values)
     def apply[T](values : List[Tree[T]],parent : Node[T]):Tree.Node[T] = NodeImpl(values,parent)
     def unapply[T](v:Tree[T]):Option[List[Tree[T]]] = 
        v match
          case r : Node[T] => Some(r.childs)
          case _ => None
  trait Node[+T] extends Tree[T]:
    def childs: List[Tree[T]]
    def copy[B>: T](childs : List[Tree[B]]):Node[B]
    def add[B >: T](v : Tree[B]):Node[B]
    def mapNode[B>: T](m : Node[T] =>Tree[B]):Tree[B] = 
      val newNode = m(this)
      if(newNode != this) then
        newNode
      else
        this.copy(childs.map{
          case e : EndTree[T] => e
          case n : Node[T] => n.mapNode[B](m)
        })
    def applyToChilds[B](f : List[T] => List[B]) :Node[B] =  Tree.NodeImpl(childs.map(child => child.applyToChilds(f)))
    def map[B](m : T => B):Node[B] = Tree.NodeImpl(childs.map(child => child.map(m)))
    

  case class NodeImpl[T](childs: List[Tree[T]],parent : Node[T] = root) extends Node[T] :
    def add[B >: T](v : Tree[B]):Node[B]= copy(childs = childs :+ v)
    def copy[B >: T](childs : List[Tree[B]]):Node[B] = this.copy(childs,parent)
    override def toString() =  "Node("+childs.mkString(", ")+")"
  case class EndTreeImpl[T](values : List[T],parent : Node[T] = root) extends EndTree[T]:
    override def toString() = "EndTree("+values.mkString(", ")+")"
    def add[B >: T](v: B): Tree[B] = copy(values = values :+ v)
    def copy[B >: T](values : List[B]):EndTree[B] = this.copy(values,parent)