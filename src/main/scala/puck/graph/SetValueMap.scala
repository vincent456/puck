package puck.graph


object SetValueMap {
  def apply[K,V]() = new SetValueMap[K,V](Map())
}
class SetValueMap[K,V](val content : Map[K, Set[V]]){

  override def toString = "SetValueMap" + content.mkString("(", ",\n", ")")

  def get(key : K) = content get key

  def getFlat(key : K) : Set[V]= content.getOrElse(key, Set.empty)

  def + (key :K , v : V) : SetValueMap[K,V] = {
    val values = content getOrElse (key, Set[V]())
    new SetValueMap(content + (key -> (values + v)))
  }

  def - (key : K) : SetValueMap[K,V] =
    new SetValueMap(content - key)

  def - (key : K,  v: V) : SetValueMap[K,V] = {
    val values = content getOrElse (key, Set[V]())
    val newValues = values - v

    if(newValues.isEmpty)
       this - key
    else
      new SetValueMap(content + (key -> newValues))

  }

  def mapValues[W](f : V => W) =
    new SetValueMap( content mapValues ( s => s map f ) )

  def bind( key : K, v: V) : Boolean =
    (content get key) exists { _ contains v}

  def toSeq = content.toSeq
  def toList = content.toList

  def flatList : List[(K,V)]=
    for{
      s <- content.toList
      (k, vs) = s
      v <- vs.toList
    } yield (k, v)

}
