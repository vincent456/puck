package puck.graph

/**
 * Created by lorilan on 26/10/14.
 */


object SetValueMap {
  def apply[K,V]() = new SetValueMap[K,V](Map())
}
class SetValueMap[K,V](val content : Map[K, Set[V]]){

  def get(key : K) = content get key

  def getFlat(key : K) : Iterable[V]= content.getOrElse(key, Iterable.empty)

  def + (key :K , v : V) : SetValueMap[K,V] = {
    val values = content getOrElse (key, Set[V]())
    new SetValueMap(content + (key -> (values + v)))
  }

  def - (key : K,  v: V) : SetValueMap[K,V] = {
    val values = content getOrElse (key, Set[V]())
    val newValues = values - v

    if(newValues.isEmpty)
      new SetValueMap(content - key)
    else
      new SetValueMap(content + (key -> newValues))

  }

  def bind( key : K, v: V) : Boolean = {
    (content get key) exists { _ contains v}
  }

  def toSeq = content.toSeq

}
