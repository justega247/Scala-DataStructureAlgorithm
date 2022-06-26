package chap7

class HashTableMutableImpl[K, V](size: Int) extends HashTable[K, V] {
  private val myHashArray = Array.fill(size)(List[(K, V)]())

  def hashCode(myKey: K): Int = {
    val tempHashCode = myKey.## % size
    if (tempHashCode < 0) tempHashCode + size else
      tempHashCode
  }

  override def insert(myKey: K, myValue: V): Unit = {
    val myList = myHashArray(hashCode(myKey))
    myHashArray(hashCode(myKey)) = (myKey, myValue) +: myList.filter(x => x._1 != myKey)
  }

  override def search(myKey: K): Option[V] = {
    val myList = myHashArray(hashCode(myKey))
    myList.find(x => x._1 == myKey).map(y => y._2)
  }

  override def delete(myKey: K): Option[V] = {
    val myList = myHashArray(hashCode(myKey))
    myHashArray(hashCode(myKey)) = myList.filter(x => x._1 != myKey)
    myList.find(x => x._1 == myKey).map(y => y._2)
  }

}

trait HashTable[Key, Value] {
  def insert(myKey: Key, myValue: Value): Unit
  def search(myKey: Key): Option[Value]
  def delete(myKey: Key): Option[Value]
}

object HashTableMutableApp {
  def main(args: Array[String]): Unit = {
    val myHashTable: HashTable[Int, String] = new HashTableMutableImpl[Int, String](17)

    myHashTable.insert(123456789, "Martin")
    myHashTable.insert(987654321, "James")
    myHashTable.insert(123454321, "Brian")
    myHashTable.insert(432112345, "Einstein")
    myHashTable.insert(776612345, "Richie")
    println(s" Martin search, ${myHashTable
      .search(123456789)}")
    println(s"James search, ${myHashTable
      .search(987654321)}")
    println(s"Brian search, ${myHashTable
      .search(123454321)}")
    println(s"Einstein search ${myHashTable
      .search(432112345)}")
    println(s"Richie search, ${myHashTable
      .search(776612345)}")
    println(s"Richie delete, ${myHashTable
      .delete(776612345)}")
    println(s"Non-existing delete, ${myHashTable
      .delete(886612345)}")
    println(s"Richie search, ${myHashTable
      .search(776612345)}")
  }
}
