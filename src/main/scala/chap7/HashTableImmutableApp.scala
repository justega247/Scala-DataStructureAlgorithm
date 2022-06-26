package chap7

protected class HashTableImmutableImpl[K, V](myHashVector: Vector[List[(K, V)]]) extends HashTableImmutable[K, V] {
  private val size = myHashVector.size

  def hashCode(myKey: K): Int = {
    val tempHashCode = myKey.## % size
    if (tempHashCode < 0) tempHashCode + size else
      tempHashCode
  }

  override def insert(myKey: K, myValue: V): HashTableImmutable[K, V] = {
    val insertionIndex = hashCode(myKey)
    val insertionList = myHashVector(insertionIndex)
    val newList = (myKey, myValue) +: insertionList.filter(_._1 != myKey)

    new HashTableImmutableImpl[K, V](myHashVector.updated(insertionIndex, newList))
  }

  override def search(myKey: K): Option[V] = {
    val myList = myHashVector(hashCode(myKey))
    myList.find(x => x._1 == myKey).map(y => y._2)
  }

  override def delete(myKey: K): HashTableImmutable[K, V] = {
    val deletionIndex = hashCode(myKey)
    val deletionList = myHashVector(deletionIndex)
    val newList = deletionList.filter(_._1 != myKey)

    new HashTableImmutableImpl[K, V](myHashVector.updated(deletionIndex, newList))
  }
}

object HashTableImmutableImpl {
  def apply[K, V](size: Int): HashTableImmutableImpl[K, V] = {
    val myHashVector = Vector.fill(size)(List())

    new HashTableImmutableImpl[K, V](myHashVector)
  }
}

trait HashTableImmutable[K, V] {
  def insert(myKey: K, myValue: V): HashTableImmutable[K, V]
  def search(myKey: K): Option[V]
  def delete(myKey: K): HashTableImmutable[K, V]
}

object HashTableImmutableApp {
  def main(args: Array[String]): Unit = {
    val myHashTable: HashTableImmutable[Int, String] =
      HashTableImmutableImpl(17)

    val filledTable = myHashTable.insert(123456789, "Martin")
      .insert(987654321, "James")
      .insert(123454321, "Brian")
      .insert(432112345, "Einstein")
      .insert(776612345, "Richie")
    println(s" Martin search, ${filledTable
      .search(123456789)}")
    println(s"James search, ${filledTable
      .search(987654321)}")
    println(s"Brian search, ${filledTable
      .search(123454321)}")
    println(s"Einstein search ${filledTable
      .search(432112345)}")
    println(s"Richie search, ${filledTable
      .search(776612345)}")
    val removedRichie = filledTable.delete(776612345)
    val nonExisting = filledTable.delete(886612345)
    println(s"Richie search, ${removedRichie
      .search(776612345)}")
    println(s"Non-existing search, ${nonExisting
      .search(886612345)}")
    println(s"Richie search in original, ${filledTable
      .search(776612345)}")
  }
}


