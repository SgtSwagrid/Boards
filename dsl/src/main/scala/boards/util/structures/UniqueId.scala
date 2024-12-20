package boards.util.structures

trait UniqueId:
  
  import UniqueId.*
  
  private val id: Int = { nextId += 1; nextId }
  
  override def hashCode: Int = id
  
  override def equals(that: Any): Boolean =
    that match
      case that: UniqueId => this.id == that.id
      case _ => false
  
object UniqueId:
  
  private var nextId: Int = -1