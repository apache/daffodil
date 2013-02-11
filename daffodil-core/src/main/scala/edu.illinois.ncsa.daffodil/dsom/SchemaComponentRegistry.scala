package edu.illinois.ncsa.daffodil.dsom
import scala.collection.mutable.HashMap
import java.util.UUID
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * Used to navigate from the Infoset back to schema components relevant
 * to a part of the Infoset.
 */
object SchemaComponentRegistry {

  private val contextMap: HashMap[UUID, ElementBase] = HashMap.empty

  def getComponentByID(uid: String): Option[ElementBase] = {
    val ctxMap = contextMap
    // TODO: why this try/catch??
    try {
      val uuid = UUID.fromString(uid)
      return ctxMap.get(uuid)
    } catch {
      case u: UnsuppressableException => throw u
      case e: Exception => {
        Assert.impossibleCase() // Let's see if this happens. 
        // TODO: if the above never happens, eliminate the try/catch
        return None
      }
    }
    None
  }

  def addComponent(sc: ElementBase): String = {
    val ctxMap = contextMap
    val uuid = UUID.randomUUID()
    ctxMap.put(uuid, sc)
    uuid.toString
  }

}

