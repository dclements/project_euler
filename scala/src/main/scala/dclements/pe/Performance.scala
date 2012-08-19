package dclements.pe;

object Perf {
  def apply[A](f: => A) = {
  
    val now = System.nanoTime
    val result = f
    val micros = (System.nanoTime - now) / 1000
    
    println("%d microseconds".format(micros))
    result
  
  }
}
