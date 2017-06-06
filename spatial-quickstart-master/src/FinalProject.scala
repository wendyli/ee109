import spatial._
import org.virtualized._
import spatial.targets.DE1

object FinalProject extends SpatialApp {
  import IR._

  override val target = DE1
  val Cmax = 320
  val Rmax = 240
  val BallCount = 10

  @struct case class Pixel16(b: UInt5, g: UInt6, r: UInt5)
  
  @struct case class Circle(x: Int, y: Int, rad: Int, velx: Int, vely: Int)

  @virtualize
  def convolveVideoStream(): Unit = {
    val imgOut = BufferedOut[Pixel16](target.VGA)
    val dwell = ArgIn[Int]
    val d = args(0).to[Int]
    setArg(dwell, d)

    Accel {

      val cirX = SRAM[Int](3)
      val cirY = SRAM[Int](3)
      val cirRad = SRAM[Int](3)
      val cirVelX = SRAM[Int](3)
      val cirVelY = SRAM[Int](3)

      // Fill array with circle values
      Foreach(0 until 3){ i =>

          cirX(i)    = 10.to[Int]
          cirY(i)    = 10.to[Int]
          cirRad(i)  = 10.to[Int]
          cirVelX(i) = 1.to[Int]
          cirVelY(i) = 1.to[Int]

      }

      // Generate circles
      Stream(*) { _ => 

        //Generate new coordinates 
        val xval = cirX(0)
        val yval = cirY(0)
            
        cirX(0) = mux( xval + cirVelX(0) > Cmax -10, Cmax - 10, xval + cirVelX(0))
        cirY(0) = mux( yval + cirVelY(0) > Rmax -10, Rmax - 10 , yval + cirVelY(0))
        
        //println("xval is ": xval)
        //println("yval is ": yval)
        // Draw circle 
        Foreach(0 until dwell) { _ =>
          Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>

            //val pixel = mux((r.to[Int] - cirX(0).to[Int])*(r.to[Int] -cirX(0).to[Int]) + (c.to[Int] - cirY(0).to[Int])*(c.to[Int] -cirY(0).to[Int]) < cirRad(0).to[Int] * cirRad(0).to[Int], Pixel16(0,63,0), Pixel16(0,0,0))
            val pixel = mux( (r > cirY(0)) && (r < cirY(0) + 10) && (c > cirX(0)) && (c < cirX(0) + 10), Pixel16(0,63,0), Pixel16(0,0,0))
            imgOut(r, c) = pixel

          }
        } // end of dwell
      
      }// end of stream(*)
    }// end of accel 
  }

  @virtualize
  def main() {
    val R = Rmax
    val C = Cmax
    convolveVideoStream()
  }
}
