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
    //val imgOut = BufferedOut[Pixel16](target.VGA)
    val dwell = ArgIn[Int]
    val d = args(0).to[Int]
    setArg(dwell, d)

    Accel {

      val cir = SRAM[Circle](3)
      // Fill array with circle values
      Foreach(0 until 1){ i =>
          cir(i) = Circle(100 ,100, 10, 0, 0)
      }

      // Generate circle
      Foreach(0 until 30000) { _ => 
        Foreach(0 until dwell) { _ =>
          Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
            
            val pixel = mux((r - cir(0).x)*(r -cir(0).x) + (c - cir(0).y)*(c -cir(0).y) < cir(0).rad * cir(0).rad, Pixel16(0,63,0), Pixel16(0,0,0))
            
            println("Cir x " + cir(0).x)
            println("Cir y " + cir(0).y)
            println("Cir rad " + cir(0).rad)

            //imgOut(r, c) = pixel

          }
        }
      }


    }
  }

  @virtualize
  def main() {
    val R = Rmax
    val C = Cmax
    convolveVideoStream()
  }
}
