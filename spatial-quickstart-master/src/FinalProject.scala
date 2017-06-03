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

      val cir = SRAM[Circle](3)
      Sequential.Foreach(0 until 1){_=>
          cir(0) = Circle(100,100, 10, 0, 0)
          cir(1) = Circle(40,60, 5, 0, 0)
          cir(2) = Circle(150,150, 15, 0, 0)
      }

      /*
      Foreach(0 until 3){ i =>
        cir(i) = Circle(random[Int](Cmax), random[Int](Rmax), 5.to[Int], random[Int](20), random[Int](20))
      }
      */

      Stream (*) { _=>
        // Generate the pixel output 
        Foreach(0 until 3) { i => 
          Foreach(0 until dwell) { _ =>
            Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
              val cir0 = cir(0)
              val cir1 = cir(1)
              val cir2 = cir(2)

              val pixel1 = mux((r - cir0.x)*(r -cir0.x) + (c - cir0.y)*(c -cir0.y) < cir0.rad * cir0.rad, Pixel16(0,63,0), Pixel16(0,0,0))
              val pixel2 = mux((r - cir1.x)*(r -cir1.x) + (c - cir1.y)*(c -cir1.y) < cir1.rad * cir1.rad, Pixel16(31,0,0), Pixel16(0,0,0))
              val pixel3 = mux((r - cir2.x)*(r -cir2.x) + (c - cir2.y)*(c -cir2.y) < cir2.rad * cir2.rad, Pixel16(0,0,31), Pixel16(0,0,0))
            
              val pixel = mux(i == 0, pixel1, mux( i == 1, pixel2, mux( i == 2, pixel3, Pixel16(0,0,0))))
              imgOut(r, c) = pixel

            }
          }
        }

      }// End of Stream
    ()
    }// End of Accel 
  }// End of convolve

  @virtualize
  def main() {
    val R = Rmax
    val C = Cmax
    convolveVideoStream()
  }
}
