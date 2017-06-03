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

              val pixel1 = mux((r - cir(0).x)*(r -cir(0).x) + (c - cir(0).y)*(c -cir(0).y) < cir(0).rad * cir(0).rad, Pixel16(0,63,0), Pixel16(0,0,0))
              val pixel2 = mux((r - cir(1).x)*(r -cir(1).x) + (c - cir(1).y)*(c -cir(1).y) < cir(1).rad * cir(1).rad, Pixel16(31,0,0), Pixel16(0,0,0))
              val pixel3 = mux((r - cir(2).x)*(r -cir(2).x) + (c - cir(2).y)*(c -cir(2).y) < cir(2).rad * cir(2).rad, Pixel16(0,0,31), Pixel16(0,0,0))
              
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
