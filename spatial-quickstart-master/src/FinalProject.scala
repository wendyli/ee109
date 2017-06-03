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

    Accel (*) {

      val cir = SRAM[Circle](3)
      // Fill array with circle values
      Foreach(0 until 3){ i =>
          cir(i) = Circle(100 + i*10,100 + i*10, 10, 5, i)
      }

      Foreach(0 until 1000) { i =>  // frame count

        //1. Change circle values
        Foreach(0 until 3){i=>
          val new_cir = cir(i)
          cir(i) = Circle(new_cir.x + new_cir.velx , new_cir.y + new_cir.vely, 10, 5, i)
        } 

        //2. Draw in circles 
        Foreach(0 until dwell) { _ =>
          Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
            
            val pixel1 = mux((r - cir(0).x)*(r -cir(0).x) + (c - cir(0).y)*(c -cir(0).y) < cir(0).rad * cir(0).rad, Pixel16(0,63,0), Pixel16(0,0,0))
            val pixel2 = mux((r - cir(1).x)*(r -cir(1).x) + (c - cir(1).y)*(c -cir(1).y) < cir(1).rad * cir(1).rad, Pixel16(31,0,0), Pixel16(0,0,0))
            val pixel3 = mux((r - cir(2).x)*(r -cir(2).x) + (c - cir(2).y)*(c -cir(2).y) < cir(2).rad * cir(2).rad, Pixel16(0,0,31), Pixel16(0,0,0))
            
            val pixel = Pixel16(pixel1.b|pixel2.b|pixel3.b, pixel1.g|pixel2.g|pixel3.g, pixel1.r|pixel2.r|pixel3.r)
            imgOut(r, c) = pixel

          }
        } 


      } // end of frame count
    }
  }

  @virtualize
  def main() {
    val R = Rmax
    val C = Cmax
    convolveVideoStream()
  }
}
