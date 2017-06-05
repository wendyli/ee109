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

    val leds = target.LEDR
    val ledOutput = StreamOut[Int](leds)
    val cirx = Reg[Int](0)

    Accel(*){

      val cir = SRAM[Circle](3)
      // Fill array with circle values
      Foreach(0 until 1){ i =>
          cir(i) = Circle(100 ,100, 10, 0, 0)
      }

      // Generate circles
      Stream(*){ 
        Foreach(0 until dwell) { _ =>
          Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
        
            val pixel = mux((r.to[Int] - cir(0).x.to[Int])*(r.to[Int] -cir(0).x.to[Int]) + (c.to[Int] - cir(0).y.to[Int])*(c.to[Int] -cir(0).y.to[Int]) < cir(0).rad.to[Int] * cir(0).rad.to[Int], Pixel16(0,63,0), Pixel16(0,0,0))

            cirx := cir(0).x.to[Int]
            ledOutput := cirx.value()

          }
        }
      }// end of stream 


    }// end of accel 
  }

  @virtualize
  def main() {
    val R = Rmax
    val C = Cmax
    convolveVideoStream()
  }
}
