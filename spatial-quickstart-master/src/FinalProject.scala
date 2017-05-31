import spatial._
import org.virtualized._
import spatial.targets.DE1

object FinalProject extends SpatialApp { 
  import IR._

  override val target = DE1

  val Kh = 3
  val Kw = 3
  val Rmax = 240
  val Cmax = 320
  val BallCount = 20

  type Int16 = FixPt[TRUE,_16,_0]
  type UInt8 = FixPt[FALSE,_8,_0]
  type UInt5 = FixPt[FALSE,_5,_0]
  type UInt6 = FixPt[FALSE,_6,_0]

  @struct class sw3(forward: Bool, backward: Bool, unused: UInt8)
  @struct case class Pixel16(b: UInt5, g: UInt6, r: UInt5)
  @struct case class Circle(x: Int, y: Int, rad: Int, velx: Int, vely: Int)

  @virtualize
  def convolveVideoStream(rows: Int, cols: Int): Unit = {

    val imgIn  = StreamIn[Pixel16](target.VideoCamera)
    val imgOut = BufferedOut[Pixel16](target.VGA)

  Accel{


    // Instantiate array of circles
    val arr = SRAM[Circle](BallCount)
    Foreach(0 until BallCount){ i =>
      arr(i) = Circle(random[Int](Cmax), random[Int](Rmax), 3.to[Int], random[Int](20), random[Int](20))
    }

    Stream(*) {_ =>

      // *************************************** 
      // Start of Part 1: Edge Detection
      // *************************************** 

      val kv = LUT[Int16](3, 3)(
         1,  2,  1,
         0,  0,  0,
        -1, -2, -1
      )
      val kh = LUT[Int16](3, 3)(
         1,  0, -1,
         2,  0, -2,
         1,  0, -1
      )

      val sr = RegFile[Int16](Kh, Kw)
      val lb = LineBuffer[Int16](Kh, Cmax)
      val storage = SRAM[Pixel16](Rmax, Cmax)

      Foreach(0 until Rmax) { r =>
        Foreach(0 until Cmax) { _ => 
          val pixel = imgIn.value()
          val grayPixel = (pixel.b.to[Int16] + pixel.g.to[Int16] + pixel.r.to[Int16]) / 3
          lb.enq(grayPixel)
        }


        val horz = Reg[Int16](0)
        val vert = Reg[Int16](0)
        
        Foreach(0 until Cmax) { c =>
          Foreach(0 until Kh par Kh) { i => sr(i, *) <<= lb(i, c) }

          Reduce(horz)(Kh*Kw by 1 par 3/*, Kw by 1 par 3*/) { ii /*(i, j)*/ =>
            val i = ii/3
            val j = ii%3
            val number = mux(r < Kh-1 || c < Kw-1, 0.to[Int16], sr(i, j))
            number * kh(i, j)
          }{_+_}

          Reduce(vert)(Kh*Kw by 1 par 3/*, Kw by 1 par 3*/) { ii /*(i, j)*/ =>
            val i = ii/3
            val j = ii%3
            val number = mux(r < Kh-1 || c < Kw-1, 0.to[Int16], sr(i, j))
            number * kv(i, j)
          }{_+_};

          val result = abs(horz.value) + abs(vert.value)
          imgOut(r,c) = Pixel16(result(5::1).as[UInt5], result(5::0).as[UInt6], result(5::1).as[UInt5]) // Technically should be sqrt(horz**2 + vert**2)
        }

       } 

      // *************************************** 
      // Start of Part 2: Collision Detection
      // *************************************** 



        // Take update x, y values and color pixels a
        Foreach(0 until Rmax) { row =>
            Foreach(0 until Cmax) { col =>

              val accum = Reg[Int16](0)

              Foreach (0 until BallCount){ i =>
                val cir = arr(i)
                val pix_val = mux((row - cir.x)*(row-cir.x) + (col - cir.y)*(col-cir.y) < cir.rad*cir.rad, 2016.to[Int16], 0.to[Int16])
                val update  = accum.value | pix_val
                accum := update
              }

              val pix = accum.value
              
              imgOut(row,col) = mux (pix == 2016.to[Int16],
                                     Pixel16(pix(5::1).as[UInt5], pix(5::0).as[UInt6], pix(5::1).as[UInt5]),
                                     storage(row,col))

            }// end of inner foreach
        }  // end of outer foreach  



      }// End of Stream
      ()
    }// End of Accel 
  }

  @virtualize
  def main() {
    val R = Rmax
    val C = Cmax
    convolveVideoStream(R, C)
  }
}
