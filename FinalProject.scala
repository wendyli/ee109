import spatial._
import org.virtualized._
import spatial.targets.DE1

object FinalProject extends SpatialApp {
  import IR._

  override val target = DE1
  val Cmax = 320
  val Rmax = 240
  val BallCount = 10
  val cirCount = 3

  type Int64 = FixPt[TRUE,_64,_0]
  type Int16 = FixPt[TRUE,_16,_0]
  type UInt8 = FixPt[FALSE,_8,_0]
  type UInt16 = FixPt[FALSE,_16,_0]
  type UInt5 = FixPt[FALSE,_5,_0]
  type UInt6 = FixPt[FALSE,_6,_0]

  @struct case class Pixel16(b: UInt5, g: UInt6, r: UInt5)

  @virtualize
  def convolveVideoStream(): Unit = {
    val imgOut = BufferedOut[Pixel16](target.VGA)
    val dwell = ArgIn[Int]
    val d = args(0).to[Int]
    setArg(dwell, d)

    Accel{

      val cirX = SRAM[Int](3)
      val cirY = SRAM[Int](3)
      val cirRad = SRAM[Int](3)
      val cirVelX = SRAM[Int](3)
      val cirVelY = SRAM[Int](3)

      // Fill array with circle values
      Foreach(0 until cirCount){ i =>

          //cirX(i)    = random[UInt16](Cmax).to[Int]
          //cirY(i)    = random[UInt16](Rmax).to[Int]
          cirX(i) = 10.to[Int] + i*10
          cirY(i) = 10.to[Int]
          cirRad(i)  = 10.to[Int]
          cirVelX(i) = 2.to[Int]
          cirVelY(i) = 2.to[Int]
          //cirVelX(i) = random[UInt8](3).to[Int] - 6.to[Int] // range of -3 to 3 
          //cirVelY(i) = random[UInt8](3).to[Int] - 6.to[Int] // range of -3 to 3 
      }

      // Generate circles
      Stream(*) { _ => 

        FSM[Int]{state => state < 3}{state =>
        
          if(state == 0.to[Int]){ // Set new velocities
            
            Sequential{

              cirVelX(0) = mux( cirX(0) + cirRad(0) >= Cmax || cirX(0) - cirRad(0) <= 0.to[Int], 0 - cirVelX(0), cirVelX(0))
              cirVelY(0) = mux( cirY(0) + cirRad(0) >= Rmax || cirY(0) - cirRad(0) <= 0.to[Int], 0 - cirVelY(0), cirVelY(0))
              cirVelX(1) = mux( cirX(1) + cirRad(1) >= Cmax || cirX(1) - cirRad(1) <= 0.to[Int], 0 - cirVelX(1), cirVelX(1))
              cirVelY(1) = mux( cirY(1) + cirRad(1) >= Rmax || cirY(1) - cirRad(1) <= 0.to[Int], 0 - cirVelY(1), cirVelY(1))

            }

          }else if(state == 1.to[Int]){  // Calculate new positions

            Sequential{

              cirX(0) = mux( cirX(0) + cirVelX(0) > Cmax -10, Cmax - 10, 
                        mux( cirX(0) + cirVelX(0) <= 10, 10, 
                             cirX(0) + cirVelX(0)))

              cirY(0) = mux( cirY(0) + cirVelY(0) > Rmax -10, Rmax - 10, 
                        mux( cirY(0) + cirVelY(0) <= 10, 10,     
                             cirY(0) + cirVelY(0)))


              cirX(1) = mux( cirX(1) + cirVelX(1) > Cmax -10, Cmax - 10, 
                        mux( cirX(1) + cirVelX(1) <= 10, 10, 
                             cirX(1) + cirVelX(1)))

              cirY(1) = mux( cirY(1) + cirVelY(1) > Rmax -10, Rmax - 10, 
                        mux( cirY(1) + cirVelY(1) <= 10, 10,     
                             cirY(1) + cirVelY(1)))
            }
          
          }else if(state == 2.to[Int]){  // Draw circle 
            
            Sequential{
              Foreach(0 until dwell) { _ =>
                Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
                  val pixel1 = mux( (r > cirY(0) - 10) && (r < cirY(0) + 10) && (c > cirX(0) - 10) && (c < cirX(0) + 10), Pixel16(0,63,0), Pixel16(0,0,0))
                  val pixel2 = mux( (r > cirY(1) - 5) && (r < cirY(1) + 5) && (c > cirX(1) - 15) && (c < cirX(1) + 15), Pixel16(0,0,31), Pixel16(0,0,0))
                  //val pixel = mux( (r.to[Int64] - cirX(0).to[Int64])*(r.to[Int64] -cirX(0).to[Int64]) + (c.to[Int64] - cirY(0).to[Int64])*(c.to[Int64] -cirY(0).to[Int64]), Pixel16(0,63,0), Pixel16(0,0,0))
                  val pixel = Pixel16(pixel1.b|pixel2.b, pixel1.g| pixel2.g, pixel1.r| pixel2.r)
                  imgOut(r, c) = pixel

                }
              } 
            }
          }

        }{state => mux(state == 2.to[Int], 0.to[Int], state + 1)}

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
