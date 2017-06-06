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

          cirX(i)    = random[UInt16](Cmax).to[Int]
          cirY(i)    = random[UInt16](Rmax).to[Int]
          cirRad(i)  = 10.to[Int]
          cirVelX(i) = random[UInt8](3).to[Int] - 6.to[Int] // range of -3 to 3 
          cirVelY(i) = random[UInt8](3).to[Int] - 6.to[Int] // range of -3 to 3 
      }

      // Generate circles
      Stream(*) { _ => 

        FSM[Int]{state => state < 3}{state =>
        
          if(state == 0.to[Int]){ // Set new velocities
            
            Sequential.Foreach(0 until cirCount){ i =>
              
              cirVelX(i) = mux( cirX(i) + cirRad(i) >= Cmax || cirX(i) - cirRad(i) <= 0.to[Int], 0 - cirVelX(i), cirVelX(i))
              cirVelY(i) = mux( cirY(i) + cirRad(i) >= Rmax || cirY(i) - cirRad(i) <= 0.to[Int], 0 - cirVelY(i), cirVelY(i))
            
            }

          }else if(state == 1.to[Int]){ // Calculate new positions

            Sequential.Foreach(0 until cirCount){ i =>

              cirX(0) = mux( cirX(i) + cirVelX(i) > Cmax - 10, Cmax - 10, 
                        mux( cirX(i) + cirVelX(i) <= 10, 10, 
                             cirX(i) + cirVelX(i)))

              cirY(0) = mux( cirY(i) + cirVelY(i) > Rmax - 10, Rmax - 10, 
                        mux( cirY(i) + cirVelY(i) <= 10, 10,     
                             cirY(i) + cirVelY(i)))
            }
          
          }else if(state == 2.to[Int]){  // Draw circle 
            
            Sequential{
              Foreach(0 until dwell) { _ =>
                Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
                  //val pixel = mux((r.to[Int] - cirX(0).to[Int])*(r.to[Int] -cirX(0).to[Int]) + (c.to[Int] - cirY(0).to[Int])*(c.to[Int] -cirY(0).to[Int]) < cirRad(0).to[Int] * cirRad(0).to[Int], Pixel16(0,63,0), Pixel16(0,0,0))
                  val pixel1 = mux((r > cirY(0)) && (r < cirY(0) + 10) && (c > cirX(0)) && (c < cirX(0) + 10), Pixel16(0,63,0), Pixel16(0,0,0))
                  val pixel2 = mux((r > cirY(1)) && (r < cirY(1) + 10) && (c > cirX(1)) && (c < cirX(1) + 10), Pixel16(31,0,0), Pixel16(0,0,0)) 
                  val pixel3 = mux((r > cirY(2)) && (r < cirY(2) + 10) && (c > cirX(2)) && (c < cirX(2) + 10), Pixel16(0,0,31), Pixel16(0,0,0))
                  val pixel  = Pixel16(pixel1.b|pixel2.b|pixel3.b, pixel1.g|pixel2.g|pixel3.g, pixel1.r|pixel2.r|pixel3.r)  
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
