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
  val cirRad = 10 

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

      val cirX = RegFile[Int](3)
      val cirY = RegFile[Int](3)
      val cirVelX = RegFile[Int](3)
      val cirVelY = RegFile[Int](3)

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
            
            Sequential{
              Sequential.Foreach(0 until cirCount){ i => 
                cirVelX(i) = mux( cirX(i) + cirRad >= Cmax || cirX(i) - cirRad <= 0.to[Int], 0 - cirVelX(i), cirVelX(i))
                cirVelY(i) = mux( cirY(i) + cirRad >= Rmax || cirY(i) - cirRad <= 0.to[Int], 0 - cirVelY(i), cirVelY(i))
              }
            }

          }else if(state == 1.to[Int]){  // Calculate new positions

            Sequential{
              Sequential.Foreach(0 until cirCount){ i => 
                cirX(i) = mux( cirX(i) + cirVelX(i) > Cmax - cirRad, Cmax - cirRad, 
                          mux( cirX(i) + cirVelX(i) <= cirRad, cirRad, 
                               cirX(i) + cirVelX(i)))

                cirY(i) = mux( cirY(i) + cirVelY(i) > Rmax - cirRad, Rmax - cirRad, 
                          mux( cirY(i) + cirVelY(i) <= cirRad, cirRad,     
                               cirY(i) + cirVelY(i)))
              }
            }
          
          }else if(state == 2.to[Int]){  // Draw circle 
            
            Sequential{
              Foreach(0 until dwell) { _ =>
                Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
                  
                  val pixel = Reg[UInt6](0)
                  Sequential.Foreach(0 until cirCount){ i=>
                    val green_pixel = mux((r.to[Int64] - cirY(i).to[Int64])*(r.to[Int64] -cirY(i).to[Int64]) + (c.to[Int64] - cirX(i).to[Int64])*(c.to[Int64] -cirX(i).to[Int64]) < cirRad.to[Int64] * cirRad.to[Int64], 63.to[UInt6], 0.to[UInt6])
                    pixel := pixel.value | green_pixel 
                  }
                  imgOut(r, c) = Pixel16(0.to[UInt5], pixel.value.to[UInt6], 0.to[UInt5])

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
