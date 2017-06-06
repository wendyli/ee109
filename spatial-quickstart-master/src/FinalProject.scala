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

    Accel{

      val cirX = SRAM[Int](3)
      val cirY = SRAM[Int](3)
      val cirRad = SRAM[Int](3)
      val cirVelX = SRAM[Int](3)
      val cirVelY = SRAM[Int](3)
      val state = SRAM[Int](1)

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

        //Get new coordinates 
        Sequential{

          if(state(0) == 1.to[Int]){ // Set new velocities
            Sequential{
              Sequential.Foreach(0 until cirCount){ i =>
                // Calculate new velocity vectors
                val RCollide = mux(cirX(i) + cirRad(i) > Cmax, 1.to[Int], 0.to[Int])
                val LCollide = mux(cirX(i) - cirRad(i) < Cmax, 1.to[Int], 0.to[Int])
                val TCollide = mux(cirY(i) + cirRad(i) > Rmax, 1.to[Int], 0.to[Int])
                val BCollide = mux(cirY(i) - cirRad(i) < Rmax, 1.to[Int], 0.to[Int])

                // Set new velocities
                cirVelX(i) = mux( RCollide == 1.to[Int]|| LCollide == 1.to[Int], 0 - cirVelX(i), cirVelX(i)
                cirVelY(i) = mux( TCollide == 1.to[Int]|| BCollide == 1.to[Int], 0 - cirVelY(i), cirVelY(i))
              }
              state(0) = 2.to[Int]
            }

          }else if(state(0) == 2.to[Int]){
            
            Sequential{
              Sequential.Foreach(0 until cirCount){ i =>
                // Calculate new positions
                cirX(i) = mux( cirX(i) + cirVelX(i) > Cmax -10, Cmax - 10, cirX(i) + cirVelX(i))
                cirY(i) = mux( cirY(i) + cirVelY(i) > Rmax -10, Rmax - 10, cirY(i) + cirVelY(i))
              }

              state(0) = 3.to[Int]
            }
       
          }else if(state(0) == 3.to[Int]){
            
            Sequential{
              // Draw circle 
              Sequential.Foreach(0 until dwell) { _ =>
                Sequential.Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
                  
                  accum = Reg[Int16](0)
                  Sequential.Foreach(0 until cirCount){ i =>
                    val pixel = mux( (r > cirY(0)) && (r < cirY(0) + 10) && (c > cirX(0)) && (c < cirX(0) + 10), 2016.to[Int16], 0.to[Int16])
                    accum := accum.value | pixel
                  }

                  imgOut(r, c) = accum.value

                }
              } 

              state(0) = 1.to[Int]
            }

          }

        } //end of outer sequential 

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
