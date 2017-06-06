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

          cirX(i)    = 10.to[Int]
          cirY(i)    = 10.to[Int]
          cirRad(i)  = 10.to[Int]
          cirVelX(i) = 1.to[Int]
          cirVelY(i) = 1.to[Int]
      }

      // Generate circles
      Stream(*) { _ => 

        FSM[Int]{state => state < 3}{state =>
        
          if(state == 0.to[Int]){ // Set new velocities
            
            Sequential{
              val RCollide = mux(cirX(0) + cirRad(0) > Cmax, 1.to[Int], 0.to[Int])
              val LCollide = mux(cirX(0) - cirRad(0) < 0,    1.to[Int], 0.to[Int])
              val TCollide = mux(cirY(0) + cirRad(0) > Rmax, 1.to[Int], 0.to[Int])
              val BCollide = mux(cirY(0) - cirRad(0) < 0,    1.to[Int], 0.to[Int])

              cirVelX(0) = mux( RCollide == 1.to[Int]|| LCollide == 1.to[Int], 0 - cirVelX(0), cirVelX(0))
              cirVelY(0) = mux( TCollide == 1.to[Int]|| BCollide == 1.to[Int], 0 - cirVelY(0), cirVelY(0))
            }

          }else if(state == 1.to[Int]){  // Calculate new positions

            Sequential{
     
              val newX = cirX(0) + cirVelX(0)
              val newY = cirY(0) + cirVelY(0)

              cirX(0) = mux( newX > Cmax -10, Cmax - 10, newX )
              cirY(0) = mux( newY > Rmax -10, Rmax - 10, newY )
            }
          
          }else if(state == 2.to[Int]){  // Draw circle 
            
            Sequential{
              Foreach(0 until dwell) { _ =>
                Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
                  //val pixel = mux((r.to[Int] - cirX(0).to[Int])*(r.to[Int] -cirX(0).to[Int]) + (c.to[Int] - cirY(0).to[Int])*(c.to[Int] -cirY(0).to[Int]) < cirRad(0).to[Int] * cirRad(0).to[Int], Pixel16(0,63,0), Pixel16(0,0,0))
                  val pixel = mux( (r > cirY(0)) && (r < cirY(0) + 10) && (c > cirX(0)) && (c < cirX(0) + 10), Pixel16(0,63,0), Pixel16(0,0,0))
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
