import spatial._
import org.virtualized._
import spatial.targets.DE1

object FinalProject extends SpatialApp {
  import IR._

  override val target = DE1
  val Cmax = 320
  val Rmax = 240
  val cirCount = 3
  val cirRad = 10 

  type Pos   = FixPt[TRUE,_16,_16]
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

      val cirX = RegFile[Int](cirCount)
      val cirY = RegFile[Int](cirCount)
      val cirVelX = RegFile[Int](cirCount)
      val cirVelY = RegFile[Int](cirCount)
      val collisionType = RegFile[Int](cirCount) // 1 = ball to ball collision , 2 = border collision , 3 = no collision
      val ballCollide = RegFile[Int](cirCount) // -1 no ball colliding, j = ball collided with

      // Fill array with circle values
      Foreach(0 until cirCount){ i =>
          cirX(i)    = random[UInt16](Cmax).to[Int]
          cirY(i)    = random[UInt16](Rmax).to[Int]
          cirVelX(i) = random[UInt8](3).to[Int] - 6.to[Int] // range of -3 to 3 
          cirVelY(i) = random[UInt8](3).to[Int] - 6.to[Int] // range of -3 to 3 
      }

      // Generate circles
      Stream(*) { _ => 
        
        FSM[Int]{state => state < 4}{state =>
        
          if(state == 0.to[Int]){ // Determine collision type
            
            Sequential{
              Sequential.Foreach(0 until cirCount){ i => 
                Sequential.Foreach(0 until cirCount){ j =>
                  val sqrRad = 4*cirRad*cirRad
                  val distSqr = (cirX(i) - cirX(j)) * (cirX(i) - cirX(j)) + (cirY(i) - cirY(j))*(cirY(i) - cirY(j))
                  collisionType(i) = mux(cirX(i) + cirRad >= Cmax || cirX(i) - cirRad <= 0.to[Int] || cirY(i) + cirRad >= Rmax || cirY(i) - cirRad <= 0.to[Int], 1.to[Int],
                                     mux(distSqr <= sqrRad, 2.to[Int], 
                                     3.to[Int]))
                  ballCollide(i) = mux(distSqr <= sqrRad, j.to[Int], -1.to[Int])
                }
              }
            }

          }else if(state == 1.to[Int]){ // Update velocities 
            // 1 = border collision , 2 = ball to ball collision , 3 = no collision
            Sequential{
              Sequential.Foreach(0 until cirCount){ i => 
                  
                  val ball2 = ballCollide(i)
                  val x2 = cirX(ball2)
                  val y2 = cirY(ball2)
                  val x1 = cirX(i)
                  val y1 = cirY(i)

                  cirVelX(i) = mux(collisionType(i) == 1 &&(cirX(i) + cirRad >= Cmax || cirX(i) - cirRad <= 0.to[Int]),0 - cirVelX(i), 
                               mux(collisionType(i) == 2 &&((x1 < x2 && cirVelX(i) > 0) || (x1 > x2 && cirVelX(i) < 0)),0 - cirVelX(i),
                               cirVelX(i)))

                  cirVelY(i) = mux(collisionType(i) == 1 && (cirY(i) + cirRad >= Rmax || cirY(i) - cirRad <= 0.to[Int]), 0 - cirVelY(i), 
                               mux(collisionType(i) == 2 && ((y1 < y2 && cirVelY(i) > 0) || (y1 > y2 && cirVelY(i) < 0)), 0 - cirVelY(i),
                               cirVelY(i)))

                  // (x-h1)^2 + (y-k1)^2 = 10^2 -> x^2 -2h1x + h1*h1 + y^2 - 2k1y + k1*k1 = 10^2
                  // (x-h2)^2 + (y-k2)^2 = 10^2 -> x^2 - 2h2x + h2*h2 + y^2 - 2k2*y + k2*k2 = 10^2 
                  // x(-2h1+2h2 ) + (h1*h1 - h2*h2) + y(-2k1+2k2) + k1*k1 - k2*k2 = 0 
                  // y  = [-x(-2h1+2h2) - (h1*h1 - h2*h2) - (k1*k1 - k2*k2) ] /(-2k1 + 2k2)
                  // solve for first equation
                  // (x-h1)^2 + (y-k1)^2 = 10^2
                  // (x-h1)^2 + ([-x(-2h1+2h2) - (h1*h1 - h2*h2) - (k1*k1 - k2*k2) ] /(-2k1 + 2k2) - k1)^2 = 10^2 
                  // const n = -(-2h1+2h2) - (h1*h1 - h2*h2) - (k1*k1 - k2*k2) ] /(-2k1 + 2k2)
                  // (x-h1)^2 + (n*x - k1)^2 = 10^2 
                  // x^2 - 2h1x + h1^2 + n^2x^2 - 2k1*n*x + k1

              }
            }

          }else if(state == 2.to[Int]){  // Calculate new positions 

            Sequential{
              Sequential.Foreach(0 until cirCount){ i => 
                cirX(i) = mux( cirX(i) + cirVelX(i) > Cmax -cirRad, Cmax - cirRad, 
                          mux( cirX(i) + cirVelX(i) <= cirRad, cirRad, 
                               cirX(i) + cirVelX(i)))

                cirY(i) = mux( cirY(i) + cirVelY(i) > Rmax -cirRad, Rmax - cirRad, 
                          mux( cirY(i) + cirVelY(i) <= cirRad, cirRad,     
                               cirY(i) + cirVelY(i)))
              }
            }
          
          }else if(state == 3.to[Int]){  // Draw circle 
            
            Sequential{
              Foreach(0 until dwell) { _ =>
                Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
                  val pixel1 = mux((r.to[Int64] - cirY(0).to[Int64])*(r.to[Int64] -cirY(0).to[Int64]) + (c.to[Int64] - cirX(0).to[Int64])*(c.to[Int64] -cirX(0).to[Int64]) < cirRad.to[Int64] * cirRad.to[Int64], Pixel16(0,63,0), Pixel16(0,0,0))
                  val pixel2 = mux((r.to[Int64] - cirY(1).to[Int64])*(r.to[Int64] -cirY(1).to[Int64]) + (c.to[Int64] - cirX(1).to[Int64])*(c.to[Int64] -cirX(1).to[Int64]) < cirRad.to[Int64] * cirRad.to[Int64], Pixel16(0,0,31), Pixel16(0,0,0))
                  val pixel3 = mux((r.to[Int64] - cirY(2).to[Int64])*(r.to[Int64] -cirY(2).to[Int64]) + (c.to[Int64] - cirX(2).to[Int64])*(c.to[Int64] -cirX(2).to[Int64]) < cirRad.to[Int64] * cirRad.to[Int64], Pixel16(31,0,0), Pixel16(0,0,0))
                  val pixel = Pixel16(pixel1.b|pixel2.b|pixel3.b, pixel1.g| pixel2.g|pixel3.g, pixel1.r| pixel2.r|pixel3.r)
                  imgOut(r, c) = pixel
                }
              } 
            }
          }

        }{state => mux(state == 3.to[Int], 0.to[Int], state + 1)}

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
