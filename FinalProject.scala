import spatial._
import org.virtualized._
import spatial.targets.DE1

object FinalProject extends SpatialApp {
  import IR._

  override val target = DE1
  val Cmax = 320
  val Rmax = 240
  val cirCount = 10
  val cirRad = 10 
  val maxCircles = 15 // maximum number of circles you can instantiate 

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

      val cirX = SRAM[Int](maxCircles) // x position 
      val cirY = SRAM[Int](maxCircles) // y position 
      val cirVelX = SRAM[Int](maxCircles) // velocity vector x comp 
      val cirVelY = SRAM[Int](maxCircles) // velocity vector y comp
      val collisionType = SRAM[Int](maxCircles) // 1 = ball to ball collision , 2 = border collision , 0 = no collision
      val ballCollide = SRAM[Int](maxCircles) // i = no ball colliding, j = ball collided with

      Sequential{ // instantiate values

        // Fill array with circle values     
        Foreach(0 until cirCount){ i =>
          Pipe{
            val X = random[UInt16](Cmax).to[Int]
            val Y = random[UInt16](Rmax).to[Int]
            cirX(i)    = mux(X < 0, 0.to[Int], X)
            cirY(i)    = mux(Y < 0, 0.to[Int], Y)
            cirVelX(i) = random[UInt8](3).to[Int] - 6.to[Int] // range of -3 to 3 
            cirVelY(i) = random[UInt8](3).to[Int] - 6.to[Int] // range of -3 to 3 
          }
        }

        // Get rid of any overlaps among circles 
        Foreach(0 until cirCount){_=> // arbitrary number of loops, TODO: ideal would be a do while loop  
          Sequential.Foreach(0 until cirCount, 0 until cirCount){ (i,j) => 
            Pipe{
               val sqrRad = 4*cirRad*cirRad
               val distSqr = (cirX(i) - cirX(j)) * (cirX(i) - cirX(j)) + (cirY(i) - cirY(j))*(cirY(i) - cirY(j))
               if(distSqr <= sqrRad && (i != j)){ // if overlap found, instantiate new x and y params 
                cirX(i) = random[UInt16](Cmax).to[Int]
                cirY(i) = random[UInt16](Rmax).to[Int]
              }
            }
          }
        }
      }

      // Update frames 
      Stream(*) { _ => 

        FSM[Int]{state => state < 4}{state =>
        
        if(state == 0.to[Int]){ // Determine collision type  
            Sequential{
              val borderCollision = SRAM[Int](maxCircles)
              val ballCollision = SRAM[Int](maxCircles)

              Sequential.Foreach(0 until cirCount){ i =>  // detect border collision 
                Pipe{
                  borderCollision(i) = mux(cirX(i) + cirRad >= Cmax || cirX(i) - cirRad <= 0.to[Int] || cirY(i) + cirRad >= Rmax || cirY(i) - cirRad <= 0.to[Int], 1.to[Int], 0.to[Int])
                  ballCollide(i) = i.to[Int]
                  ballCollision(i) = 0.to[Int]
                }
              }

              Sequential.Foreach(0 until cirCount, 0 until cirCount){ (i,j) =>  // detect ball to ball collision
                Pipe{
                  val sqrRad = 4*cirRad*cirRad
                  val distSqr = (cirX(i) - cirX(j)) * (cirX(i) - cirX(j)) + (cirY(i) - cirY(j))*(cirY(i) - cirY(j))
                  ballCollision(i) = mux(distSqr <= sqrRad && (i != j), 1.to[Int], ballCollision(i))
                  ballCollide(i) = mux(distSqr <= sqrRad && (i != j), j.to[Int], ballCollide(i))
                }
              }

              Sequential.Foreach(0 until cirCount){ i => // determine collision type
                Pipe{
                  collisionType(i) = mux(borderCollision(i) == 1, 1.to[Int], 
                                     mux(ballCollision(i) == 1, 2.to[Int], 
                                     0.to[Int]))
                }
              }
            }

          }else if(state == 1.to[Int]){ // Update velocities 
            // 1 = border collision , 2 = ball to ball collision , 0 = no collision
            Sequential{
              Sequential.Foreach(0 until cirCount){ i => 
                Pipe{
                    val ball2 = ballCollide(i)
                    val x2 = cirX(ball2)
                    val y2 = cirY(ball2)
                    val x1 = cirX(i)
                    val y1 = cirY(i)

                    if(collisionType(i) == 1){
                      Pipe{
                        cirVelX(i) = mux(cirX(i) + cirRad >= Cmax || cirX(i) - cirRad <= 0.to[Int],0 - cirVelX(i), cirVelX(i))
                        cirVelY(i) = mux(cirY(i) + cirRad >= Rmax || cirY(i) - cirRad <= 0.to[Int], 0 - cirVelY(i), cirVelY(i))
                      }
                    }else if(collisionType(i) == 2){
                      Pipe{
                        cirVelX(i) = mux((x1 < x2 && cirVelX(i) > 0) || (x1 > x2 && cirVelX(i) < 0),0 - cirVelX(i), cirVelX(i)))
                        cirVelY(i) = mux((y1 < y2 && cirVelY(i) > 0) || (y1 > y2 && cirVelY(i) < 0), 0 - cirVelY(i), cirVelY(i))
                      }
                    }else{
                      Pipe{
                        cirVelX(i) = cirVelX(i)
                        cirVelY(i) = cirVelY(i)
                      }
                    }
                }
              }
            }
          
          }else if(state == 2.to[Int]){  // Calculate new positions

            Sequential{
              Sequential.Foreach(0 until cirCount){ i => 
                Pipe{
                  cirX(i) = mux( cirX(i) + cirVelX(i) > Cmax -cirRad, Cmax - cirRad, 
                            mux( cirX(i) + cirVelX(i) <= cirRad, cirRad, 
                                 cirX(i) + cirVelX(i)))

                  cirY(i) = mux( cirY(i) + cirVelY(i) > Rmax -cirRad, Rmax - cirRad, 
                            mux( cirY(i) + cirVelY(i) <= cirRad, cirRad,     
                                 cirY(i) + cirVelY(i)))
                }
              }
            }
          
          }else if(state == 3.to[Int]){  // Draw circle 
            
            Sequential{
              Foreach(0 until dwell){_=>
                Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
                  val acc = SRAM[UInt6](1)
                  
                  Pipe{
                    acc(0) = 0.to[UInt6]
                        
                    Sequential.Foreach(0 until cirCount){ i=>
                      Pipe{
                        val pixel = mux((r.to[Int64] - cirY(i).to[Int64])*(r.to[Int64] -cirY(i).to[Int64]) + (c.to[Int64] - cirX(i).to[Int64])*(c.to[Int64] -cirX(i).to[Int64]) < cirRad.to[Int64] * cirRad.to[Int64], 63.to[UInt6], 0.to[UInt6])
                        acc(0) = mux(acc(0) == 0.to[UInt6], pixel, acc(0))
                      } 
                    }
                  }
                  imgOut(r, c) = Pixel16(0.to[UInt5], acc(0), 0.to[UInt5])
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
