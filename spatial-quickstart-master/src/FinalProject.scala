import spatial._
import org.virtualized._
import spatial.targets.DE1

object StreamingSobel extends SpatialApp {
  import IR._
  override val target = DE1
  
  val Cmax = 128
  val BallCount = 20
  val DoubleCount = 40

  type bit   = FixPt[FALSE,_1,_0]
  type Int16 = FixPt[TRUE,_16,_0]
  type UInt8 = FixPt[FALSE,_8,_0]
  type UInt5 = FixPt[FALSE,_5,_0]
  type UInt6 = FixPt[FALSE,_6,_0]

  @struct case class Pixel24(b: UInt8, g: UInt8, r: UInt8)
  @struct case class Pixel16(b: UInt5, g: UInt6, r: UInt5)
  @struct case class Circle(x: Int, y: Int, rad: Int, velx: Int, vely: Int)
  @struct case class Position(b0: bit, b1: bit, b2: bit, b3: bit, b4: bit, b5: bit, b6: bit, b7:bit)

  // Returns true if a collision occurs
  @virtualize
  def checkCollision(ballA: Circle, ballB: Circle): Unit = {
    val xDelta = ballA.x - ballB.x
    val yDelta = ballA.y - ballB.y
    val distSqr = xDelta*xDelta + yDelta*yDelta
    val sumRad = ballA.rad + ballB.rad
    val sqrRad = sumRad*sumRad

    val output = mux((distSqr <= sqrRad), 1.to[bit], 0.to[bit])

  }

 @virtualize
  def convolveVideoStream(rows: Int, cols: Int): Unit = {
    val R = ArgIn[Int]
    val C = ArgIn[Int]
    setArg(R, rows)
    setArg(C, cols)

    val imgIn  = StreamIn[Pixel24](target.VideoCamera)
    val imgOut = StreamOut[Pixel16](target.VGA)

    Accel {
      val fifoIn = FIFO[Int16](128)
      val fifo2  = FIFO[Int16](128)
      val fifoOut = FIFO[Int16](128)

      Stream(*) { _ =>
        // *****************************************************
        // Intialize ball array 
        // *****************************************************
         
         // Generate random number
         val rand = RegFile[Int](DoubleCount) // stores random x, y positions 
         val randVel = RegFile[Int](DoubleCount) // stores random x, y velocities

         val b0 = Reg[bit](0)
         val b1 = Reg[bit](0)
         val b2 = Reg[bit](0)
         val b3 = Reg[bit](0)
         val b4 = Reg[bit](0)
         val b5 = Reg[bit](0)
         val b6 = Reg[bit](0)
         val b7 = Reg[bit](0)

         //starting values
         b0 := 1.to[bit]
         b4 := 1.to[bit]
         b5 := 1.to[bit]

        // generate array of random values ( 0 - 127)
        Sequential(0 until DoubleCount){ i=>
          b0 := b0.value ^ b2.value
          b1 := b1.value ^ b3.value
          b2 := b2.value ^ b4.value
          b3 := b3.value ^ b5.value
          b4 := b4.value ^ b6.value
          b5 := b5.value ^ b7.value
          b6 := b6.value ^ b0.value
          b7 := b7.value ^ b1.value  // for now ignore since test size is 128 pixels
          
          rand(i) = b0.value.to[Int]*64 + b1.value.to[Int]*32 + b2.value.to[Int]*16 + b3.value.to[Int]*8 + b4.value.to[Int]*4 + b5.value.to[Int]*2 + b6.value.to[Int]
          randVel(i) = mux(b0.value.to[Int] == 1, 0 - (b1.value.to[Int]*8 + b2.value.to[Int]*4+ b3.value.to[Int]*2 + b4.value.to[Int]), b1.value.to[Int]*8 + b2.value.to[Int]*4+ b3.value.to[Int]*2 + b4.value.to[Int])
         }


         // Generate intial circle structs
         val arr = RegFile[Circle](BallCount)
         Foreach(0 until DoubleCount-2 par 2){ i =>
           arr(i/2) = Circle(rand(i), rand(i+1), 5, randVel(i), randVel(i+1))
         }


         // ADD EDGE DETECTION CODE HERE
         // ADD EDGE DETECTION CODE HERE
         val pixel_video = imgIn.value()
         val pixel_gray = (pixel_video.b.to[Int16] + pixel_video.g.to[Int16] + pixel_video.r.to[Int16]) / 3
         fifoIn.enq(pixel_gray)
         // ADD EDGE DETECTION CODE HERE
         // ADD EDGE DETECTION CODE HERE

        // **********************************************************************
        // Check if a collision has occured and determine new ball position
        // **********************************************************************
        
        // **********************************************************************
        // Take new ball positions and update pixel values
        // **********************************************************************
        
        Foreach (0 until R, 0 until C) { (row, col) =>
        val pixel = fifoIn.deq()
        val accum = Reg[Int16](0)
        accum := 0

        // color individual balls green
          Foreach (0 until BallCount){ i =>
            val cir = arr(i)
            val pix_val = mux((row - cir.x)*(row-cir.x) + (col - cir.y)*(col-cir.y) < cir.rad*cir.rad , 2016.to[Int16], 0.to[Int16])
            val update  = accum | pix_val
            accum := update
          }

          val ball_pixel = accum.value
          fifoOut.enq(ball_pixel)
        }

      // send pixel values to image out
      val new_pixel = fifoOut.deq()
      imgOut := Pixel16(new_pixel(15::11).as[UInt5], new_pixel(10::5).as[UInt6], new_pixel(4::0).as[UInt5])


      }
      ()
    }
  }

  @virtualize
  def main() {
    val R = args(0).to[Int]
    val C = Cmax
    convolveVideoStream(R, C)
  }
}

