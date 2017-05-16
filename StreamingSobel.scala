import spatial._
import org.virtualized._
import spatial.targets.DE1

object StreamingSobel extends SpatialApp { 
  import IR._

  override val target = DE1

  val Kh = 3
  val Kw = 3
  val Cmax = 128

  type Int16 = FixPt[TRUE,_16,_0]
  type UInt8 = FixPt[FALSE,_8,_0]
  type UInt5 = FixPt[FALSE,_5,_0]
  type UInt6 = FixPt[FALSE,_6,_0]
  @struct case class Pixel24(b: UInt8, g: UInt8, r: UInt8)
  @struct case class Pixel16(b: UInt5, g: UInt6, r: UInt5)

  @virtualize
  def convolveVideoStream(rows: Int, cols: Int): Unit = {
    val R = ArgIn[Int]
    val C = ArgIn[Int]
    setArg(R, rows)
    setArg(C, cols)

    val imgIn  = StreamIn[Pixel24](target.VideoCamera)
    val imgOut = StreamOut[Pixel16](target.VGA)

    Accel {
      val kh = RegFile[Int16](Kh, Kw)
      val kv = RegFile[Int16](Kh, Kw)

      Pipe {
        kh(0, 0) = 1
        kh(1, 0) = 2
        kh(2, 0) = 1
        kh(0, 1) = 0
        kh(1, 1) = 0
        kh(2, 1) = 0
        kh(0, 2) = -1
        kh(1, 2) = -2
        kh(2, 2) = -1
        kv(0, 0) = 1
        kv(0, 1) = 2
        kv(0, 2) = 1
        kv(1, 0) = 0
        kv(1, 1) = 0
        kv(1, 2) = 0
        kv(2, 0) = -1
        kv(2, 1) = -2
        kv(2, 2) = -1
      }

      val sr = RegFile[Int16](Kh, Kw)
      val fifoIn = FIFO[Int16](128)
      val fifoOut = FIFO[Int16](128)
      val lb = LineBuffer[Int16](Kh, Cmax)

      Stream(*) { _ =>
        // Main Hardware block here
        // TODO: Get a pixel from imgIn, and convert it to gray scale. We will be convolving our kernel over the gray-scale pixels.
        // After converting the pixel, you will need to push it to fifoIn.
        // Using a fifo ensures that the timing is synchronized between the convolution stage and pixel preprocessing stage.
        // YOUR CODE HERE:
          val pixel_video = imgIn.value()
 
          val pixel_gray = (pixel_video.b.to[Int16] + pixel_video.g.to[Int16] + pixel_video.r.to[Int16]) / 3
          fifoIn.enq(pixel_gray) 
        // TODO: We will need to iterate over all the pixels in the frame using a Foreach loop. In each iteration of the loop,
         // we need to convolve a Kh by Kw window around the currrent pixel with the kernel. 
         // To obtain the window, first we need to update the line buffer by dequeuing one pixel from fifoIn and enqueuing it 
         //into the line buffer. After the update, we can load the current window from the line buffer by using: 
         // i => sr(i, *) <<= lb(i, c), where i is the row number and lb is the lin  buffer. 
         //You will need to iterate over rows in the kernel to perform the load. 

    
        // YOUR CODE HERE:
          Foreach(0 until R, 0 until C) { (r_inx, c_inx) =>

            val curr_pixel = fifoIn.deq()

            lb.enq(curr_pixel)

            Foreach(0 until Kh par Kw) { i =>
              sr(i, *) <<= lb(i, c_inx)
            }

        // TODO: After we load the window around the current pixel, we need to perform convolution. 
        //If you perform the last step correctly, sr should now contain a window of size Kh by Kw. 
        //Please convolve it with the horizontal kernel (kh) and the vertical kernel (kv).
        // Here is an example to convolve the horizontal window with sr:
        //  val horz = Reduce(Reg[Int16])(Kh by 1, Kw by 1) { (i, j) =>
        //    val number = mux(r < Kh-1 || c < Kw-1, 0.to[Int16], sr(i, j))
        //    number * kh(i, j)
        //  }{_+_}
        // What we are doing here is that for each pixel in the window, we multiply it by 0 if it does not overlap with the kernel.
        // Otherwise we multiply it with the corresponding kernel value at position (i,j). 
        // You will need to do the same computation for the vertical kernel.
        // YOUR CODE HERE:
  
        val horz = Reduce(Reg[Int16])(Kh by 1, Kw by 1) { (i, j) =>
             val number = mux(r_inx < Kh-1 || c_inx < Kw-1, 0.to[Int16], sr(i, j))
              number * kh(i, j)
          }{_+_}

          val vert = Reduce(Reg[Int16])(Kh by 1, Kw by 1) { (i, j) =>
             val number = mux(r_inx < Kh-1 || c_inx < Kw-1, 0.to[Int16], sr(i, j))
              number * kv(i, j)
          }{_+_}             
         
        // TODO: After you obtain the convolved values with the horizontal kernel and the vertical kernel, 
        //you can simply add the absolute values of both to form the final output of your sobel filter. Please enqueue it into fifoOut. 
        // YOUR CODE HERE: 
         fifoOut.enq( abs(horz.value) + abs(vert.value) )


         }


        // TODO: This step should be performed outside of the Foreach loop over the whole frame.
        // You will need to dequeue pixels from fifOut, and send it to imgOut (The StreamOut port).
        // YOUR CODE HERE:
         val new_pixel = fifoOut.deq()
    
         imgOut := Pixel16(new_pixel(10::6).as[UInt5], new_pixel(10::5).as[UInt6], new_pixel(10::6).as[UInt5])
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
