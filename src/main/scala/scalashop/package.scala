
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int
  type DRGBA = (Int, Int, Int, Int)

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    if(radius > 0) {
      val pixels = for {
        xCoord <- clamp(x - radius, 0, src.width - 1) to clamp(x + radius, 0, src.width - 1)
        yCoord <- clamp(y - radius, 0, src.height - 1) to clamp(y + radius, 0, src.height - 1)
      } yield src(xCoord, yCoord)
      val ((tr, tg, tb, ta), w) = pixels.map(x => ((red(x), green(x), blue(x), alpha(x)), 1)).reduce(sumRGBA)
      rgba(tr/w, tg/w, tb/w, ta/w)
    } else {
      src(x, y)
    }
  }

  def sumRGBA(a: (DRGBA, Int), b: (DRGBA, Int)): (DRGBA, Int) = {
    (a, b) match {
      case (((r1, g1, b1, a1), w1),((r2, g2, b2, a2), w2)) =>
        ((r1 + r2, g1 + g2, b1 + b2, a1 + a2), w1 + w2)
    }
  }

}
