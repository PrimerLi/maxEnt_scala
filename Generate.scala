import scala.math._
import java.io._
import scala.util.Random

object Generate
{
    def gaussian(x: Double, mu: Double, sigma: Double): Double = 
    {
        return 1.0/(sqrt(2*Pi)*sigma)*exp(-(x - mu)*(x - mu)/(2*sigma*sigma))
    }

    def spectral(omega: Double): Double = 
    {
        return 0.55 * gaussian(omega, -1.2, 0.25) + 0.45 * gaussian(omega, 0.9, 0.3)
    }

    def A(omega: Array[Double]): Vector = 
    {
        val length = omega.length
        val result = new Vector(length)
        for (i <- 0 to length - 1)
        {
            result.setElement(i, spectral(omega(i)))
        }
        return result
    }

    val random = new Random()
    val error = 0.02
    def GReal(omega_n:Array[Double], omega:Array[Double]): Vector = 
    {
        val result = Kernel.KReal(omega_n, omega)*A(omega)
        val noise = new Vector(result.getLength)
        for (i <- 0 to result.getLength() - 1)
        {
            noise.setElement(i, error*random.nextGaussian())
        }
        return result + noise
    }

    def GImag(omega_n:Array[Double], omega:Array[Double]): Vector = 
    {
        val result = Kernel.KImag(omega_n, omega)*A(omega)
        val noise = new Vector(result.getLength)
        for (i <- 0 to noise.getLength - 1)
        {
            noise.setElement(i, error*random.nextGaussian())
        }
        return result + noise
    }

    def main(args: Array[String]): Unit = 
    {
        var writer = new PrintWriter(new File("A.txt"))
        val n_omega = 101
        val niom = 60
        val omegaLower = -5.0
        val omegaUpper = -omegaLower
        val domega = (omegaUpper - omegaLower)/(n_omega - 1).toDouble
        val omega: Array[Double] = new Array[Double](n_omega)
        for (i <- 0 to omega.length - 1)
        {
            omega(i) = omegaLower + i*domega
        }
        for (i <- 0 to omega.length - 1)
        {
            writer.write(omega(i).toString + "  " + A(omega).getElement(i).toString + "\n")
        }
        writer.close()
        val omega_n:Array[Double] = new Array[Double](niom)
        val beta = 10.0
        for (i <- 0 to niom-1)
        {
            omega_n(i) = (2*i+1)*Pi/beta
        }
        writer = new PrintWriter(new File("G_real.txt"))
        for (i <- 0 to niom-1)
        {
            writer.write(omega_n(i).toString + "  " + GReal(omega_n, omega).getElement(i).toString  + "\n")
        }
        writer.close()
        writer = new PrintWriter(new File("G_imag.txt"))
        for (i <- 0 to niom-1)
        {
            writer.write(omega_n(i).toString + "  " + GImag(omega_n, omega).getElement(i).toString + "\n")
        }
        writer.close()
        writer = new PrintWriter(new File("error.txt"))
        writer.write(error.toString)
        writer.close()
    }
}
