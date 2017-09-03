import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.math._
import java.io._
import sys.process._
import scala.language.postfixOps

object Main
{
    def main(args:Array[String]): Unit =
    {
        if (args.length != 3)
        {
            println("alphaInitial = args(0), alphaNumber = args(1), a = args(2). ")
            System.exit(-1)
        }

        var (omega_n, g_real) = ReadFile.readGreenFunction("G_real.txt")
        var (_, g_imag) = ReadFile.readGreenFunction("G_imag.txt")
        var error: Double = 0
        val source = Source.fromFile("error.txt")
        for (line <- source.getLines)
        {
            error = line.toDouble
        }
        source.close()

        val alphaInitial = args(0).toDouble
        val alphaNumber = args(1).toInt
        val alphaValues = new ArrayBuffer[Double]()
        var a = args(2).toDouble
        for (i <- 0 to alphaNumber - 1)
        {
            alphaValues.append(alphaInitial*exp(-a*i))
        }

        val alphaWriter = new PrintWriter(new File("alpha.txt"))
        for (alpha <- alphaValues)
        {
            alphaWriter.write(alpha.toString + "\n")
        }
        alphaWriter.close()

        val modelFile = new File("model.txt")
        if (!modelFile.exists())
        {
            val writer = new PrintWriter(modelFile)
            def gauss(x: Double, mu: Double, sigma: Double):Double = 
            {
                return 1.0/(sqrt(2*Pi)*sigma)*exp(-(x-mu)*(x-mu)/(2*sigma*sigma))
            }
            val omegaUpper:Double = 5
            val omegaLower = -omegaUpper
            val length = 101
            val domega = (omegaUpper - omegaLower)/(length-1)
            val omega: ArrayBuffer[Double] = new ArrayBuffer[Double]()
            for (i <- 0 to length-1)
            {
                omega.append(omegaLower + i*domega)
            }
            for (i <- 0 to length-1)
            {
                writer.write(omega(i).toString + "  " + gauss(omega(i), 0, 1) + "\n")
            }
            writer.close()
        }
        var (omega, model) = ReadFile.readSpectralFunction("model.txt")
        var spectral = model
        def printSpectral(omega: Vector, spectral:Vector, outputFile: String):Unit = 
        {
            val length = omega.getLength()
            val writer = new PrintWriter(new File(outputFile))
            for (i <- 0 to length-1)
            {
                writer.write(omega.getElement(i).toString + "  " + spectral.getElement(i).toString + "\n")
            }
            writer.close()
        }
        val K_real: Matrix = Kernel.KReal(omega_n, omega)
        val K_imag: Matrix = Kernel.KImag(omega_n, omega)
        val maxEnt = new MaxEnt()
        for (alpha <- alphaValues)
        {
            println("alpha = " + alpha)
            spectral = maxEnt.newton(alpha, spectral, model, omega, g_real, g_imag, K_real, K_imag, error)
            val outputFile: String = "A_" + alpha.toString + ".txt"
            printSpectral(omega, spectral, outputFile)
            "mv " + outputFile + "  model.txt" ! 
        }
    }
}
