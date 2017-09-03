import java.io._

object CalculateGreenFunction
{
    def printFile(x:Vector, y:Vector, output:String): Unit = 
    {
        val writer = new PrintWriter(new File(output))
        assert(x.getLength() == y.getLength())
        for (i <- 0 to x.getLength() - 1)
        {
            writer.write(x.getElement(i).toString + "  " + y.getElement(i).toString + "\n")
        }
        writer.close()
    }
    def main(args: Array[String]):Unit = 
    {
        val (omega, spectral) = ReadFile.readSpectralFunction("model.txt")
        val (omega_n, _) = ReadFile.readGreenFunction("G_real.txt")
        val G_real: Vector = Kernel.KReal(omega_n, omega)*spectral
        val G_imag: Vector = Kernel.KImag(omega_n, omega)*spectral
        printFile(omega_n, G_real, "G_real_test.txt")
        printFile(omega_n, G_imag, "G_imag_test.txt")
    }
}
