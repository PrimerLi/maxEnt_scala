import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object ReadFile
{
    def readGreenFunction(fileName: String): (Vector, Vector) = 
    {
        val G = new ArrayBuffer[Double]()
        val omega = new ArrayBuffer[Double]()
        val source = Source.fromFile(fileName)
        for (line <- source.getLines)
        {
            val temp = line.split("  ")
            omega.append(temp(0).toDouble)
            G.append(temp(1).toDouble)
        }
        source.close()
        return (new Vector(omega), new Vector(G))
    }

    def readSpectralFunction(fileName: String): (Vector, Vector) = 
    {
        return readGreenFunction(fileName)
    }

    def main(args: Array[String]): Unit = 
    {
        val (omega, g) = readGreenFunction("G_imag.txt")
        for (i <- 0 to omega.getLength - 1)
        {
            print(omega.getElement(i) + "  " + g.getElement(i) + "\n")
        }
    }
}
