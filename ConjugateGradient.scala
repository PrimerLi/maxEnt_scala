import scala.util.Random
import scala.util.control.Breaks._
import DoubleUtils._

object ConjugateGradient
{
    def createPositiveDefiniteMatrix(dimension: Int): Matrix = 
    {
        val random = new Random()
        val A = new Matrix(dimension, dimension)
        for (i <- 0 to dimension-1)
        {
            for (j <- 0 to dimension-1)
            {
                A.setElement(i, j, random.nextInt(dimension*2))
            }
        }
        return A.transpose()*A
    }

    def solveLinearSystem(A: Matrix, b: Vector): Vector = 
    {
        val row = A.getRow()
        val col = A.getCol()
        val length = b.getLength()
        if (row != col || row != length)
        {
            println("Matrix dimensions are incompatible. ")
            System.exit(-1)
        }
        val dimension = row
        var x = new Vector(dimension)
        var count = 0
        var iterationMax = 5*dimension
        val eps:Double = 1.0e-32
        var r0: Vector = new Vector(dimension)
        var p: Vector = new Vector(dimension)
        r0 = b - A*x
        p = r0
        var r1 = r0
        breakable{
            var alpha:Double = 0.0
            var beta:Double = 0.0
            var error:Double = 0.0
            while(true)
            {
                count = count + 1
                if (count > iterationMax) break
                alpha = r0*r0/(p*(A*p))
                x = x + alpha*p
                r1 = r0 - alpha*(A*p)
                error = r1.norm()
                //println("count = " + count + ", error = " + error)
                if (error < eps) break
                beta = r1*r1/(r0*r0)
                p = r1 + beta*p
                r0 = r1
            }
        }
        return x
    }

    def main(args: Array[String]): Unit = 
    {
        if (args.length != 1)
        {
            println("matrix dimension = args(0)")
            System.exit(-1)
        }
        val dimension = args(0).toInt
        val A = createPositiveDefiniteMatrix(dimension)
        /*for (i <- 0 to dimension - 1)
        {
            for (j <- 0 to dimension - 1)
            {
                A.setElement(i, j, -A.getElement(i, j))
            }
        }*/
        println("A = ")
        println(A)
        val b = new Vector(dimension)
        val random = new Random()
        for (i <- 0 to dimension-1)
        {
            b.setElement(i, random.nextInt(dimension))
        }
        println("b = ")
        println(b)
        val x = solveLinearSystem(A, b)
        println("Solution of A*x = b is ")
        println(x)
        println("error = " + (A*x - b).norm())
        println("Inverse error = " + (A*(A.inverse()*b) - b).norm())
    }
}
