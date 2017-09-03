import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.collection.immutable.Range
import scala.util.control.Breaks._

class Matrix
{
    private var matrix: ArrayBuffer[ArrayBuffer[Double]] = new ArrayBuffer[ArrayBuffer[Double]]()
    private var row: Int = 0
    private var col: Int = 0

    def this(row: Int, col: Int)
    {
        this()
        if (row <= 0 || col <= 0)
        {
            println("Matrix dimension should be a positive integer. ")
            System.exit(-1)
        }
        this.row = row
        this.col = col
        for (i <- 0 to row - 1)
        {
            var list:ArrayBuffer[Double] = new ArrayBuffer[Double]()
            for (j <- 0 to col - 1)
            {
                list.append(0)
            }
            matrix.append(list)
        }
    }

    def this(parameter: ArrayBuffer[ArrayBuffer[Double]])
    {
        this()
        this.row = parameter.size
        this.col = parameter(0).size
        for (i <- 0 to row - 1)
        {
            var list: ArrayBuffer[Double] = new ArrayBuffer[Double]()
            for (j <- 0 to col - 1)
            {
                list.append(parameter(i)(j))
            }
            this.matrix.append(list)
        }
    }

    def printMatrix(): Unit = 
    {
        println(row + "  " + col)
        for (i <- 0 to row - 1)
        {
            for (j <- 0 to col - 1)
            {
                print(matrix(i)(j) + "  ")
            }
            print("\n")
        }
    }
    override def toString(): String = 
    {
        //var result: String = "row = " + row.toString + ", col = " + col.toString + "\n"
        var result: String = ""
        for (i <- 0 to row - 1)
        {
            for (j <- 0 to col - 1)
            {
                result = result + matrix(i)(j).toString + "  "
            }
            result = result + "\n"
        }
        return result
    }

    def add(that: Matrix): Matrix = 
    {
        if (this.row != that.row || this.col != that.col)
        {
            println("Matrix dimensions are incompatible. ")
            println(this.row + "  " + this.col)
            println(that.row + "  " + that.col)
            System.exit(-1)
        }
        var result: Matrix = new Matrix(this.row, this.col)
        for (i <- 0 to row - 1)
        {
            for (j <- 0 to col - 1)
            {
                result.matrix(i)(j) = this.matrix(i)(j) + that.matrix(i)(j) 
            }
        }
        return result
    }

    def + (that: Matrix): Matrix = this.add(that)

    def subtract(that: Matrix): Matrix = 
    {
        if (this.row != that.row || this.col != that.col)
        {
            println("Matrix dimensions are incompatible. ")
            println(this.row + "  " + this.col)
            println(that.row + "  " + that.col)
            System.exit(-1)
        }
        var result: Matrix = new Matrix(this.row, this.col)
        for (i <- 0 to row - 1)
        {
            for (j <- 0 to col - 1)
            {
                result.matrix(i)(j) = this.matrix(i)(j) - that.matrix(i)(j)
            }
        }
        return result
    }

    def - (that: Matrix) = this.subtract(that)

    def product(that: Matrix): Matrix = 
    {
        if (this.col != that.row)
        {
            println("Matrix dimensions are incompatible. ")
            println(this.row + "  " + this.col)
            println(that.row + "  " + that.col)
            System.exit(-1)
        }
        var result: Matrix = new Matrix(this.row, that.col)
        for (i <- 0 to this.row - 1)
        {
            for (j <- 0 to that.col - 1)
            {
                var s: Double = 0.0
                for (k <- 0 to this.col - 1)
                {
                    s = s + this.matrix(i)(k) * that.matrix(k)(j)
                }
                result.matrix(i)(j) = s
            }
        }
        return result
    }

    def * (that: Matrix): Matrix = this.product(that)

    def product(that: Vector): Vector = 
    {
        if (this.col != that.getLength())
        {
            println("Matrix dimensions are incompatible. ")
            System.exit(-1)
        }
        var result: Vector = new Vector(this.row)
        for (i <- 0 to this.row - 1)
        {
            var sum: Double = 0.0
            for (j <- 0 to this.col - 1)
            {
                sum = sum + this.matrix(i)(j) * that.getElement(j)
            }
            result.setElement(i, sum)
        }
        return  result
    }

    def * (that: Vector): Vector = this.product(that)

    def getRow(): Int = 
    {
        return this.row
    }

    def getCol(): Int = 
    {
        return this.col
    }

    def getElement(i: Int, j: Int): Double = 
    {
        if (i < 0 || i >= row || j < 0 || j >= col)
        {
            println("Index out of bounds. ")
            System.exit(-1)
        }
        return this.matrix(i)(j)
    }

    def setElement(i: Int, j: Int, value: Double): Unit = 
    {     
        if (i < 0 || i >= row || j < 0 || j >= col)
        {
            println("Index out of bounds. ")
            System.exit(-1)
        }
        this.matrix(i)(j) = value
    }

    def trace(): Double = 
    {
        var sum = 0.0
        if (this.row != this.col)
        {
            println("You cannot calculate the trace of a non-square matrix. ")
            System.exit(-1)
        }
        for (i <- 0 to this.row - 1)
        {
            sum = sum + this.matrix(i)(i)
        }
        return sum
    }

    def scale(factor: Double): Matrix = 
    {
        var result: Matrix = new Matrix(this.row, this.col)
        for (i <- 0 to this.row - 1)
        {
            for (j <- 0 to this.col - 1)
            {
                result.matrix(i)(j) = factor*this.matrix(i)(j)
            }
        }
        return result
    }

    def * (factor: Double): Matrix = this.scale(factor)

    def transpose():Matrix = 
    {
        var result:Matrix = new Matrix(this.col, this.row)
        for (j <- 0 to this.col - 1)
        {
            for (i <- 0 to this.row - 1)
            {
                result.matrix(j)(i) = this.matrix(i)(j)
            }
        }
        return result
    }

    def norm(): Double = 
    {
        return sqrt(this.product(this.transpose()).trace())
    }

    def QRDecomposition(): (Matrix, Matrix) = 
    {
        if (this.row != this.col)
        {
            println("Only square matrix can be QR decomposed. ")
            System.exit(-1)
        }
        var dimension = this.row
        var Q: Matrix = new Matrix(dimension, dimension)
        var R: Matrix = new Matrix(dimension, dimension)
        var columns: ArrayBuffer[Vector] = new ArrayBuffer[Vector]()
        for (j <- 0 to dimension-1)
        {
            var temp: Vector = new Vector(dimension)
            for (i <- 0 to dimension - 1)
            {
                temp.setElement(i, this.matrix(i)(j))
            }
            columns.append(temp)
        }
        //var orthonormalized: ArrayBuffer[Vector] = new ArrayBuffer[Vector]()
        for (i <- 0 to dimension - 1)
        {
            var projection: Vector = new Vector(dimension)
            for (j <- 0 to i-1)
            {
                projection = projection.add(columns(j).scale(columns(i).innerProduct(columns(j))))
            }
            columns(i) = (columns(i).subtract(projection)).normalize()
        }
        for (i <- 0 to dimension - 1)
        {
            for (j <- 0 to dimension - 1)
            {
                Q.setElement(i, j, columns(j).getElement(i))
            }
        }
        def identityMatrix(dimension: Int): Matrix = 
        {
            var result: Matrix = new Matrix(dimension, dimension)
            for (i <- 0 to dimension-1)
            {
                result.matrix(i)(i) = 1.0
            }
            return result
        }

        def isOrthogonal(m: Matrix): Boolean = 
        {
            if (m.row != m.col)
            {
                println("Only square matrix could be orthogonal. ")
                return false
            }
            val dimension: Int = m.row
            var id: Matrix = identityMatrix(dimension)
            val eps = 1.0e-8
            return ((m.transpose().product(m)).subtract(id)).norm() < eps
        }

        if (!isOrthogonal(Q))
        {
            println("The Q matrix is not orthogonal. Q = ")
            println(Q)
            println("Q^T * Q = ")
            println(Q.transpose().product(Q))
            System.exit(-1)
        }
        R = Q.transpose().product(this)
        return (Q, R)
    }
    
    def inverse(): Matrix = 
    {
        if (this.row != this.col)
        {
            println("Only square matrix is invertible. ")
            System.exit(-1)
        }
        val dimension: Int = this.row
        var (q: Matrix, r: Matrix) = this.QRDecomposition()
        var rInverse: Matrix = new Matrix(dimension, dimension)
        var range: Range = Range(0, dimension)
        var augmented: Matrix = new Matrix(dimension, dimension*2)
        val identityMatrix: Matrix = new Matrix(dimension, dimension)
        for (i <- 0 to dimension-1)
        {
            identityMatrix.matrix(i)(i) = 1.0
        }
        for (i <- 0 to dimension - 1)
        {
            for (j <- 0 to 2*dimension - 1)
            {
                if (j < dimension)
                {
                    augmented.matrix(i)(j) = r.matrix(i)(j)
                }
                else
                {
                    augmented.matrix(i)(j) = identityMatrix.matrix(i)(j - dimension)
                }
            }
        }
        println("augmented matrix before inversion: ")
        println(augmented)
        for (i <- range.reverse)
        {
            for (j <- Range(0, i).reverse)
            {
                if (abs(augmented.matrix(i)(i)) < 1.0e-16)
                {
                    println("Your matrix may be singular. ")
                    System.exit(-1)
                }
                var factor: Double = -augmented.matrix(j)(i)/augmented.matrix(i)(i)
                for (colNumber <- 0 to 2*dimension-1)
                {
                    augmented.matrix(j)(colNumber) += factor*augmented.matrix(i)(colNumber)
                }
            }
            var factor: Double = 1.0/augmented.matrix(i)(i)
            for (colNumber <- 0 to 2*dimension - 1)
            {
                augmented.matrix(i)(colNumber) *= factor
            }
        }
        println("augmented matrix after inversion: ")
        println(augmented)
        for (i <- 0 to dimension-1)
        {
            for (j <- 0 to dimension-1)
            {
                rInverse.matrix(i)(j) = augmented.matrix(i)(j+dimension)
            }
        }
        return rInverse.product(q.transpose())
    }

    def eigensystem(): (Vector, Matrix) = 
    {
        if (this.row != this.col)
        {
            println("Only square matrix has eigensystem. ")
            System.exit(-1)
        }
        var dimension: Int = this.row
        var eigenvalues: Vector = new Vector(dimension)
        var qMatrices: ArrayBuffer[Matrix] = new ArrayBuffer[Matrix]()
        var iterationMax:Int = 30
        var count: Int = 0
        for (i <- 0 to dimension - 1)
        {
            eigenvalues.setElement(i, this.matrix(i)(i))
        }
        var updated: Matrix = this
        val eps: Double = 1.0e-20
        breakable{
            while(true)
            {
                //println("count = " + count + "\n eigenvalues = " + eigenvalues)
                //println("updated : ")
                //println(updated)
                count = count + 1
                if (count > iterationMax) break
                var (q:Matrix, r:Matrix) = updated.QRDecomposition()
                qMatrices.append(q)
                updated = r.product(q)
                for (i <- 0 to dimension-1)
                {
                    eigenvalues.setElement(i, updated.matrix(i)(i))
                }
                if ((updated.subtract(q.product(r))).norm() < eps) break
            }
        }
        var oMatrix: Matrix = new Matrix(dimension, dimension)
        for (i <- 0 to dimension-1)
        {
            oMatrix.matrix(i)(i) = 1.0
        }
        def f(m: Matrix):Boolean = 
        {
            return true
        }
        for (i <- 0 to qMatrices.size - 1)
        {
            oMatrix = oMatrix.product(qMatrices(i))
        }
        return (eigenvalues, oMatrix)
    }
    
}
