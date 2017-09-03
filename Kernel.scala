object Kernel
{
    private def square(a: Double) = a*a
    def KReal(omega_n:Array[Double], omega:Array[Double]):Matrix = 
    {
        val row = omega_n.length
        val col = omega.length
        val result = new Matrix(row, col)
        for (i <- 0 to row - 1)
        {
            for (j <- 0 to col - 1)
            {
                result.setElement(i, j, -omega(j)/(omega_n(i)*omega_n(i) + omega(j)*omega(j)))
            }
        }
        val domega = omega(1) - omega(0)
        return result*domega
    }

    def KReal(omega_n: Vector, omega: Vector): Matrix = 
    {
        val row: Int = omega_n.getLength()
        val col: Int = omega.getLength()
        val omega_n_Array = new Array[Double](row)
        val omega_Array = new Array[Double](col)
        for (i <- 0 to row - 1)
        {
            omega_n_Array(i) = omega_n.getElement(i)
        }
        for (i <- 0 to col - 1)
        {
            omega_Array(i) = omega.getElement(i)
        }
        return KReal(omega_n_Array, omega_Array)
    }

    def KImag(omega_n: Array[Double], omega: Array[Double]): Matrix = 
    {
        val row = omega_n.length
        val col = omega.length
        val result = new Matrix(row, col)
        for (i <- 0 to row - 1)
        {
            for (j <- 0 to col - 1)
            {
                result.setElement(i, j, -omega_n(i)/(omega_n(i)*omega_n(i) + omega(j)*omega(j)))
            }
        }
        val domega = omega(1) - omega(0)
        return result*domega
    }
    
    def KImag(omega_n: Vector, omega: Vector): Matrix = 
    {
        val row: Int = omega_n.getLength()
        val col: Int = omega.getLength()
        val omega_n_Array = new Array[Double](row)
        val omega_Array = new Array[Double](col)
        for (i <- 0 to row - 1)
        {
            omega_n_Array(i) = omega_n.getElement(i)
        }
        for (i <- 0 to col - 1)
        {
            omega_Array(i) = omega.getElement(i)
        }
        return KImag(omega_n_Array, omega_Array)
    }
}
