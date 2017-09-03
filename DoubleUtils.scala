object DoubleUtils
{
    implicit class DoubleImprovements(factor: Double)
    {
        def * (vector: Vector) = vector*factor
        def * (matrix: Matrix) = matrix*factor
    }
}
