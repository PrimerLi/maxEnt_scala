import scala.math._
import scala.util.control.Breaks._
import scala.io.Source

class MaxEnt
{
    def entropy(A: Vector, model: Vector, omega: Vector): Double = 
    {
        val length = A.getLength()
        if (length != model.getLength() || length != omega.getLength())
        {
            println("A, model, and omega must have the same length. ")
            System.exit(-1)
        }
        var sum = 0.0
        val domega = omega.getElement(1) - omega.getElement(0)
        for (i <- 0 to length-1)
        {
            sum = sum + domega*(A.getElement(i)*log(abs(A.getElement(i))/model.getElement(i)))
        }
        return -sum
    }

    def chi(G_real: Vector, G_imag: Vector, K_real: Matrix, K_imag: Matrix, A: Vector, error: Double): Double =
    {
        var result = 0.0
        result = result + (G_real - K_real * A)*(G_real - K_real * A)
        result = result + (G_imag - K_imag * A)*(G_imag - K_imag * A)
        result = result*(1/error)
        return result
    }

    def Q(alpha: Double, A: Vector, model: Vector, omega: Vector, G_real: Vector, G_imag: Vector, K_real: Matrix, K_imag: Matrix, error: Double): Double =
    {
        return alpha*entropy(A, model, omega) - chi(G_real, G_imag, K_real, K_imag, A, error)
    }

    def gradient(alpha: Double, A: Vector, model: Vector, omega: Vector, G_real: Vector, G_imag: Vector, K_real: Matrix, K_imag: Matrix, error: Double):Vector = 
    {
        val length = A.getLength()
        if (length != model.getLength() || length != omega.getLength())
        {
            println("Length error. ")
            System.exit(-1)
        }
        var result: Vector = new Vector(length)
        val domega = omega.getElement(1) - omega.getElement(0)
        for (i <- 0 to length-1)
        {
            result.setElement(i, -alpha*(1 + log(abs(A.getElement(i))/model.getElement(i)))*domega)
        }
        result = result + K_real.transpose()*(1.0/error)*(G_real - K_real*A)
        result = result + K_imag.transpose()*(1.0/error)*(G_imag - K_imag*A)
        return result
    }

    def J(alpha: Double, omega: Vector, A: Vector, K_real: Matrix, K_imag: Matrix, error: Double):Matrix = 
    {
        val length: Int = omega.getLength()
        if (A.getLength() != length)
        {
            println("Length error. ")
            System.exit(-1)
        }
        var result: Matrix = new Matrix(length, length)
        val domega = omega.getElement(1) - omega.getElement(0)
        for (i <- 0 to length-1)
        {
            result.setElement(i, i, -alpha*domega/A.getElement(i))
        }
        result = result - K_real.transpose()*K_real*(1.0/error)
        result = result - K_imag.transpose()*K_imag*(1.0/error)
        return result
    }

    def newton(alpha: Double, A: Vector, model: Vector, omega: Vector, G_real: Vector, G_imag: Vector, K_real: Matrix, K_imag: Matrix, error: Double): Vector = 
    {
        val length = A.getLength()
        if (length != model.getLength() || length != omega.getLength())
        {
            println("Length error. ")
            System.exit(-1)
        }
        var f: Vector = new Vector(length)
        var Jacobian: Matrix = new Matrix(length, length)
        var resultSpectral = new Vector(length)
        resultSpectral = A
        var count:Int = 0
        var iterationMax:Int = 30
        var iterationError: Double = 0
        val eps: Double = 1.0e-10
        breakable{
            while(true)
            {
                count = count + 1
                if (count > iterationMax) break
                f = gradient(alpha, resultSpectral, model, omega, G_real, G_imag, K_real, K_imag, error)
                Jacobian = J(alpha, omega, resultSpectral, K_real, K_imag, error)
                //var diff = ConjugateGradient.solveLinearSystem(Jacobian, f)
                var diff = Jacobian.inverse()*f
                iterationError = diff.norm()
                println("count = " + count + ", Newton iteration error = " + iterationError)
                resultSpectral = resultSpectral - diff
                for (index <- 0 to resultSpectral.getLength - 1)
                {
                    if (resultSpectral.getElement(index) < 0)
                    {
                        resultSpectral.setElement(index, eps)
                    }
                }
                if (iterationError < eps) break
            }
        }
        return resultSpectral
    }
}
