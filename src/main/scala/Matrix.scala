type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {
  //am facut- o cu .transpose, dar am auzit ca trebuie facuta ca la curs
  def transpose: Matrix = {
    def transposeHelper(m: List[List[Double]]):List[List[Double]] =
      m match {
        case Nil :: _ => Nil
        case _ => m.map(_.head) :: transposeHelper(m.map(_.tail))
      }
    m match
      case None => new Matrix(None)
      case Some(m) => new Matrix(Some(transposeHelper(m)))
  }

  def map(f: Double => Double): Matrix = {
    m match
      case None => new Matrix(None)
      case Some(m) => new Matrix(Some(m.map(_.map(f))))
  }
  //aici m-am luat dupa functia product din curs in mare parte, sper sa nu fie o problema
  def *(other: Matrix): Matrix = {
    (this.data, other.data) match
      case (Some(mat1), Some(mat2)) if mat1.head.size == mat2.size =>
       new Matrix(Some(mat1.map(line => mat2.transpose.map(col => line.zip(col).map(pair => pair._1 * pair._2).sum)))) 
      case _ => new Matrix(None)
  }
  
  def ++(x: Double): Matrix = {
    m match
      case None => new Matrix(None)
      case Some(mat) => new Matrix(Some(mat.map(_:+x)))
  }

  def -(other: Matrix): Matrix = {
    (this.m, other.data) match
      case (Some(mat1), Some(mat2)) if mat1.length == mat2.length && mat1.head.length == mat2.head.length =>
        new Matrix(Some(mat1.zip(mat2).map((row1, row2) => row1.zip(row2).map((elem1, elem2) => elem1 - elem2))))
      case _ => new Matrix(None)
  }

  def data: Option[Mat] = m

  def height: Option[Int] = data.map(_.size)

  def width: Option[Int] = data.map(_.head.size)

  override def toString: String = m.map(_.mkString(" ")).mkString("\n")
}

object Matrix {
  def apply(data: Mat): Matrix = new Matrix(Some(data))
  def apply(data: Option[Mat]): Matrix = new Matrix(data)
  def apply(dataset: Dataset): Matrix = new Matrix(Some(dataset.data.tail.map(_.map(_.toDouble))))
}
