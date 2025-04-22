class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m
  override def toString: String = data.map(_.mkString(",")) mkString("\n")

  def selectColumn(col: String): Dataset = {
    if (data.head.indexOf(col) != -1)
      Dataset(data.map(column => List(column(data.head.indexOf(col)))))
    else
      Dataset(List.empty[List[String]])
  }

  def selectColumns(cols: List[String]): Dataset = {
    def findColumnNumbers(list: List[String], indexes: List[Int], cols: List[String]): List[Int] = {
      if (cols.isEmpty || list.isEmpty)
        indexes.reverse
      else {
        if (list.indexOf(cols.head) != -1)
          findColumnNumbers(list, list.indexOf(cols.head) :: indexes, cols.drop(1)) 
        else findColumnNumbers(list, indexes, cols.drop(1))
      }
    }
    if (findColumnNumbers(data.head, List.empty[Int], cols).nonEmpty) {
      Dataset(data.map(row => findColumnNumbers(data.head, List.empty[Int], cols).map(row)))
    } else {
      Dataset(List.empty[List[String]])
    }
  }

  def split(percentage: Double): (Dataset, Dataset) = {
    def sorted = data.drop(1).sortBy(row => row.head)
    def counter: Int = math.ceil(1 / percentage).toInt - 1
    //sigur se putea rezolva mai elegant
    def counterfunc (k: Int, eval:List[List[String]], antre:List[List[String]], database: List[List[String]]): (List[List[String]], List[List[String]]) = {
      if (database.isEmpty)
        (antre, eval)
      else {
        if (k != counter)
          counterfunc(k + 1, eval, antre:+database.head, database.drop(1))
        else
          counterfunc(0, eval:+database.head, antre, database.drop(1))
      }
    }
    val (antre, eval) =  counterfunc(0, List.empty[List[String]], List.empty[List[String]], sorted)
    (new Dataset(this.getHeader::antre), new Dataset(this.getHeader::eval))
  }

  def size: Int = data.length - 1
  def getRows: List[List[String]] = data
  def getHeader: List[String] = data.head
}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val source = io.Source.fromFile(csv_filename)
    val lines = source.getLines().toList
    source.close()
    new Dataset(lines.map(_.split(',').toList))
  }
  def apply(ds: List[List[String]]): Dataset = new Dataset(ds)
}
