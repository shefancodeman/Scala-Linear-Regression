object Regression {

  def regression(dataset_file: String,
                 attribute_columns: List[String],
                 value_column: String,
                 test_percentage: Double,
                 alpha: Double,
                 gradient_descent_steps: Int): (Matrix, Double) = {
    val (antrenare, evaluare) = Dataset.apply(dataset_file).split(test_percentage)

    def Gradient_Descent(dataset: Matrix, W: Matrix, steps: Int = gradient_descent_steps, alpha: Double): Matrix = {
      if (steps > 0) {
        val eroare = dataset.*(W).-(Matrix.apply(antrenare.selectColumn(value_column))) //pas 1+2 combinat
        val gradient = dataset.transpose.*(eroare).map(_ / dataset.height.get)  //pas 3
        Gradient_Descent(dataset, W.-(gradient.map(_ * alpha)), steps - 1, alpha) //pas 4 - recurenta
      }
      else W
    }
    //am folosit o tona de variabile ca sa se inteleaga cam ce pas am facut si cum am facut
    val inceput: List[List[Double]] = List.fill(Matrix.apply(antrenare.selectColumns(attribute_columns)).++(1).width.get)(List(0.0))
    val W_output = Gradient_Descent(Matrix.apply(antrenare.selectColumns(attribute_columns)).++(1), Matrix.apply(Some(inceput)), gradient_descent_steps, alpha)
    //aici este pasul 5
    //eval este dataset, inceput2 este W, err pasul 1&2 eval value 3&4 intr-un singur pas
    val eval = Matrix.apply(evaluare.selectColumns(attribute_columns)).++(1)
    val inceput2 = Matrix.apply(Some(List.fill(Matrix.apply(evaluare.selectColumns(attribute_columns)).++(1).width.get)(List(0.0))))
    val err = eval.*(inceput2).-(Matrix.apply(evaluare.selectColumn(value_column)))
    val eval_value = Matrix(eval.transpose.*(err).map(_ / inceput2.height.get).data)
    (W_output, W_output.-(eval_value).map(math.abs(_)).data.get.flatten.foldLeft(0.0)(_+_))
  }
}

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(Regression.regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
