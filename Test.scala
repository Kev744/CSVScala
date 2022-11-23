import scala.annotation.tailrec
import scala.io.BufferedSource

object Test extends App{

  def readCSV(lines: Iterable[String], delimiter : String): Iterable[Array[String]] = {
    // drop CSV header
    val size : Int = lines.head.split(delimiter).length
    val data: Iterable[String] = lines.drop(1)

    val rows: Iterable[Array[String]] =
      data.map { line =>
        //TODO cleaning line and separate fields by the comma character
        val row: Array[String] = line.split(delimiter).map(x => x.trim)

        // cleansing: if fields are missing, we pad row with empty strings
        row.padTo(size, "")
      }
    rows
  }

  @tailrec
  def display(partition : Iterator[Array[String]], acc : String = ""): String =  partition match {
    case x if !x.hasNext => acc
    case x => display(x, acc + x.next.mkString("Array(", ", ", ")") + "\n")
  }
  import scala.io.Source

  val file: BufferedSource = Source.fromFile("resources/ski_stations_ratings.csv")
  val part : Iterable[Array[String]] = readCSV(file.getLines().to(Iterable), ",")

  print(display(part.iterator))


}
