import os.ProcessOutput.Readlines

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

case class Locked[A <: AnyRef](private val it : A) {
  def apply[B](f: A => B): B = it.synchronized {
    f(it)
  }
}

object thread {
  def apply[A](f: => A) : Future[A] = {
    val p = Promise[A]()
    val t = new Thread {
      override def run(): Unit = {
        f
      }
    }
    t.setDaemon(true)
    t.start()
    p.future
  }
}

object main {

  private val names = (0 to 10000).map(n => f"$n%04d")
  private val n_events = names.size * 2 + 1

  def main(args: Array[String]): Unit = {
    val data = os.pwd / "data"

    os.remove.all(data)
    os.makeDir(data)

    val lines = Locked(mutable.Buffer[String]())
    def addLine(line: String): Unit = {
      lines(_.append(line))
    }
    val watching = Seq[String](
      //"create",
      "delete",
      //"modify",
      //"move"
    )

    val flags = Seq("-r","-q","-m") ++ watching.flatMap(x => Seq("-e",x))



    val p = os.proc("inotifywait" +: flags :+ "data").spawn(stdout = Readlines(addLine))

    //thread {
    //  val watch = os.proc("inotifywait" +: flags :+ "data").call(timeout = 3000).out.lines
    //  watch.foreach(println)
    //}

    val root_dir = data / "r"
    os.makeDir(root_dir)

    names.foreach { s =>
      val dir_name = s"d_$s"
      val dir_path = root_dir / dir_name
      val file_path = dir_path / s"file_$s"

      os.makeDir.all(dir_path)
      os.write(file_path, s)
    }

    os.remove.all(root_dir)

    var iter = 0
    while ((iter < 10000) && (n_events > lines(_.size))) {
      Thread.sleep(1)
      iter += 1
    }

    p.destroy

    if (n_events == lines(_.size)) {
      println("happy")
    } else {
      lines(_.toList).foreach(println)
      println("sad")
      System.exit(1)
    }

  }

}
