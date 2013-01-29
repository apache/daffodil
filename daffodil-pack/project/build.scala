import sbt._

object DaffodilBuild extends Build {

  var s = Defaults.defaultSettings

  lazy val example = Project(id = "daffodil-example", base = file("."), settings = s)

  val packFiles = Seq(
    "bin",
    "build.sbt"
  )

  lazy val packTask = TaskKey[Unit]("pack", "Generate distributable pack files", KeyRanks.APlusTask)
  lazy val packTaskSettings = packTask <<= (tarTask, zipTask) map { (t, z) => () }
  s ++= packTaskSettings

  lazy val tarTask = TaskKey[Unit]("pack-tar", "Generate a distributable tar file", KeyRanks.APlusTask)
  lazy val tarTaskSettings = tarTask <<= (Keys.fullClasspath in Runtime, Keys.name, Keys.version, Keys.target) map { (cp, n, v, t) =>
    val outBase = "%s-%s".format(n, v)
    packFiles(t, outBase, packFiles, cp.files, "tar -jcf", "tar.bz2")
  }
  s ++= tarTaskSettings

  lazy val zipTask = TaskKey[Unit]("pack-zip", "Generate a distributable zip file", KeyRanks.APlusTask)
  lazy val zipTaskSettings = zipTask <<= (Keys.fullClasspath in Runtime, Keys.name, Keys.version, Keys.target) map { (cp, n, v, t) =>
    val outBase = "%s-%s".format(n, v)
    packFiles(t, outBase, packFiles, cp.files, "zip -r", "zip")
  }
  s ++= zipTaskSettings

  // Unforunately, we can't completly reuse IO.copy, for two reason:
  // 1) IO.copy doesn't copy recurisvely (IO.copyDirectory does though)
  // 2) IO.copy doesn't retain permissions (all we care about is the execute bit)
  def copy(source: File, dest: File) {
    def copyFile(source: File, dest: File) {
      IO.copyFile(source, dest)
      dest.setExecutable(source.canExecute)
    }

    def copyDirectory(source: File, dest: File) {
      IO.createDirectory(dest)
      val files = source.listFiles
      files.foreach(f => {
        val newFile = new File(dest, f.name)
        if (f.isFile) {
          copyFile(f, newFile)
        }
        if (f.isDirectory) {
          copyDirectory(f, newFile)
        }
      })
    }

    if (source.isFile) {
      copyFile(source, dest)
    }

    if (source.isDirectory) {
      copyDirectory(source, dest)
    }
  }


  def packFiles(outDir: File, outBase: String, inFiles: Seq[String], cpFiles: Seq[File], packCmd: String, packExt: String) {
    IO.withTemporaryDirectory(dir => {
      val daffodilDir = new File(dir, outBase)
      inFiles.foreach(f => {
        val source = new File(f)
        val dest = new File(daffodilDir, f)

        copy(source, dest)

      })

      val libDir = new File(daffodilDir, "lib")
      IO.createDirectory(libDir)
      cpFiles.filter(_.isFile).foreach(f => {
        val dest = new File(libDir, f.name)
        IO.copyFile(f, dest)
      })

      val outFile = new File(outDir, "%s.%s".format(outBase, packExt))
      if (outFile.exists) {
        if (!outFile.delete) {
          sys.error("Failed to remove old file: %s".format(outFile))
        }
      }
      val r = java.lang.Runtime.getRuntime()
      val p = r.exec("%s %s %s".format(packCmd, outFile.getAbsoluteFile(), daffodilDir.getName), null, new File(daffodilDir, ".."))
      p.waitFor()
      val ret = p.exitValue()
      if (ret != 0) {
        sys.error("Failed to pack file: %s".format(outFile))
      }

    })
  }
}
