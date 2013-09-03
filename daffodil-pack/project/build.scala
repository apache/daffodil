import sbt._

import java.io.OutputStream
import java.io.FileOutputStream
import java.io.FileInputStream
import java.io.BufferedOutputStream
import org.apache.commons.io.IOUtils
import org.apache.commons.compress.archivers.ArchiveOutputStream
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream
import org.apache.commons.compress.archivers.zip.ZipArchiveOutputStream
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorOutputStream


object DaffodilBuild extends Build {

  var s = Defaults.defaultSettings

  lazy val example = Project(id = "daffodil-example", base = file("."), settings = s)

  val packFileList = Seq(
    "bin" -> "/bin",
    "build.sbt" -> "/build.sbt",
    "README" -> "/README",
    "../daffodil-examples/src/test/resources/edu/illinois/ncsa/daffodil/" -> "/examples"
  )

  lazy val packTask = TaskKey[Unit]("pack", "Generate distributable pack files", KeyRanks.APlusTask)
  lazy val packTaskSettings = packTask <<= (tarTask, zipTask) map { (t, z) => () }
  s ++= packTaskSettings

  lazy val tarTask = TaskKey[Unit]("pack-tar", "Generate a distributable tar file", KeyRanks.APlusTask)
  lazy val tarTaskSettings = tarTask <<= (Keys.fullClasspath in Runtime, Keys.name, Keys.version, Keys.crossTarget) map { (cp, n, v, t) =>
    packFiles(t / "pack", n, v, packFileList, cp.files, "tar.bz2", createTarBZ2Archiver)
  }
  s ++= tarTaskSettings

  lazy val zipTask = TaskKey[Unit]("pack-zip", "Generate a distributable zip file", KeyRanks.APlusTask)
  lazy val zipTaskSettings = zipTask <<= (Keys.fullClasspath in Runtime, Keys.name, Keys.version, Keys.crossTarget) map { (cp, n, v, t) =>
    packFiles(t / "pack", n, v, packFileList, cp.files, "zip", createZipArchiver)
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

  def modifyBuildSBT(file: File, version: String) {
    val DaffodilDepLine = """(\s+)//(.*)XXX_VERSION_XXX(.*)""".r
    val VersionLine = """//(.*)XXX_VERSION_XXX(.*)""".r
    val lines = scala.io.Source.fromFile(file).getLines
    val modifiedLines = lines.map { l =>
      l match {
        case DaffodilDepLine(indent, pre, post) => indent + pre + version + post
        case VersionLine(pre, post) => pre + version + post
        case _ => l
      }
    }.toList

    val fw = new java.io.FileWriter(file)
    val bw = new java.io.BufferedWriter(fw)
    modifiedLines.foreach( l => {
      bw.write(l)
      bw.newLine()
    })
    bw.close()
  }

  def packFiles(outDir: File, name: String, version: String, inFiles: Seq[(String,String)], cpFiles: Seq[File], packExt: String, createArchiver: (OutputStream) => ArchiveOutputStream) {
    outDir.mkdirs()
    val outBase = "%s-%s".format(name, version)
    IO.withTemporaryDirectory(dir => {
      val daffodilDir = new File(dir, outBase)
      inFiles.foreach { case (fs, fd) => {
        val source = new File(fs)
        val dest = new File(daffodilDir, fd)

        copy(source, dest)
        
        // Special case for build.sbt
        if (fs == "build.sbt") {
          modifyBuildSBT(dest, version)
        }
      }}

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

      val fOut = new FileOutputStream(outFile)
      val bOut = new BufferedOutputStream(fOut)
      val aOut = createArchiver(bOut)

      val base = daffodilDir.getParentFile.toURI.toString

      try {
        addFileToArchive(aOut, daffodilDir, base)
      } catch {
        case e: Exception => sys.error("Failed to pack file: %s".format(outFile))
      } finally {
        aOut.close
        bOut.close
        fOut.close
      }

    })
  }

  def createTarBZ2Archiver(os: OutputStream): ArchiveOutputStream = {
    val bz2 = new BZip2CompressorOutputStream(os)
    val tar = new TarArchiveOutputStream(bz2)
    tar
  }

  def createZipArchiver(os: OutputStream): ArchiveOutputStream = {
    val zip = new ZipArchiveOutputStream(os)
    zip
  }

  def addFileToArchive(aOut: ArchiveOutputStream, f: File, base: String) {
    if (f.isFile) {
      val ae = aOut.createArchiveEntry(f, f.toURI.toString.replace(base, ""))
      aOut.putArchiveEntry(ae)
      IOUtils.copy(new FileInputStream(f), aOut)
      aOut.closeArchiveEntry()
    } else if (f.isDirectory) {
      val children = f.listFiles
      children.foreach { child => addFileToArchive(aOut, child, base) }
    }
  }

  def exec(cmd: String): Seq[String] = {
    val r = java.lang.Runtime.getRuntime()
    val p = r.exec(cmd)
    p.waitFor()
    val ret = p.exitValue()
    if (ret != 0) {
      sys.error("Command failed: " + cmd)
    }
    val is = p.getInputStream
    val res = scala.io.Source.fromInputStream(is).getLines()
    res.toSeq
  }

  // get the version from the latest tag
  s ++= Seq(Keys.version := {
    val describe = exec("git describe --long HEAD")
    assert(describe.length == 1)
    
    val DescribeRegex = """^(.+)-(.+)-(.+)$""".r
    val res = describe(0) match {
      case DescribeRegex(taggedVersion, "0", hash) => {
        // we are on a tag, build a tag release
        val status = exec("git status --porcelain")
        if (status.length > 0) {
          taggedVersion + "-SNAPSHOT"
        } else {
          taggedVersion
        }
      }
      case DescribeRegex(version, _, hash) => {
        // not on a tag

        // get the current branch
        val branch = exec("git rev-parse --abbrev-ref HEAD")
        assert(branch.length == 1)
        val VersionBranchRegex = """^(\d+\.\d+\.\d+)$""".r
        branch(0) match {
          case VersionBranchRegex(versionBranch) => {
            // we are developing on a version branch, create a snapshot
            versionBranch + "-SNAPSHOT"
          }
          case _ => {
            // not on a version branch (e.g. a review branch), try to figure
            // out the tracking branch
            val trackingBranch = exec("git for-each-ref --format=%(upstream:short) refs/heads/" + branch(0))
            assert(trackingBranch.length == 1)
            val TrackingBranchRegex = """^[^/]+/(.+)$""".r
            trackingBranch(0) match {
              case TrackingBranchRegex(trackingVersion) => {
                trackingVersion + "-SNAPSHOT"
              }
              case _ => {
                // no idea what the version is, set it to a fefault
                "0.0.0-SNAPSHOT"
              }
            }
          }
        }
      }
    }
    res
  })

  s ++= Seq(Keys.libraryDependencies <+= Keys.version(v => "edu.illinois.ncsa" %% "daffodil-cli" % v))
}
