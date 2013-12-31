package   refactoring.matlab.processing

import java.io._
import scala.sys.process._

object CompileManager {
  val baseDir = "/tmp"
  val libDir = "/home/twt/Dropbox/myworkspace/dsl/  .refactoring.matlab/stencil"
  val libFiles = List("global.h", "array.h", "hemi.h", "ndarray.h", "inplane-full.h", "unroll.h")
  
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
  
  def printToFile(file: String, text: String) {
    val f = new File(file)
    val p = new java.io.PrintWriter(f)
    try { p.print(text) } finally { p.close() }
  }
  
  def copyFile(src: String, dst: String) {
    import java.io.{File,FileInputStream,FileOutputStream}
    new FileOutputStream(dst) getChannel() transferFrom(
      new FileInputStream(src) getChannel, 0, Long.MaxValue)    
  }
  
  def compile(code: GeneratedCode) {
    // create a temp directory
    val tempDir = s"$baseDir/stencilcode/"
    val dir = new File(tempDir)
    if (dir.exists) {
      dir.delete
    }
    dir.mkdir
    
    printToFile(s"$tempDir/main.cu", code.main)
    code.stencils.foreach{ case (f, c) => printToFile(s"$tempDir/$f", c) }
    
    // copy other lib files
    libFiles.foreach{f => copyFile(s"$libDir/$f", s"$tempDir/$f")}
    
    // compile
    val cmd = Seq("/usr/local/cuda/bin/nvcc", "-o", "stencil", "main.cu")
    val wd = dir
    val proc = Process(cmd, wd, ("PATH", "/usr/local/cuda/bin:/usr/bin:/bin"))
    val out = proc.!! 
    println(s"output: $out")
  }
}