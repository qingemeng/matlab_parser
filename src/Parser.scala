//import scala.sys.process._
//import java.io._
//import org.xml.sax.SAXParseException
///**
// * Created with IntelliJ IDEA.
// * User: gemengqin
// * Date: 12/26/13
// * Time: 4:41 PM
// * To change this template use File | Settings | File Templates.
// */
//object Parser {
//  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B = try { f(param) } finally { param.close() }
//
//  val path = "/Users/gemengqin/Dropbox/ntu/FYP/matlab_to/src/m_files"
//
//  def main(args: Array[String]) {
//    val filename = path + "/test.m"
//
//    parseFile(filename)
//
//
//    //    val root = path + "/mat2c"
//    //    for (dir <- new File(root).listFiles.sorted){
//    //      if(dir.isDirectory()){
//    //        dir.listFiles.sorted.foreach(parseFile(_))
//    //      } else {
//    //        parseFile(dir)
//    //      }
//    //    }
//  }
//
//  def parseFile(filename: String){
//    parseFile(new File(filename), true)
//  }
//
//  def parseFile(file: File, display: Boolean = false) {
//    val filename = file.getAbsolutePath()
//    if (filename.endsWith(".m")) {
//      println(filename)
//
//      try {
//        val output = convertXML(filename)
//        try {
//          val script = scala.xml.XML.loadString(output)
//          val dsl = MatlabILParser.parse(script)
//
//          if(display) println(dsl.pretty())
//
//        } catch {
//          case e: SAXParseException => println("SAXParseException: XML Error")
//          case e: UnsupportedOperationException => println("UnsupportedOperationException: " + e.getMessage)
//          case e: Throwable => {
//            writeToFile(path + "/xml/" + file.getName() + ".xml", output)
//            throw e
//          }
//        }
//      } catch {
//        case e: IOException => {
//          println("IOException: " + e.getMessage())
//        }
//      }
//    } else {
//      println("Not matlab file: " + filename)
//    }
//  }
//
//  def writeToFile(fileName:String, data:String) =
//    using (new FileWriter(fileName)) {
//      fileWriter => fileWriter.write(data)
//    }
//
//  def convertXML(filename: String) = {
//    val process = "/opt/local/bin/octave --path " + path +" --quiet --eval octave_to_xml(\"" + filename + "\")"
//
//    val qb = Process(process)
//    var out = scala.collection.mutable.ListBuffer[String]()
//    var err = scala.collection.mutable.ListBuffer[String]()
//
//    val exit = qb ! ProcessLogger((s) => out += s, (s) => err += s)
//
//    if(exit == 0){
//      out.mkString("\n").substring(6)
//    } else {
//      throw new IOException("Cannot convert matlab script: " + filename + " -> " + err.mkString("\n"))
//    }
//  }
//}