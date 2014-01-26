///**
// * Created with IntelliJ IDEA.
// * User: gemengqin
// * Date: 12/27/13
// * Time: 9:53 AM
// * To change this template use File | Settings | File Templates.
// */
//
//import scala.sys.process._
//import java.io._
//import org.xml.sax.SAXParseException
//import model.statement._
//import refactoring.matlab.processing._
//
//object test {
//  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B = try { f(param) } finally { param.close() }
//
//  val path = "/Users/gemengqin/Dropbox/ntu/FYP/matlab_to/src/m_files"
//
//  def main(args: Array[String]) {
//    val filename = path + "/test.m"
//
//    var ast = parseFile(filename)
//
//    // preprocessing
//    ast = Preprocessor.preprocess(ast)
//
//    val gen = CodeGenerator.generate(ast)
//
//    CompileManager.compile(gen)
//    println(gen.main)
//    println(gen.stencils)
//    //println(ast.treePretty(0))
//  }
//
//  def parseFile(filename: String): StatementBlock = {
//    parseFile(new File(filename), false)
//  }
//
//  def parseFile(file: File, display: Boolean = false): StatementBlock = {
//    val filename = file.getAbsolutePath()
//    if (filename.endsWith(".m")) {
//      //println(filename)
//
//      try {
//        val output = convertXML(filename)
//        try {
//          val script = scala.xml.XML.loadString(output)
//          //println(output)
//          val dsl = MatlabILParser.parse(script)
//
//          if(display) println(dsl.treePretty())
//          return dsl
//
//        } catch {
//          case e: SAXParseException => println("SAXParseException: XML Error"); throw e
//          case e: UnsupportedOperationException => println("UnsupportedOperationException: " + e.getMessage); throw e
//          case e => {
//            writeToFile(path + "/xml/" + file.getName() + ".xml", output)
//            throw e
//          }
//        }
//      } catch {
//        case e: IOException => {
//          println("IOException: " + e.getMessage())
//          throw e
//        }
//      }
//    } else {
//      println("Not matlab file: " + filename)
//      throw new UnsupportedOperationException("Not matlab file")
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
//      println(out.mkString("\n"))
//      out.mkString("\n").substring(6)
//    } else {
//      throw new IOException("Cannot convert octave script: " + filename + " -> " + err.mkString("\n"))
//    }
//  }
//}