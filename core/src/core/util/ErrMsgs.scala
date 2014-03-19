package core.util

/**
 * Created with IntelliJ IDEA.
 * User: gemengqin
 * Date: 3/19/14
 * Time: 12:11 PM
 * To change this template use File | Settings | File Templates.
 */
object ErrMsgs {

  def getErrMsg(errId:Int , line:Int):String ={
    val errInfo = new ErrMsgs().getErrMsg(errId,line)
    errInfo

  }

}

class ErrMsgs{



  def getErrMsg(errId:Int, line: Int) :String ={

    errId match {
      case ErrConstants.VarNotInitialized => " is not initialized"  + " in line: " +  line
    }



  }
}
