package   core.util

import java.util.regex.Pattern

object DotDescriptor {
  def strEscape(str: String): String = {
	val s1 = str.replaceAll("[\\s]+", " ")
	
	//Pattern p = Pattern.compile("\"[^Q]+\"", Pattern.LITERAL | Pattern.MULTILINE | Pattern.DOTALL);
	val p = Pattern.compile("\"[^\"]+\"")
	val m = p.matcher(s1)
	
	val sb = new StringBuffer()
	while (m.find()) {
		var s = m.group();
		s = s.replaceAll("\\\\", "\\\\\\\\\\\\\\\\")
		m.appendReplacement(sb, s)
	}
	m.appendTail(sb)

	val s2 = sb.toString()
	
	s2.replaceAll("\"", "\\\\\"")
  }
}

class DotDescriptor(description: String) {
  val attributes = scala.collection.mutable.Map[String, String]()

  override def toString(): String = {
    val b = new StringBuilder()

    b.append(description)
    if (!attributes.isEmpty) {
      b.append(" [")
      for (key <- attributes.keySet) {
        b.append(key)
          .append("=")
          .append(attributes(key))
          .append(",")
      }
      b.deleteCharAt(b.length() - 1)
      b.append("];")
    } else {
      b.append(";")
    }

    b.toString()
  }
}