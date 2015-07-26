import scala.util.parsing.combinator._

object Regexen {
  val RangeRegex = """\[(\\\]|[^\]])*\]""".r
  val ModifierRegex = """(\*|\+|\?|\{\d+,\d*\}|\{\d+\})\??""".r
  val CharacterRegex = ("""\\""" + """u[\daa-fA-F]{4}|\\.|[^()|]""").r

  val ExactRegex = """\{(\d+)\}.*""".r
  val RepetitionRegex = """\{\d*,(\d+)\}.*""".r
  val UnboundedRegex = """\{\d*,\}.*""".r
}

sealed trait Everything {
  def greatest: Option[Long]

  def containsAnchor(target: Anchor): Boolean = this match {
    case anchor: Anchor => target == anchor
    case Base(e) => e.containsAnchor(target)
    case Atom => false
    case Factor(base, modifier) => modifier.repeatAtLeastOnce && base.containsAnchor(target)
    case Term(factors) => factors.exists(_.containsAnchor(target))
    case Reg(choices) => choices.forall(_.containsAnchor(target))
  }

  def maximumMatchableLength: Option[Long] = if (containsAnchor(Startline) && containsAnchor(Endline)) {
    greatest
  } else {
    None
  }
}
sealed trait Anchor extends Everything {
  def greatest = Some(0)
}
case object Startline extends Anchor
case object Endline extends Anchor
case class Base(e: Everything) extends Everything {
  def greatest = e.greatest
}
case object Atom extends Everything {
  def greatest = Some(1)
}
case class Factor(base: Everything, modifier: Modifier) extends Everything {
  def greatest = for {
    g <- base.greatest
    m <- modifier.maxRepeats
  } yield g * m
}
case class Modifier(i:String) {

  def maxRepeats: Option[Long] = if (i.startsWith("*") || i.startsWith("+")) {
    None
  } else if (i.startsWith("?")) {
    Some(1)
  } else {
    i match {
      case Regexen.RepetitionRegex(x) => Some(x.toLong)
      case Regexen.ExactRegex(x) => Some(x.toLong)
      case Regexen.UnboundedRegex() => None
    }
  }

  def repeatAtLeastOnce = !(i.startsWith("?") || i.startsWith("*") || i.startsWith("{0"))
}
case class Term(factors: List[Everything]) extends Everything {
  def greatest = factors.map(_.greatest).fold(Some(0L)) {
    case (Some(x), Some(y)) => Some(x + y)
    case _ => None
  }
}
case class Reg(choices: List[Everything]) extends Everything {
  def greatest = choices.map(_.greatest).fold(Some(0L)) {
    case (Some(x), Some(y)) => Some(x max y)
    case _ => None
  }
}

class JsonSchemaRegexParser extends RegexParsers {

  def startline: Parser[Everything] = "^" ^^ {x => Startline}
  def endline: Parser[Everything] = "$" ^^ {x => Endline}

  def character = Regexen.CharacterRegex

  def range = Regexen.RangeRegex

  def atom: Parser[Everything] = startline | endline | (range | character) ^^ {x => Atom}

  def base: Parser[Everything] = atom | "(" ~ reg ~ ")" ^^ {case a~b~c => Base(b)}

  def modifier: Parser[Modifier] = Regexen.ModifierRegex ^^ {x => Modifier(x)}

  def factor: Parser[Everything] = base ~ modifier.? ^^ {
    case a~b => {
      Factor(a,b.getOrElse(Modifier("{1}")))
    }
  }

  def term: Parser[Everything] = factor.* ^^ {l => Term(l)}

  def reg: Parser[Everything] = repsep(term, "|") ^^ {x => Reg(x)}

} 

object RegexAnalyzer extends JsonSchemaRegexParser {

  /**
   * Find an upper bound on the length of a string matchable by a JSON Schema regex
   *
   * e.g. getMaxLength("""^[0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12}$|^[0-9a-f]{16}$""")
   * returns Some(36)
   *
   * @param targetRegex
   * @return None if we fail to parse the regex or the regex can match arbitrarily long strings
   *         Some(upper bound) otherwise.
   */
  def getMaxLength(targetRegex: String): Option[Long] = parseAll(reg, targetRegex) match {
    case Success(matched, _) => matched.maximumMatchableLength
    case _ => None
  }
}
