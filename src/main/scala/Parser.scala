// mpstk - the Multiparty Session Types toolKit
// Copyright 2018 Alceste Scalas <alceste.scalas @ imperial.ac.uk>
// Released under the MIT License: https://opensource.org/licenses/MIT
package mpstk.parser

import scala.util.parsing.combinator._
import scala.language.postfixOps

import mpstk.{Channel, Label, Role, Session, Type, GroundType}
import mpstk.{Process, GlobalType, MPST, BasePayloadCont, PayloadCont,
              Branch, Select, Rec, RecVar, End}
import mpstk.Context
import mpstk.raw

protected[parser] case class ParserConfig[A, PC <: BasePayloadCont[A]](
  PayloadCont: (Type, A) => PC,
  endPayload: Type,
  endCont: A
)

/** Base parser trait with common syntactic elements. */
protected[parser]
abstract trait BaseParser extends RegexParsers {
  def comment: Parser[String] = """(?m)#.*$""".r
  def comments: Parser[Unit] = rep(comment) ^^ { _ => () }

  def identifier: Parser[String] = """[a-zA-Z]\w*""".r

  def label: Parser[Label] = identifier ^^ { l => Label(l) }
  def role:  Parser[Role]  = identifier ^^ { r => Role(r) }

  def ground: Parser[GroundType] = bool | int | string | unit

  def bool: Parser[GroundType.Bool.type] = "[Bb]ool".r ^^ {
    _ => GroundType.Bool
  }
  def int: Parser[GroundType.Int.type] = "[Ii]nt".r ^^ {
    _ => GroundType.Int
  }
  def string: Parser[GroundType.String.type] = {
    ("[Ss]tring".r | "[Ss]tr".r) ^^ { _ => GroundType.String }
  }
  def unit: Parser[GroundType.Unit.type] = "[Uu]nit".r ^^ {
    _ => GroundType.Unit
  }

  // Choice adaptation: restrain the type for "value"

  def choice[A, PC <: BasePayloadCont[A]](value: Parser[String],
                             tpe: Parser[Type],
                             cont: Parser[A],
                             cfg: ParserConfig[A, PC]): Parser[(Label, PC)] = {
    label ~ payloadcont(value, tpe, cont, cfg) ^^ { lpc =>
      (lpc._1, lpc._2)
    }
  }

  def payloadcont[A, PC <: BasePayloadCont[A]](value: Parser[String],
                                      tpe: Parser[Type],
                                      cont: Parser[A],
                                      cfg: ParserConfig[A, PC]): Parser[PC] = {
    payloadcontProcess(value, tpe, cont, cfg) |
    payloadcontFull(tpe, cont, cfg) |
    payloadcontNoPay(cont, cfg) |
    payloadcontNoCont(tpe, cfg) |
    payloadcontEmpty(cfg)
  }

  def payloadcontProcess[A, PC <: BasePayloadCont[A]](value: Parser[String],
                                      tpe: Parser[Type],
                                      cont: Parser[A],
                                      cfg: ParserConfig[A, PC]): Parser[PC] = {
    ("(" ~> (value ~ "@" ~ tpe) <~ ")") ~ ("." ~> cont) ^^ {
      pc => cfg.PayloadCont(pc._1._2, pc._2)
    }
  }

  def payloadcontFull[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                                      cont: Parser[A],
                                      cfg: ParserConfig[A, PC]): Parser[PC] = {
    ("(" ~> tpe <~ ")") ~ ("." ~> cont) ^^ {
      pc => cfg.PayloadCont(pc._1, pc._2)
    }
  }

  def payloadcontNoPay[A, PC <: BasePayloadCont[A]](cont: Parser[A],
                                      cfg: ParserConfig[A, PC]): Parser[PC] = {
    (("(" ~ ")")?) ~> ("." ~> cont) ^^ { cnt =>
      cfg.PayloadCont(cfg.endPayload, cnt)
    }
  }

  def payloadcontNoCont[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                                      cfg: ParserConfig[A, PC]): Parser[PC] = {
    ("(" ~> tpe <~ ")") ^^ { pay =>
      cfg.PayloadCont(pay, cfg.endCont)
    }
  }

  def payloadcontEmpty[A, PC <: BasePayloadCont[A]](cfg: ParserConfig[A, PC]): Parser[PC] = {
    "" ^^ { _ => cfg.PayloadCont(cfg.endPayload, cfg.endCont) }
  }

  def choices[A, PC <: BasePayloadCont[A]](value: Parser[String],
                       tpe: Parser[Type],
                       cont: Parser[A],
                       cfg: ParserConfig[A, PC]): Parser[List[(Label, PC)]] = {
    choicesMulti(value, tpe, cont, cfg) | choicesSingle(value, tpe, cont, cfg)
  }

  def choicesMulti[A, PC <: BasePayloadCont[A]](value: Parser[String],
                       tpe: Parser[Type],
                       cont: Parser[A],
                       cfg: ParserConfig[A, PC]): Parser[List[(Label, PC)]] = {
    "{" ~> rep1sep(choice(value, tpe, cont, cfg), ",") <~ "}" ^? ({
      case l: List[(Label, PC)] if (
        // Ensure that labels are unique
        l.map(_._1).distinct.size == l.size
      ) => l
    }, { l =>
      val labels = l.map { _._1 }
      val dupl = labels.filter { l =>
        labels.indexOf(l) != labels.lastIndexOf(l)
      }.distinct
      f"""Choice with duplicated label(s): ${dupl.mkString(", ")}"""
    })
  }

  def choicesSingle[A, PC <: BasePayloadCont[A]](value: Parser[String],
                       tpe: Parser[Type],
                       cont: Parser[A],
                       cfg: ParserConfig[A, PC]): Parser[List[(Label, PC)]] = {
    choice(value, tpe, cont, cfg) ^^ { c => List(c) }
  }
}

// Process parser here:
// syntax:
// Process P:
// P = c[q]<|m(d).P | P = c[q](S){m1(d1).P1,...,mn(dn).Pn} | P = 0
// | P = (P|Q) | P = (nu s)P | P = X⟨c1,...,cn⟩ | P = def D in P
// Def D:
// "X(x1,...,xn) = P"
// Endpoint c:
// x | s[p]

protected[parser]
class ProcParser extends BaseParser {
  private val cfg = ParserConfig[Process, Process.PayloadCont](
    (payload: Type, cont: Process) => Process.PayloadCont(payload, cont),
    End,
    Process.End
  )

  def session: Parser[Session] = identifier ^^ { s => Session(s) }

  def value: Parser[String] = identifier ^^ { x => x }

  def channel: Parser[Channel] = session ~ ("[" ~> role <~ "]") ^^ {
    sr => Channel(sr._1, sr._2)
  }

  def tpe: Parser[Type] = ground //| mpst

  def proc: Parser[Process] = {
    ("(" ~> definition <~ ")") | definition | call | ("(" ~> parallel <~ ")") | branch | select | end | rec | recvar // | res
  }

  def end: Parser[Process.End.type] = "0".r ^^ { _ => Process.End }

  def parallelSym: Parser[String] = "|"
  def parallel: Parser[Process.Parallel] = {
    proc ~ (parallelSym ~> proc) ^^ { rp =>
      Process.Parallel(rp._1, rp._2)
    }
  }

  def defName: Parser[Process.DefName] = identifier ^^ { name => Process.DefName(name) }

  // def choices[A, PC <: BasePayloadCont[A]](value: Parser[String],
  //                        tpe: Parser[Type],
  //                        cont: Parser[A],
  //                        cfg: ParserConfig[A, PC]): Parser[List[(Label, PC)]] = {
  //     choicesMulti(value, tpe, cont, cfg) | choicesSingle(value, tpe, cont, cfg)
  //   }

  // def defVars[A](value: Parser[String],
  //               tpe: Parser[Type]) : Parser[List[(A)]] = {

  // }

  // def definition: Parser[Process.Definition] = {
  //   ("def" ~> defName ~ ("(" ~> defVars(value, tpe) <~ ")") ~ ("=" proc) <~ "in") ~ proc ^^ { rv =>
  //     Process.Definition(rv._1._1._1, Map(rv._1._1._2:_*), rv._1._2, rv._2)
  //   }
  // }

  // Simpler Definition with no arguments
  def definition: Parser[Process.Definition] = {
    ("def" ~> defName ~ ("=" ~> proc <~ "in")) ~ proc ^^ { rv =>
      Process.Definition(rv._1._1, rv._1._2, rv._2)
    }
  }

  def callSymL: Parser[String] = "⟨" | "<"
  def callSymR: Parser[String] = "⟩" | ">"
  def call: Parser[Process.Call] = {
    defName ~ (callSymL ~ callSymR) ^^ { cv =>
      Process.Call(cv._1)
    }
  }

  def branchSym: Parser[String] = "(S)" | "Σ"
  def branch: Parser[Process.Branch] = {
    session ~ ("[" ~> role <~ "]") ~ ("[" ~> role <~ "]") ~ (branchSym ~> choices(value, tpe, proc, cfg)) ^^ { rc =>
      Process.Branch(rc._1._1._1, rc._1._1._2, rc._1._2, Map(rc._2:_*))
    }
  }

  def selectSym: Parser[String] = "<|" | "◁"
  def select: Parser[Process.Select] = {
    session ~ ("[" ~> role <~ "]") ~ ("[" ~> role <~ "]") ~ (selectSym ~> choices(value, tpe, proc, cfg)) ^^ { rc =>
      Process.Select(rc._1._1._1, rc._1._1._2, rc._1._2, Map(rc._2:_*))
    }
  }

  def recSym: Parser[String] = "μ" | "rec"
  def rec: Parser[Process.Rec] = (((recSym ~ "(") ~> recvar <~ ")") ~ proc ~ (callSymL ~> channel <~ callSymR)) ^^ { rm =>
    Process.Rec(rm._1._1, rm._1._2, rm._2)
  } ^? ({
    case p: Process.Rec if p.guarded => p
  }, { p =>
    s"Unguarded recursion on ${p.recvar}"
  })

  def recvar: Parser[Process.RecVar] = identifier ^^ { name => Process.RecVar(name) }


  // Need to adapt choices to process format
  // phase 1: payload = name or value + type as annotation
  // phase 2: payload .= channel + type as annotation

  // When channel passing is allowed: change the actor to allow for "value"
}

// object ProcParser extends ProcParser {
//   /** Parse a process from a string. */
//   def parse(input: String): ParseResult[Process] = {
//     parseAll(comments ~> proc, input)
//   }
// }

/** Parser for global types. */
object ProcParser extends ProcParser {
  import java.nio.file.{Files, Path, Paths}
  import scala.collection.JavaConverters._

  /** Parse a global type from a string. */
  def parse(input: String): ParseResult[Process] = {
    parseAll(comments ~> proc, input)
  }

  /** Parse a global type from a file, given as {@code Path}.
    *
    * @throws java.io.IOException in case of I/O error
    */
  def parse(input: Path): ParseResult[Process] = {
    parse(Files.readAllLines(input).asScala.mkString("\n"))
  }

  /** Parse a global type from a file, given as {@code String}.
    *
    * @throws java.io.IOException in case of I/O error
    * @throws java.nio.file.InvalidPathException if {@code filename} is invalid
    */
  def parseFile(filename: String): ParseResult[Process] = {
    parse(Paths.get(filename))
  }
}

protected[parser]
class MPSTParser extends BaseParser {
  private val cfg = ParserConfig[MPST, PayloadCont](
    (payload: Type, cont: MPST) => PayloadCont(payload, cont),
    End,
    End
  )

  def tpe: Parser[Type] = ground | mpst

  def value: Parser[String] = identifier ^^ { x => x }

  // NOTE: always try to match recvar last (otw, it might capture e.g. "end")
  def mpst: Parser[MPST] = {
    ("(" ~> mpst <~ ")") | branch | select | end | rec | recvar
  }

  def closedmpst: Parser[MPST] = mpst ^? ({
    case t: MPST if t.closed => t
  }, { t =>
    f"The session must be closed (free variable(s): ${t.fv.mkString(", ")})"
  })

  def end: Parser[End.type] = "end".r ^^ { _ => End }

  def branchSym: Parser[String] = "&"
  def branch: Parser[Branch] = {
    role ~ (branchSym ~> choices(value, tpe, mpst, cfg)) ^^ { rc =>
      Branch(rc._1, Map(rc._2:_*))
    }
  }

  def selectSym: Parser[String] = "⊕" | "(+)"
  def select: Parser[Select] = {
    role ~ (selectSym ~> choices(value, tpe, mpst, cfg)) ^^ { rc =>
      Select(rc._1, Map(rc._2:_*))
    }
  }

  def recSym: Parser[String] = "μ" | "rec"
  def rec: Parser[Rec] = (((recSym ~ "(") ~> recvar <~ ")") ~ mpst) ^^ { rm =>
    Rec(rm._1, rm._2)
  } ^? ({
    case t: Rec if t.guarded => t
  }, { t =>
    s"Unguarded recursion on ${t.recvar}"
  })

  def recvar: Parser[RecVar] = identifier ^^ { name => RecVar(name) }
}

/** Session type parser. */
object MPSTParser extends MPSTParser {
  /** Parse a session type from a string. */
  def parse(input: String): ParseResult[MPST] = {
    parseAll(comments ~> mpst, input)
  }
}

protected[parser]
class ContextParser extends MPSTParser {
  def session: Parser[Session] = identifier ^^ { s => Session(s) }

  def channel: Parser[Channel] = session ~ ("[" ~> role <~ "]") ^^ {
    sr => Channel(sr._1, sr._2)
  }

  def entry: Parser[(Channel, MPST)] = channel ~ (":" ~> closedmpst) ^^ { cs =>
    (cs._1, cs._2)
  }

  def entries: Parser[List[(Channel, MPST)]] = {
    repsep(entry, ",") ^? ({
      case l: List[(Channel, MPST)] if (
        // Ensure that channels are unique
        l.map(_._1).distinct.size == l.size
      ) => l
    }, { l =>
      val channels = l.map { _._1 }
      val dupl = channels.filter { c =>
        channels.indexOf(c) != channels.lastIndexOf(c)
      }.distinct
      f"""Context with duplicated channel(s): ${dupl.mkString(", ")}"""
    })
  }

  def context: Parser[Context] = entries ^^ { entries => Context(entries:_*) }
}

/** Parser for session typing contexts. */
object ContextParser extends ContextParser {
  import java.nio.file.{Files, Path, Paths}
  import scala.collection.JavaConverters._

  /** Parse a session typing context from a string. */
  def parse(input: String): ParseResult[Context] = {
    parseAll(comments ~> context, input)
  }

  /** Parse a session typing context from a file, given as {@code Path}.
    *
    * @throws java.io.IOException in case of I/O error
    */
  def parse(input: Path): ParseResult[Context] = {
    parse(Files.readAllLines(input).asScala.mkString("\n"))
  }

  /** Parse a session typing context from a file, given as {@code String}.
    *
    * @throws java.io.IOException in case of I/O error
    * @throws java.nio.file.InvalidPathException if {@code filename} is invalid
    */
  def parseFile(filename: String): ParseResult[Context] = {
    parse(Paths.get(filename))
  }
}

protected[parser]
class GlobalTypeParser extends MPSTParser {
  private val cfg = ParserConfig[GlobalType, GlobalType.PayloadCont](
    (payload: Type, cont: GlobalType) => GlobalType.PayloadCont(payload, cont),
    End,
    GlobalType.End
  )

  private def payloadCont(payload: Type, cont: GlobalType) = {
    GlobalType.PayloadCont(payload, cont)
  }
  private val endPayload = End
  private val endCont = GlobalType.End

  // NOTE: always try to match gtrecvar last (otw, it might capture e.g. "end")
  def globaltype: Parser[GlobalType] = {
    ("(" ~> globaltype <~ ")") | comm | gtend | gtrec | gtrecvar
  }

  // We only accept closed types
  override def tpe: Parser[Type] = ground | closedmpst

  def gtend: Parser[GlobalType.End.type] = "end".r ^^ { _ => GlobalType.End }

  def commSym: Parser[String] = "→" | "->"
  def comm: Parser[GlobalType.Comm] = {
    role ~ (commSym ~> role <~ (":"?)) ~ choices(value, tpe, globaltype, cfg) ^^ { rc=>
      GlobalType.Comm(rc._1._1, rc._1._2, Map(rc._2:_*))
    }
  }

  // TODO: refactor the following, to avoid code duplication?
  def gtrecSym: Parser[String] = "μ" | "rec"
  def gtrec: Parser[GlobalType.Rec] = {
    (((gtrecSym ~ "(") ~> gtrecvar <~ ")") ~ globaltype) ^^ { rm =>
      GlobalType.Rec(rm._1, rm._2)
    }
  } ^? ({
    case t: GlobalType.Rec if t.guarded => t
  }, { t =>
    s"Unguarded recursion on ${t.recvar}"
  })

  def gtrecvar: Parser[GlobalType.RecVar] = identifier ^^ {
    name => GlobalType.RecVar(name)
  }
}

/** Parser for global types. */
object GlobalTypeParser extends GlobalTypeParser {
  import java.nio.file.{Files, Path, Paths}
  import scala.collection.JavaConverters._

  /** Parse a global type from a string. */
  def parse(input: String): ParseResult[GlobalType] = {
    parseAll(comments ~> globaltype, input)
  }

  /** Parse a global type from a file, given as {@code Path}.
    *
    * @throws java.io.IOException in case of I/O error
    */
  def parse(input: Path): ParseResult[GlobalType] = {
    parse(Files.readAllLines(input).asScala.mkString("\n"))
  }

  /** Parse a global type from a file, given as {@code String}.
    *
    * @throws java.io.IOException in case of I/O error
    * @throws java.nio.file.InvalidPathException if {@code filename} is invalid
    */
  def parseFile(filename: String): ParseResult[GlobalType] = {
    parse(Paths.get(filename))
  }
}
