package mpstk

/** Processes. */
sealed abstract class Process {
  /** Return a version of this process respecting the Barendregt convention */
  def barendregt: Process = ops.barendregt(this)
  //TODO: make a process version in Ops.scala

  /** Is the process closed? */
  //def closed: Boolean = ops.closed(this)

  /** Free variables. */
  //def fv: Set[Process.RecVar] = ops.fv(this)

  /** Does the process have guarded recursion variables, only? */
  def guarded: Boolean = ops.guarded(this)
  //guardedness for processes is different (and we may not care about it)

  /** Merge (if possible) with the given process. */
  //def merge(that: MPST): Either[String, MPST] = ops.merge(this, that)
  //merge operator only for types? Maybe we'll use a similar thing and call it merge
  //for continuations in the case of choice constructs, se keep for now.

  /** Return the potential outputs towards role @to, as pairs of label
    * and payload.
    */
  //def outputs(to: Role): Set[(Label, Type)] = ops.outputs(this, to)
  //TODO: make a process version in Ops.scala

  /** Unfold payload process, ensuring they do not have variables bound by the
    * carrier process.
    */
  //def unfoldPayloads: Process = ops.unfoldPayloads(this)
  //processes do not send the contents, just an endpoint or ground type

  /** Convert to raw process. */
  //protected[mpstk] def toRaw: raw.Process = raw.ops.procToRaw(this)
  //we probably don't care

  lazy val roles: Set[Role] = ops.roles(this)

  def extraction(r: Role): Either[String, MPST] = ops.extraction(this, r)

  lazy val extractions: Map[Role, Either[String, MPST]] = {
    Map(roles.map(r => (r, extraction(r))).toSeq:_*)
  }
}

object Process {
  /** Terminated process */
  case object End extends Process {
    override def toString = "0"
  }

  /** Process payload and continuation */
  case class PayloadCont(payload: Type,
                        cont: Process) extends BasePayloadCont[Process](payload, cont)

  /** A generic choice, abstracting branching and selection */
  sealed abstract class Choice(val choices: Choices[PayloadCont]) extends Process {
    override def toString = {
      val cs = choices.map { lpc => s"${lpc._1}${lpc._2}" }
      cs.mkString(", ")
    }
  }

  /** Branching. */
  case class Branch(s: Session, to: Role, from: Role,
                    override val choices: Choices[PayloadCont]) extends Choice(choices) {
    override def toString = s"${s}${to}${from}(S){${super.toString}}"
  }

  /** Selection. */
  case class Select(s: Session, from: Role, to: Role,
                    override val choices: Choices[PayloadCont]) extends Choice(choices) {
    override def toString = s"${s}${from}${to}<|{${super.toString}}"
  }

  /** Parallel. */
  case class Parallel(p1: Process, p2: Process) extends Process {
    override def toString = s"${p1} | ${p2}"
  }

  /** Recursive process (shortcut). */
  case class Rec(recvar: RecVar, body: Process, chan: Channel) extends Process {
    override def toString = s"μ(${recvar})${body}⟨${chan}⟩"
  }

  /** Recursion variable. */
  case class RecVar(name: String) extends Process {
    override def toString = s"${name}"
  }

  /** Definition variable. */
  case class DefName(name: String) extends Process {
    override def toString = s"${name}"
  }

  case class Definition(defvar: DefName, p1: Process, p2: Process) extends Process {
    override def toString= s"def ${defvar} = ${p1} in ${p2}" //TODO ${defvar}(${vars})
  }

  case class Call(callvar: DefName) extends Process {
    override def toString= s"${callvar}⟨⟩" //TODO <${vars}>
  }
}