package mpstk

/** Processes. */
sealed abstract class DOT {

//   lazy val roles: Set[Role] = ops.roles(this)

//   def extraction(r: Role): Either[String, MPST] = ops.extraction(this, r)

//   lazy val extractions: Map[Role, Either[String, MPST]] = {
//     Map(roles.map(r => (r, extraction(r))).toSeq:_*)
//   }
}

object DOT {
  /** State. */
  case class State(name: String) extends DOT {
    override def toString = s"${name}"
  }

  /** Branching. */
  case class Branch(s: Session, to: Role, from: Role,
                    label: Label, payload: String) extends DOT {
    override def toString = s"i(${s}, ${from}, ${to}, ${label}, ${payload})"
  }

  /** Selection. */
  case class Select(s: Session, from: Role, to: Role,
                    label: Label, payload: String) extends DOT {
    override def toString = s"o(${s}, ${from}, ${to}, ${label}, ${payload})"
  }

  /** Communication. */
  case class Communication(s: Session, from: Role, to: Role,
                    label: Label, payload: String) extends DOT {
    override def toString = s"t(${s}, ${from}, ${to}, ${label}, ${payload})"
  }
  // May or may not be used (if assuming label unicity, we need only search for the last actions)

  /** Anything else. */
  case class TheRest(s: String) extends DOT {
      override def toString = s"${s}"
  }
}