// TODO
// Base: projector
// Use a map to store temporary type construction data
// expand process specification with annotation in branch & select payload
// design a form of reporting/checking on the labels

package mpstk.tool

import mpstk._

import scala.collection.immutable.ListMap

import java.nio.file.{Path, Paths}

import picocli.{CommandLine => cli}

/** Implementation of the command line projection tool. */
object Extractor extends Common {
  override val TOOL_NAME = "mpstk extract"

  /** Tool configuration, set up via command line options */
  @cli.Command(name = "mpstk extract",
               version = Array("0.1"), // FIXME: synchronise with sbt
               description = Array("Extract local session types"))
  private class Config extends CommonConfig {
    @cli.Spec
    var spec: cli.Model.CommandSpec = null // Injected by picocli

    var roles: Array[Role] = Array()
    @cli.Option(names = Array("-r", "--roles"),
                paramLabel = "ROLES",
                description = Array("comma-separated list of roles to project (default: all roles in the global type)"))
    def setRoles(rolesStr: String): Unit = {
      roles = rolesStr.split(",").map { r => Role(r) }
    }

    var session: Session = Session("s")
    @cli.Option(names = Array("-s", "--session"),
                paramLabel = "SESSION",
                description = Array("name of the session for the projected typing context (default: s)"))
    def setSession(s: String): Unit = {
      session = Session(s)
    }

    var path: Option[Path] = None
    @cli.Parameters(arity = "0..1", paramLabel = "FILE",
                    description = Array("file to extract (extension: .proc)"))
    def setPath(p: Path): Unit = {
      if (!p.toString.endsWith(FILE_EXT_PROCESS)) {
        printError(s"${p}: unsupported file extension")
      }
      checkReadableFile(p)      
      path = Some(p)
    }
  }

  /** Main entry point. */
  def main(args: Array[String]): Unit = {
    val cfg = new Config
    val cmdline = new picocli.CommandLine(cfg)
    try {
      cmdline.parse(args:_*)

      setLogLevel(cfg)

      if (cfg.printHelp) {
        if (!cfg.path.isEmpty) printError(
          s"""Cannot process files with "${cfg.OPTION_HELP}"""")
        printHelp(cmdline)
      }

      if (cfg.path.isEmpty) {
        printError("Missing required parameter: FILE")
      }

      val path = cfg.path.get

      parser.ProcParser.parse(path) match {
        case parser.ProcParser.NoSuccess(msg, input) => {
          printParseError(path.toString, input.pos, msg)
          throw new RuntimeException("Unreachable")
        }
        case parser.ProcParser.Success(prc, _) => {
          if (cfg.roles.size == 0) {
            cfg.roles = prc.roles.toArray //todo change
          }
          val extr = Map(cfg.roles.map(r => (r, prc.extraction(r))):_*) //todo change
          
          if (extr.values.exists(_.isLeft)) {
            val err = "Undefined extractions:\n" ++ (
              extr.foldLeft(List[String]()) {
                case (acc, (r, Left(err))) => acc :+ s" * ${r}: ${err}"
                case (acc, (_, Right(_))) => acc
              }.mkString("\n"))
            printError(err, false)
          } else {
            val res = cfg.roles.foldLeft(List[String]()) { (acc, r) =>
              acc ++ List(s"${Channel(cfg.session, r)}: ${extr(r).right.get}")
            }.mkString(",\n")
            println(res)
          }
        }
      }
    } catch {
      case e: IllegalArgumentException => {
        printError(e.getMessage)
      }
      case e: cli.ParameterException => {
        printError(e.getMessage)
      }
    }
  }
}
