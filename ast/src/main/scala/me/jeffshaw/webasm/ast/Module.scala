package me.jeffshaw.webasm.ast

import me.jeffshaw.unsigned.UInt
import scala.reflect.{ClassTag, classTag}
import scodec.bits.{BitVector, ByteVector}
import scodec._

case class Module(
  types: Vector[FuncType],
  globals: Vector[Global],
  tables: Vector[Table],
  memories: Vector[Memory],
  funcs: Vector[Func],
  start: Option[Var],
  elems: Vector[Segment[Vector[Var]]],
  data: Vector[Segment[ByteVector]],
  imports: Vector[Import],
  exports: Vector[Export]
)

object Module {
  def unfold[A](
    unfoldTypes: A => (Vector[FuncType], A),
    unfoldGlobals: A => (Vector[Global], A),
    unfoldTables: A => (Vector[Table], A),
    unfoldMemories: A => (Vector[Memory], A),
    unfoldFuncs: A => (Vector[Func], A),
    unfoldStart: A => (Option[Var], A),
    unfoldElems: A => (Vector[Segment[Vector[Var]]], A),
    unfoldData: A => (Vector[Segment[ByteVector]], A),
    unfoldImports: A => (Vector[Import], A),
    unfoldExports: A => (Vector[Export], A)
  )(init: A
  ): (Module, A) = {
    val (types, typesRemainder) = unfoldTypes(init)
    val (globals, globalsRemainder) = unfoldGlobals(typesRemainder)
    val (tables, tablesRemainder) = unfoldTables(globalsRemainder)
    val (memories, memoriesRemainder) = unfoldMemories(tablesRemainder)
    val (funcs, funcsRemainder) = unfoldFuncs(memoriesRemainder)
    val (start, startRemainder) = unfoldStart(funcsRemainder)
    val (elems, elemsRemainder) = unfoldElems(startRemainder)
    val (data, dataRemainder) = unfoldData(elemsRemainder)
    val (imports, importsRemainder) = unfoldImports(dataRemainder)
    val (exports, exportsRemainder) = unfoldExports(importsRemainder)

    val module =
      Module(
        types = types,
        globals = globals,
        tables = tables,
        memories = memories,
        funcs = funcs,
        start = start,
        elems = elems,
        data = data,
        imports = imports,
        exports = exports
      )

    (module, exportsRemainder)
  }

  sealed trait Section

  abstract class SectionCompanion[S <: Section, Inner](implicit sectionTag: ClassTag[S]) {
    def apply(value: Inner): S
    def unapply(section: S): Option[Inner]

    val Id: Byte

    lazy val sectionHeader: BitVector = wcodecs.u8.encode(Id).require
    lazy val sectionHeaderCodec = codecs.constant(sectionHeader)
    val default: S
    def needed(value: Inner): Boolean

    def innerCodec: Codec[Inner]

    implicit lazy val codec: Codec[S] =
      Codec[S](
        encoder = Encoder { section: S =>
          unapply(section) match {
            case Some(inner) =>
              if (needed(inner))
                for {
                  inner <- innerCodec.encode(inner)
                  length <- wcodecs.len32.encode(inner.intSize.get)
                } yield sectionHeader ++ length ++ inner
              else Attempt.successful(BitVector.empty)
            case None =>
              val className =
                classTag[S].runtimeClass.getCanonicalName
              Attempt.failure(Err.General(s"Expected $className", List(className)))
          }
        },
        decoder = Decoder { value: BitVector =>
          val maybeSection =
            for {
              _ <- sectionHeaderCodec
              length <- wcodecs.len32
              bytes <- codecs.bits(length * 8)
              value <- Decoder[Inner](Function.const[Attempt[DecodeResult[Inner]], BitVector](innerCodec.decode(bytes))(_))
            } yield apply(value)

          maybeSection.decode(value).recover({
            case Err.General(message, _) if message.startsWith(s"expected constant $sectionHeader") =>
              DecodeResult(default, value)
          })
        }
      )
  }

  case class FuncTypes(
    funcTypes: Vector[FuncType]
  ) extends Section

  object FuncTypes extends SectionCompanion[FuncTypes, Vector[FuncType]] {
    override val Id: Byte = 3
    override val default: FuncTypes = FuncTypes(Vector.empty)

    override def needed(value: Vector[FuncType]): Boolean = value.nonEmpty

    override val innerCodec: Codec[Vector[FuncType]] =
      wcodecs.vec(Codec[FuncType])
  }

  case class Globals(
    globals: Vector[Global]
  ) extends Section

  object Globals extends SectionCompanion[Globals, Vector[Global]] {
    override val Id: Byte = 6
    override val default: Globals = Globals(Vector.empty)

    override def needed(value: Vector[Global]): Boolean = ???

    override val innerCodec: Codec[Vector[Global]] = wcodecs.vec(Codec[Global])
  }

  case class Tables(
    tables: Vector[Table]
  ) extends Section

  object Tables extends SectionCompanion[Tables, Vector[Table]] {
    override val Id: Byte = 4
    override val default: Tables = Tables(Vector.empty)

    override def needed(value: Vector[Table]): Boolean = value.nonEmpty

    override val innerCodec: Codec[Vector[Table]] =
      wcodecs.vec(Codec[Table])
  }

  case class Memories(
    memories: Vector[Memory]
  ) extends Section

  object Memories extends SectionCompanion[Memories, Vector[Memory]] {
    override val Id: Byte = 5
    override val default: Memories = Memories(Vector.empty)

    override def needed(value: Vector[Memory]): Boolean = value.nonEmpty

    override val innerCodec: Codec[Vector[Memory]] =
      wcodecs.vec(Codec[Memory])
  }

  case class Funcs(
    funcs: Vector[Var]
  ) extends Section

  object Funcs extends SectionCompanion[Funcs, Vector[Var]] {
    def ofFuncs(f: Vector[Func]): Funcs =
      Funcs(f.map(_.funcType))

    override val Id: Byte = 3
    override val default: Funcs = Funcs(Vector.empty)

    override def needed(value: Vector[Var]): Boolean = value.nonEmpty

    override val innerCodec: Codec[Vector[Var]] =
      wcodecs.vec(wcodecs.vu32)
  }

  case class Start(
    start: Option[Var]
  ) extends Section

  object Start extends SectionCompanion[Start, Option[Var]] {
    override val Id: Byte = 8
    override val default: Start = Start(None)

    override def needed(value: Option[Var]): Boolean = value.nonEmpty

    override val innerCodec: Codec[Option[Var]] =
      wcodecs.vu32.xmap(Some(_), _.get)
  }

  type Elem = Segment[Vector[Var]]

  case class Elems(
    elems: Vector[Elem]
  ) extends Section

  object Elems extends SectionCompanion[Elems, Vector[Elem]] {
    override val Id: Byte = 9
    override val default: Elems = Elems(Vector.empty)

    override def needed(value: Vector[Elem]): Boolean = value.nonEmpty

    override val innerCodec: Codec[Vector[Elem]] =
      wcodecs.vec(Segment.codec[Vector[Var]](wcodecs.vec(wcodecs.vu32)))
  }

  type Data = Segment[ByteVector]

  case class Datas(
    datas: Vector[Data]
  ) extends Section

  object Datas extends SectionCompanion[Datas, Vector[Data]] {
    override val Id: Byte = 11
    override val default: Datas = Datas(Vector.empty)

    override def needed(value: Vector[Data]): Boolean = value.nonEmpty

    override val innerCodec: Codec[Vector[Data]] =
      wcodecs.vec(Segment.codec[ByteVector](wcodecs.string))
  }

  case class Imports(
    imports: Vector[Import]
  ) extends Section

  object Imports extends SectionCompanion[Imports, Vector[Import]] {
    override val Id: Byte = 2
    override val default: Imports = Imports(Vector.empty)

    override def needed(value: Vector[Import]): Boolean = value.nonEmpty

    override val innerCodec: Codec[Vector[Import]] =
      wcodecs.vec(Codec[Import])
  }

  case class Exports(
    exports: Vector[Export]
  ) extends Section

  object Exports extends SectionCompanion[Exports, Vector[Export]] {
    override val Id: Byte = 7
    override val default: Exports = Exports(Vector.empty)

    override def needed(value: Vector[Export]): Boolean = value.nonEmpty

    override val innerCodec: Codec[Vector[Export]] =
      wcodecs.vec(Codec[Export])
  }

  case class Code(
    locals: Vector[ValueType],
    body: Instructions
  )

  object Code {
    def apply(f: Func): Code =
      Code(f.locals, f.body)

    def compress(ts: Vector[ValueType]): Vector[(Int, ValueType)] = {
      def combine(t: ValueType, accum: Vector[(Int, ValueType)]): Vector[(Int, ValueType)] = {
        accum match {
          case (n, head) +: tail if head == t =>
            (n + 1, t) +: accum

          case _ =>
            (1, t) +: accum
        }
      }

      ts.foldRight[Vector[(Int, ValueType)]](Vector.empty)(combine)
    }

    def decompress(ts: Vector[(Int, ValueType)]): Vector[ValueType] = {
      for {
        (n, local) <- ts
        repeatedLocal <- Vector.fill(n)(local)
      } yield repeatedLocal
    }

    val localCodec: Codec[(Int, ValueType)] =
      wcodecs.len32 ~ ValueType.codec

    val localsCodec: Codec[Vector[ValueType]] =
      wcodecs.vec(localCodec).xmap(
        decompress,
        compress
      )

    implicit val codec: Codec[Code] =
      Codec[Code](
        encoder =
          Encoder { code: Code =>
            val Code(locals, body) = code
            for {
              localsBytes <- localsCodec.encode(locals)
              instructionBytes <- Instructions.thenEnd.encode(body)
              lenBytes <- wcodecs.len32.encode(localsBytes.intSize.get + instructionBytes.intSize.get)
            } yield lenBytes ++ localsBytes ++ instructionBytes
          },
        decoder =
          for {
            locals <- localsCodec
            body <- Instructions.thenEnd
          } yield Code(locals, body)
      )
  }

  case class Codes(
    codes: Vector[Code]
  ) extends Section

  object Codes extends SectionCompanion[Codes, Vector[Code]] {
    override val Id: Byte = 10
    override val default: Codes = Codes(Vector.empty)

    override def needed(value: Vector[Code]): Boolean = value.nonEmpty

    override val innerCodec: Codec[Vector[Code]] =
      wcodecs.vec(Codec[Code])
  }

  case class UserSection(b: BitVector) extends Section

  object UserSection extends SectionCompanion[UserSection, BitVector] {
    override val Id: Byte = 0
    override val default: UserSection = UserSection(BitVector.empty)

    override def needed(value: BitVector): Boolean = value.nonEmpty

    override val innerCodec: Codec[BitVector] =
      codecs.bits

    val ignore: Decoder[Boolean] =
      codecs.recover(Codec[UserSection].xmap(Function.const(()), _ => UserSection.default))
  }

  val version = UInt(1)

  val versionBytes = wcodecs.u32.encode(version).require

  val magicHeader = wcodecs.u32.encode(UInt(0x6d736100)).require

  val magicHeaderCodec =
    codecs.constant(magicHeader)

  implicit val codec: Codec[Module] =
    Codec(
      encoder =
        Encoder[Module] { module: Module =>
          for {
            typesBytes <- Encoder[FuncTypes].encode(FuncTypes(module.types))
            importBytes <- Encoder[Imports].encode(Imports(module.imports))
            funcBytes <- Encoder[Funcs].encode(Funcs.ofFuncs(module.funcs))
            tableBytes <- Encoder[Tables].encode(Tables(module.tables))
            memoryBytes <- Encoder[Memories].encode(Memories(module.memories))
            globalsBytes <- Encoder[Globals].encode(Globals(module.globals))
            exportBytes <- Encoder[Exports].encode(Exports(module.exports))
            startBytes <- Encoder[Start].encode(Start(module.start))
            elemBytes <- Encoder[Elems].encode(Elems(module.elems))
            codeBytes <- Encoder[Codes].encode(Codes(module.funcs.map(Code(_))))
            dataBytes <- Encoder[Datas].encode(Datas(module.data))
          } yield {
            magicHeader ++
              versionBytes ++
              typesBytes ++
              importBytes ++
              funcBytes ++
              tableBytes ++
              memoryBytes ++
              globalsBytes ++
              exportBytes ++
              startBytes ++
              elemBytes ++
              codeBytes ++
              dataBytes
          }
        },
      decoder =
        for {
          _ <- magicHeaderCodec
          decodedVersion <- wcodecs.u32
          _ <-
            if (decodedVersion == version)
              codecs.ignore(0)
            else codecs.fail(Err.apply(s"Expected version $version but got $decodedVersion."))
          _ <- UserSection.ignore
          types <- Codec[FuncTypes]
          _ <- UserSection.ignore
          imports <- Codec[Imports]
          _ <- UserSection.ignore
          funcs <- Codec[Funcs]
          _ <- UserSection.ignore
          tables <- Codec[Tables]
          _ <- UserSection.ignore
          memories <- Codec[Memories]
          _ <- UserSection.ignore
          globals <- Codec[Globals]
          _ <- UserSection.ignore
          exports <- Codec[Exports]
          _ <- UserSection.ignore
          start <- Codec[Start]
          _ <- UserSection.ignore
          elems <- Codec[Elems]
          _ <- UserSection.ignore
          codes <- Codec[Codes]
          _ <- UserSection.ignore
          data <- Codec[Datas]
          _ <- UserSection.ignore
        } yield {
          val composedFunctions =
            for {
              (funcType, code) <- funcs.funcs.zip(codes.codes)
            } yield Func(funcType, code.locals, code.body)

          Module(
            types = types.funcTypes,
            globals = globals.globals,
            tables = tables.tables,
            memories = memories.memories,
            funcs = composedFunctions,
            start = start.start,
            elems = elems.elems,
            data = data.datas,
            imports = imports.imports,
            exports = exports.exports
          )
        }
    )

}
