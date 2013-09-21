package org.newtonpolyhedron
import scala.util.matching.Regex
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.entity.vector.MathVector
import org.newtonpolyhedron.entity.vector.VectorFormat
import scala.io.Source
import java.io.File
import org.newtonpolyhedron.ex.WrongFormatException
import org.newtonpolyhedron.entity.Matrix
import org.apache.commons.math3.FieldElement
import org.newtonpolyhedron.entity.vector.MathVector
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.Product

object InputParser {
  private type Vec[C, V <: Vec[C, V]] = MathVector[C, V]
  private type VecFmt[C, V <: Vec[C, V]] = VectorFormat[C, V]
  private type Lines = Seq[String]
  private type ISeq[V] = IndexedSeq[V]
  private type IV = IntMathVec
  private type FV = FracMathVec
  private type El[T] = FieldElement[T]

  private val intFmt = IntMathVec.IntMathVecFormat
  private val fracFmt = FracMathVec.FracMathVecFormat

  //
  // General
  //
  /** Parses file by given parsers */
  def genParseFile[R](file: File)(read: Lines => R): R = {
    val lines = {
      val src = Source.fromFile(file)
      val lines = src.getLines.toIndexedSeq
      src.close
      refineLines(lines)
    }
    read(lines)
  }

  /** Parsing function envelop, that reads dimension and strips comment */
  def genParseLines[R](lines: Lines, emptyRes: => R)(read: (Int, Lines) => R): R =
    if (lines.isEmpty)
      emptyRes
    else {
      require(lines forall (s => !s.isEmpty && !s.contains("\t") && !s.contains("  ") && (s.trim == s)), "Lines should be refined")
      val dim = Integer.parseInt(lines.head)
      val travLines = lines.tail.toIndexedSeq
      val linesNoComment = {
        val commentIdx = travLines indexOf "@"
        if (commentIdx == -1) travLines else travLines take commentIdx
      }
      read(dim, linesNoComment)
    }

  /** Parses consequent vectors using given format */
  def parseVectors[C, V <: Vec[C, V]](dim: Int)(format: VecFmt[C, V],
                                                lines: Lines): ISeq[V] =
    if (lines.isEmpty)
      IndexedSeq.empty
    else {
      def vecFromString(s: String): V = {
        val parsed = s split " " map format.parseElement
        require(parsed.length == dim, "Incorrect line size: '" + s + "', while dim = " + dim)
        format makeVector parsed
      }
      def read(src: Lines, content: ISeq[V]): ISeq[V] =
        src.headOption match {
          case None    => content
          case Some(s) => read(src.tail, content :+ vecFromString(s))
        }
      read(lines, IndexedSeq.empty)
    }

  /** Normalizes spacing, trims, removes empty lines */
  def refineLines(lines: Seq[String]) =
    lines map (_ replaceAll ("[\\s]+", " ") trim) filterNot (_.isEmpty)

  //
  // Poly and Cone
  //
  /** @return pointList, commonLimits, basis */
  def parsePolyFromFile[C, V <: Vec[C, V]](file: File,
                                           format: VecFmt[C, V]): (ISeq[V], ISeq[IV], ISeq[IV]) =
    genParseFile(file)(parsePolyFromLines(format))

  /** @return pointList, commonLimits, basis */
  def parsePolyFromLines[C, V <: Vec[C, V]](format: VecFmt[C, V])(lines: Lines) = {
    val empty = (IndexedSeq.empty[V], IndexedSeq.empty[IV], IndexedSeq.empty[IV])
    genParseLines(lines, empty) { (dim, travLines) =>
      val commonLimits = parseSectionIfPresent("$", dim)(intFmt, travLines)
      val basis = parseSectionIfPresent("#", dim)(intFmt, travLines)
      val pointList = parsePointsSection(Seq("$", "#"), dim)(format, travLines)
      (pointList, commonLimits, basis)
    }
  }

  private def parseSectionIfPresent[C, V <: Vec[C, V]](separator: String,
                                                       dim: Int)(format: VecFmt[C, V],
                                                                 lines: Lines): ISeq[V] = {
    val start = lines indexOf separator
    if (start == -1) IndexedSeq.empty
    else {
      val end = lines indexOf (separator, start + 1)
      if (end == -1) throw new WrongFormatException("Section has no end: " + separator)
      else if (lines.indexOf(separator, end + 1) != -1) throw new WrongFormatException("Too many section separators: " + separator)
      else parseVectors(dim)(format, lines slice (start + 1, end))
    }
  }

  private def parsePointsSection[C, V <: Vec[C, V]](seps: Lines,
                                                    dim: Int)(format: VecFmt[C, V],
                                                              lines: Lines): ISeq[V] = {
    val start = lines lastIndexWhere (seps contains _) // or -1
    parseVectors(dim)(format, lines slice (start + 1, lines.size))
  }

  //
  // Intersection
  //
  /** @return (poly1, poly2, ...; dim) */
  def parsePolysFromFile[C, V <: Vec[C, V]](file: File,
                                            format: VecFmt[C, V]): (ISeq[ISeq[V]], Int) =
    genParseFile(file)(parsePolysFromLines(format))

  /** @return (poly1, poly2, ...; dim) */
  def parsePolysFromLines[C, V <: Vec[C, V]](format: VecFmt[C, V])(lines: Lines): (ISeq[ISeq[V]], Int) = {
    val sep = "%"
    def readRec(dim: Int,
                travLines: Lines,
                acc: ISeq[ISeq[V]]): ISeq[ISeq[V]] =
      if (travLines.isEmpty)
        acc
      else {
        val sections = travLines span (_ != sep)
        val currPoly = parseVectors(dim)(format, sections._1)
        readRec(dim, sections._2 drop 1, acc :+ currPoly)
      }
    val empty = (IndexedSeq.empty[ISeq[V]], 0)
    genParseLines(lines, empty) { (dim, travLines) =>
      (readRec(dim, travLines, IndexedSeq.empty), dim)
    }
  }

  //
  // Matrix
  //
  /** @return matrix */
  def parseMatrixFromFile[C, V <: Vec[C, V], R <: El[R]](file: File,
                                                         format: VecFmt[C, V],
                                                         fabric: Seq[V] => Matrix[R]): Matrix[R] = {
    val res = genParseFile(file)(parseMatrixFromLines(format, fabric))
    if (res.isDefined) res.get
    else throw new WrongFormatException("Matrix was empty")
  }

  /** @return matrix */
  def parseMatrixFromLines[C, V <: Vec[C, V], R <: El[R]](format: VecFmt[C, V], fabric: Seq[V] => Matrix[R])(lines: Lines): Option[Matrix[R]] = {
    val empty: Option[Matrix[R]] = None
    genParseLines(lines, empty) { (dim, travLines) =>
      val vecs = parseVectors(dim)(format, travLines)
      Some(fabric(vecs))
    }
  }

  /** @return (matrix, skipRow, skipCol) */
  def parseMatrixWithSkipFromFile[C, V <: Vec[C, V], R <: El[R]](file: File,
                                                                 format: VecFmt[C, V],
                                                                 fabric: Seq[V] => Matrix[R]): (Matrix[R], Int, Int) = {
    val res = genParseFile(file)(parseMatrixWithSkipFromLines(format, fabric))
    if (res.isDefined) res.get
    else throw new WrongFormatException("Matrix was empty")
  }

  /** @return (matrix, skipRow, skipCol) */
  def parseMatrixWithSkipFromLines[C, V <: Vec[C, V], R <: El[R]](format: VecFmt[C, V], fabric: Seq[V] => Matrix[R])(lines: Lines): Option[(Matrix[R], Int, Int)] = {
    val empty: Option[(Matrix[R], Int, Int)] = None
    genParseLines(lines, empty) { (dim, travLines) =>
      val firstLine = travLines.head split " " map (_.toInt)
      if (firstLine.size != 2) throw new WrongFormatException("Second line must be two numbers - row and column to skip (or -1)")
      val (skipRow, skipCol) = (firstLine(0), firstLine(1))
      val vecs = parseVectors(dim)(format, travLines.tail)
      Some((fabric(vecs), skipRow, skipCol))
    }
  }

  def parsePowerTransfBaseFromFile(file: File): (Polynomial, Polynomial, Int, Int, Int, Int) = {
    genParseFile(file)(parsePowerTransfBaseFromLines)
  }

  def parsePowerTransfBaseFromLines(lines: Lines): (Polynomial, Polynomial, Int, Int, Int, Int) = {
    def empty = throw new WrongFormatException("File was empty")
    genParseLines(lines, empty)(parsePowerTransfBaseFromRefLines)
  }

  def parsePowerTransfBaseFromRefLines(dim: Int, lines: Lines): (Polynomial, Polynomial, Int, Int, Int, Int) = {
    val parts = lines.splitBySkippingDelim(_ == "%")
    if (dim != 3) throw new WrongFormatException("For now can handle only 3D polys")
    if (parts.size != dim) throw new WrongFormatException("Incorrect file format or wrong number of sections")
    val poly1 = parsePowerTransfBasePoly(dim, parts(0))
    val poly2 = parsePowerTransfBasePoly(dim, parts(1))
    val indices = parts(2) flatMap (_ split ' ') map (Integer.parseInt)
    (poly1, poly2, indices(0), indices(1), indices(2), indices(3))
  }

  def parsePowerTransfBasePoly(dim: Int, lines: Lines): Polynomial = {
    val fmt = FracMathVec.FracMathVecFormat
    val res = lines map { line =>
      val split = line split ' '
      if (!split.head.endsWith(",")) throw new WrongFormatException(s"Incorrect line format '$line'")
      val coeff = Integer.parseInt(split.head.dropRight(1).trim)
      val powers = fmt.makeVector(split.tail map (fmt.parseElement))
      new Term(Product(coeff), powers)
    }
    res.toIndexedSeq
  }
}