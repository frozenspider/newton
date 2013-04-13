package org.newtonpolyhedron
import scala.util.matching.Regex
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.entity.vector.IntMathVec.IntMathVecFormat
import org.newtonpolyhedron.entity.vector.MathVector
import org.newtonpolyhedron.entity.vector.VectorFormat
import scala.io.Source
import java.io.File
import org.newtonpolyhedron.ex.WrongFormatException

object InputParser {

  //
  // General
  //
  /** Parses file by given parsers */
  def genParseFile[C, V <: MathVector[C, V], R](file: File,
                                                format: VectorFormat[C, V])(
                                                  read: (Seq[String], VectorFormat[C, V]) => R): R = {
    val lines = {
      val src = Source.fromFile(file)
      val lines = src.getLines
      src.close
      refineLines(lines.toStream)
    }
    read(lines, format)
  }

  /** Parsing function envelop, that reads dimension and strips comment */
  def genParseLines[C, V <: MathVector[C, V], R](lines: Seq[String],
                                                 format: VectorFormat[C, V],
                                                 emptyRes: R)(read: (Int, Seq[String]) => R): R =
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
  def parseVectors[C, V <: MathVector[C, V]](dim: Int)(format: VectorFormat[C, V],
                                                       lines: Seq[String]): IndexedSeq[V] =
    if (lines.isEmpty)
      IndexedSeq.empty
    else {
      def vecFromString(s: String): V = {
        val parsed = s split " " map format.parseElement
        require(parsed.length == dim, "Incorrect line size: '" + s + "', while dim = " + dim)
        format makeVector parsed
      }
      def read(src: Seq[String], content: IndexedSeq[V]): IndexedSeq[V] =
        src.headOption match {
          case None    => content
          case Some(s) => read(src.tail, content :+ vecFromString(s))
        }
      read(lines, IndexedSeq.empty)
    }

  /** Normalizes spacing, trims, removes empty lines */
  def refineLines(lines: Stream[String]) =
    lines map (_ replaceAll ("[\\s]+", " ") trim) filterNot (_.isEmpty)

  //
  // Poly and Cone
  //
  /** @return pointList, commonLimits, basis */
  def parsePolyFromFile[C, V <: MathVector[C, V]](file: File,
                                                  format: VectorFormat[C, V]): (IndexedSeq[V], IndexedSeq[IntMathVec], IndexedSeq[IntMathVec]) =
    genParseFile(file, format)(parsePolyFromLines)

  /** @return pointList, commonLimits, basis */
  def parsePolyFromLines[C, V <: MathVector[C, V]](lines: Seq[String],
                                                   format: VectorFormat[C, V]): (IndexedSeq[V], IndexedSeq[IntMathVec], IndexedSeq[IntMathVec]) = {
    val empty = (IndexedSeq.empty[V], IndexedSeq.empty[IntMathVec], IndexedSeq.empty[IntMathVec])
    genParseLines(lines, format, empty) { (dim, travLines) =>
      val commonLimits = parseSectionIfPresent("$", dim)(IntMathVecFormat, travLines)
      val basis = parseSectionIfPresent("#", dim)(IntMathVecFormat, travLines)
      val pointList = parsePointsSection(Seq("$", "#"), dim)(format, travLines)
      (pointList, commonLimits, basis)
    }
  }

  private def parseSectionIfPresent[C, V <: MathVector[C, V]](separator: String,
                                                              dim: Int)(format: VectorFormat[C, V],
                                                                        lines: Seq[String]): IndexedSeq[V] = {
    val start = lines indexOf separator
    if (start == -1) IndexedSeq.empty
    else {
      val end = lines indexOf (separator, start + 1)
      if (end == -1) throw new WrongFormatException("Section has no end: " + separator)
      else if (lines.indexOf(separator, end + 1) != -1) throw new WrongFormatException("Too many section separators: " + separator)
      else parseVectors(dim)(format, lines slice (start + 1, end))
    }
  }

  private def parsePointsSection[C, V <: MathVector[C, V]](seps: Seq[String],
                                                           dim: Int)(format: VectorFormat[C, V],
                                                                     lines: Seq[String]): IndexedSeq[V] = {
    val start = lines lastIndexWhere (seps contains _) // or -1
    parseVectors(dim)(format, lines slice (start + 1, lines.size))
  }

  //
  // Intersection
  //

  /** @return (poly1, poly2, ...; dim) */
  def parsePolysFromFile[C, V <: MathVector[C, V]](file: File,
                                                   format: VectorFormat[C, V]): (IndexedSeq[IndexedSeq[V]], Int) =
    genParseFile(file, format)(parsePolysFromLines)

  /** @return (poly1, poly2, ...; dim) */
  def parsePolysFromLines[C, V <: MathVector[C, V]](lines: Seq[String],
                                                    format: VectorFormat[C, V]): (IndexedSeq[IndexedSeq[V]], Int) = {
    val sep = "%"
    def readRec(dim: Int,
                travLines: Seq[String],
                acc: IndexedSeq[IndexedSeq[V]]): IndexedSeq[IndexedSeq[V]] =
      if (travLines.isEmpty)
        acc
      else {
        val sections = travLines span (_ != sep)
        val currPoly = parseVectors(dim)(format, sections._1)
        readRec(dim, sections._2 drop 1, acc :+ currPoly)
      }
    val empty = (IndexedSeq.empty[IndexedSeq[V]], 0)
    genParseLines(lines, format, empty) { (dim, travLines) =>
      (readRec(dim, travLines, IndexedSeq.empty), dim)
    }
  }
}