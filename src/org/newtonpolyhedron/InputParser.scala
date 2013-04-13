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
  def readFromFile[C, V <: MathVector[C, V], R](file: File,
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

  def refineLines(lines: Stream[String]) =
    lines map (_ replaceAll ("[\\s]+", " ") trim) filterNot (_.isEmpty)

  def readFromLines[C, V <: MathVector[C, V], R](lines: Seq[String],
                                                 format: VectorFormat[C, V],
                                                 emptyRes: R)(read: (Int, Seq[String]) => R): R =
    if (lines.isEmpty)
      emptyRes
    else {
      require(lines forall (s => !s.isEmpty && !s.contains("\t") && !s.contains("  ") && (s.trim == s)), "Lines should be refined")
      val dim = Integer.parseInt(lines.head)
      val travLines = lines.tail.toIndexedSeq
      read(dim, travLines)
    }

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

  //
  // Poly and Cone
  //
  /** @return pointList, commonLimits, basis */
  def readPolyFromFile[C, V <: MathVector[C, V]](file: File,
                                                 format: VectorFormat[C, V]): (IndexedSeq[V], IndexedSeq[IntMathVec], IndexedSeq[IntMathVec]) =
    readFromFile(file, format)(readPolyFromLines)

  /** @return pointList, commonLimits, basis */
  def readPolyFromLines[C, V <: MathVector[C, V]](lines: Seq[String],
                                                  format: VectorFormat[C, V]): (IndexedSeq[V], IndexedSeq[IntMathVec], IndexedSeq[IntMathVec]) = {
    val empty = (IndexedSeq.empty[V], IndexedSeq.empty[IntMathVec], IndexedSeq.empty[IntMathVec])
    readFromLines(lines, format, empty) { (dim, travLines) =>
      val commonLimits = readSectionIfPresent("$", dim)(IntMathVecFormat, travLines)
      val basis = readSectionIfPresent("#", dim)(IntMathVecFormat, travLines)
      val pointList = readPoints(Seq("$", "#"), "@", dim)(format, travLines)
      (pointList, commonLimits, basis)
    }
  }

  /** @return subsequence, from start to end (inclusively) */
  private def subseq[T](s: Seq[T],
                        start: Int,
                        end: Int) = {
    s drop start take (end - start + 1)
  }

  private def readSectionIfPresent[C, V <: MathVector[C, V]](separator: String,
                                                             dim: Int)(format: VectorFormat[C, V],
                                                                       lines: Seq[String]): IndexedSeq[V] = {
    val start = lines indexOf separator
    if (start == -1) IndexedSeq.empty
    else {
      val end = lines indexOf (separator, start + 1)
      if (end == -1) throw new WrongFormatException("Section has no end: " + separator)
      else if (lines.indexOf(separator, end + 1) != -1) throw new WrongFormatException("Too many section separators: " + separator)
      else parseVectors(dim)(format, subseq(lines, start + 1, end - 1))
    }
  }

  private def readPoints[C, V <: MathVector[C, V]](seps: Seq[String],
                                                   commentSep: String,
                                                   dim: Int)(format: VectorFormat[C, V],
                                                             lines: Seq[String]): IndexedSeq[V] = {
    val start = lines lastIndexWhere (seps contains _) // or -1
    val end = {
      val endOption = lines lastIndexOf (commentSep)
      if (endOption == -1) lines.size else endOption
    }
    parseVectors(dim)(format, subseq(lines, start + 1, end - 1))
  }
}