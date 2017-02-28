package org.newtonpolyhedron

import java.io.File

import scala.io.Source

import org.newtonpolyhedron.entity.Product
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.matrix.Matrix
import org.newtonpolyhedron.ex.WrongFormatException
import org.newtonpolyhedron.utils.LanguageImplicits._
import org.newtonpolyhedron.utils.PolynomialUtils._
import org.newtonpolyhedron.utils.parsing.ParseFormats._
import spire.math.Rational

object InputParser {
  private type Lines = Seq[String]
  private type ISeq[E] = IndexedSeq[E]
  private type ISeqSeq[E] = ISeq[ISeq[E]]
  private type OptISeqSeq[E] = Option[ISeqSeq[E]]
  private type ISeqSeqSeq[E] = ISeq[ISeqSeq[E]]
  private type IV = ISeq[BigInt]
  private type FV = ISeq[Rational]
  private type MatrixFactory[A] = (ISeqSeq[A] => Matrix[A])

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
  def parseVectors[C](dim: Int)(lines: Lines)(parseElement: Parse[C]): ISeqSeq[C] =
    if (lines.isEmpty)
      IndexedSeq.empty
    else {
      def vecFromString(s: String): ISeq[C] = {
        val parsed = s split " " map parseElement
        require(parsed.length == dim, "Incorrect line size: '" + s + "', while dim = " + dim)
        parsed
      }
      def read(src: Lines, content: ISeqSeq[C]): ISeqSeq[C] =
        src.headOption match {
          case None    => content
          case Some(s) => read(src.tail, content :+ vecFromString(s))
        }
      read(lines, IndexedSeq.empty)
    }

  /** Normalizes spacing, trims, removes empty lines */
  def refineLines(lines: Seq[String]) =
    for {
      l <- lines
      refined = l.replaceAll("[\\s]+", " ").trim
      if (!refined.isEmpty)
    } yield refined

  //
  // Poly and Cone
  //
  /** @return pointList, commonLimits, basis */
  def parsePolyFromFile[C](file: File)(parseElement: Parse[C]): (ISeqSeq[C], OptISeqSeq[BigInt], OptISeqSeq[BigInt]) =
    genParseFile(file)(lines => parsePolyFromLines(lines)(parseElement))

  /** @return pointList, commonLimits, basis */
  def parsePolyFromLines[C](lines: Lines)(parseElement: Parse[C]) = {
    val empty: (ISeqSeq[C], OptISeqSeq[BigInt], OptISeqSeq[BigInt]) =
      (IndexedSeq.empty[ISeq[C]], None, None)
    genParseLines(lines, empty) { (dim, travLines) =>
      val commonLimits = parseSectionIfPresent("$", dim)(travLines)(parseInt)
      val basis = parseSectionIfPresent("#", dim)(travLines)(parseInt)
      val pointList = parsePointsSection(Seq("$", "#"), dim)(travLines)(parseElement)
      (pointList, commonLimits, basis)
    }
  }

  private def parseSectionIfPresent[C](separator: String, dim: Int)(lines: Lines)(parseElement: Parse[C]): OptISeqSeq[C] = {
    val start = lines indexOf separator
    if (start == -1) None
    else {
      val end = lines indexOf (separator, start + 1)
      if (end == -1) throw new WrongFormatException("Section has no end: " + separator)
      else if (lines.indexOf(separator, end + 1) != -1) throw new WrongFormatException("Too many section separators: " + separator)
      else Some(parseVectors(dim)(lines slice (start + 1, end))(parseElement))
    }
  }

  private def parsePointsSection[C](seps: Lines, dim: Int)(lines: Lines)(parseElement: Parse[C]): ISeqSeq[C] = {
    val start = lines lastIndexWhere (seps contains _) // or -1
    parseVectors(dim)(lines slice (start + 1, lines.size))(parseElement)
  }

  //
  // Intersection
  //
  /** @return (poly1, poly2, ...; dim) */
  def parsePolysFromFile[C](file: File)(parseElement: Parse[C]): (ISeqSeqSeq[C], Int) =
    genParseFile(file)(lines => parsePolysFromLines(lines)(parseElement))

  /** @return (poly1, poly2, ...; dim) */
  def parsePolysFromLines[C](lines: Lines)(parseElement: Parse[C]): (ISeqSeqSeq[C], Int) = {
    val sep = "%"
    def readRec(dim: Int,
                travLines: Lines,
                acc: ISeqSeqSeq[C]): ISeqSeqSeq[C] =
      if (travLines.isEmpty)
        acc
      else {
        val sections = travLines span (_ != sep)
        val currPoly = parseVectors(dim)(sections._1)(parseElement)
        readRec(dim, sections._2 drop 1, acc :+ currPoly)
      }
    val empty = (IndexedSeq.empty[ISeqSeq[C]], 0)
    genParseLines(lines, empty) { (dim, travLines) =>
      (readRec(dim, travLines, IndexedSeq.empty), dim)
    }
  }

  //
  // Matrix
  //
  /** @return matrix */
  def parseMatrixFromFile[R](file: File, mFactory: MatrixFactory[R])(parseElement: Parse[R]): Matrix[R] = {
    val res = genParseFile(file)(lines => parseMatrixFromLines(lines)(mFactory, parseElement))
    if (res.isDefined) res.get
    else throw new WrongFormatException("Matrix was empty")
  }

  /** @return matrix */
  def parseMatrixFromLines[R](lines: Lines)(mFactory: MatrixFactory[R], parseElement: Parse[R]): Option[Matrix[R]] = {
    val empty: Option[Matrix[R]] = None
    genParseLines(lines, empty) { (dim, travLines) =>
      val vecs = parseVectors(dim)(travLines)(parseElement)
      Some(mFactory(vecs))
    }
  }

  /** @return (matrix, skipRow, skipCol) */
  def parseMatrixWithSkipFromFile[R](file: File, mFactory: MatrixFactory[R])(parseElement: Parse[R]): (Matrix[R], Int, Int) = {
    val res = genParseFile(file)(lines => parseMatrixWithSkipFromLines(lines)(mFactory, parseElement))
    if (res.isDefined) res.get
    else throw new WrongFormatException("Matrix was empty")
  }

  /** @return (matrix, skipRow, skipCol) */
  def parseMatrixWithSkipFromLines[R](lines: Lines)(mFactory: MatrixFactory[R], parseElement: Parse[R]): Option[(Matrix[R], Int, Int)] = {
    val empty: Option[(Matrix[R], Int, Int)] = None
    genParseLines(lines, empty) { (dim, travLines) =>
      val firstLine = travLines.head split " " map (_.toInt)
      if (firstLine.size != 2) throw new WrongFormatException("Second line must be two numbers - row and column to skip (or -1)")
      val (skipRow, skipCol) = (firstLine(0), firstLine(1))
      val vecs = parseVectors(dim)(travLines.tail)(parseElement)
      Some((mFactory(vecs), skipRow, skipCol))
    }
  }

  def parsePowerTransfBaseFromFile(file: File): (Polys, ISeqSeq[Int]) = {
    genParseFile(file)(parsePowerTransfBaseFromLines)
  }

  def parsePowerTransfBaseFromLines(lines: Lines): (Polys, ISeqSeq[Int]) = {
    def empty = throw new WrongFormatException("File was empty")
    genParseLines(lines, empty)(parsePowerTransfBaseFromRefLines)
  }

  def parsePowerTransfBaseFromRefLines(dim: Int, lines: Lines): (Polys, ISeqSeq[Int]) = {
    if (dim != 3) throw new WrongFormatException("For now can handle only 3D polys")
    val (polyLines, chosenLines) = {
      val delimIdx = lines.indexOf("#")
      if (delimIdx == -1) throw new WrongFormatException("Please separate chosen points by #")
      val split = lines.splitAt(delimIdx)
      (split._1, split._2.drop(1))
    }
    val polyStringGroups = polyLines.splitBySkippingDelim(_ == "%")
    if (polyStringGroups.size != (dim - 1)) throw new WrongFormatException("Incorrect file format or wrong number of sections")
    val polys = polyStringGroups map (part => parsePowerTransfBasePoly(dim, part))
    val indices = chosenLines.toIndexedSeq map { line =>
      (line split ' ' map parseInt map (_.toInt)).toIndexedSeq
    }
    (polys, indices)
  }

  def parsePowerTransfBasePoly(dim: Int, lines: Lines): Polynomial = {
    val res = lines map { line =>
      val split = (line split ' ').toVector
      val coeffStr = split.head.trim
      if (!coeffStr.endsWith(",")) throw new WrongFormatException(s"Incorrect line format '$line'")
      val coeff = parseInt(coeffStr.dropRight(1))
      val powers = split.tail map parseFrac
      new Term(Product(coeff), powers)
    }
    res.toIndexedSeq
  }
}
