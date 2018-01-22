package org.newtonpolyhedron.solve.face

import scala.collection.immutable.SortedSet

import org.fs.utility.collection.table.KeyTable
import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.entity.Face

class FaceBuilderImpl extends FaceBuilder {
  override def faces(
    lookupTable: KeyTable[IntVec, Int, Boolean],
    dim:         Int
  ): Map[Int, SortedSet[Face]] = {
    val lookupData = extactLookupTableData(lookupTable)
    val facesMap = gatherFacesMap(dim, lookupData)
    facesMap
  }

  private def gatherFacesMap(
    dim:        Int,
    lookupData: Seq[Seq[Int]]
  ): Map[Int, SortedSet[Face]] = {
    def recurse(
      targetDim: Int,
      result:    Map[Int, SortedSet[Face]],
      prevFace:  Set[Face]
    ): Map[Int, SortedSet[Face]] = {
      if (targetDim < 0) result
      else {
        val faces = findCommonFaces(dim, targetDim, prevFace, lookupData)
        recurse(targetDim - 1, result + ((targetDim, faces)), faces)
      }
    }
    recurse(dim - 1, Map.empty, Set.empty)
  }

  def extactLookupTableData(lookup: KeyTable[IntVec, Int, Boolean]): Seq[Seq[Int]] = {
    for {
      point <- lookup.rowKeys
    } yield for {
      colKey <- lookup.colKeys if lookup.isDefinedAt(point, colKey)
    } yield colKey
  }

  /**
   * Finds the common faces of a lesser dimension
   *
   * @param polyDim
   *            polyhedron dimension
   * @param targetDim
   *            a target dimension
   * @param upperLevelFaces
   *            faces of an upper level
   * @param lookupTableData
   *            data from lookup table (i.e. table without keys)
   * @return list of faces of a lower dimension
   */
  def findCommonFaces(
    polyDim:         Int,
    targetDim:       Int,
    upperLevelFaces: Set[Face],
    lookupTableData: Seq[Seq[Int]]
  ): SortedSet[Face] = {
    require(polyDim >= 2, "Polyhedron dimension >= 2")
    require(targetDim >= 0, "Target dimension must be nonnegative")
    require(targetDim < polyDim, "Target dimension must be < poly dimension")
    val width = polyDim - targetDim

    //
    // Forming naked unchecked faces
    //
    // List may contains semi-duplicates and nothing is yet known
    // about higher-dimension face associations
    //

    val facesLists: Seq[Seq[Face]] = (for {
      pointListCombination <- lookupTableData.combinations(width)
    } yield {
      val commonPoints = getCommonPoints(pointListCombination)
      val res = if (targetDim > 0) {
        // Common face
        if (commonPoints.size > targetDim) {
          Seq(new Face(commonPoints))
        } else {
          Seq.empty
        }
      } else {
        // Vertex
        for (commonPoint <- commonPoints) yield new Face(Seq(commonPoint))
      }
      res
    }).toIndexedSeq
    val faces: Set[Face] = facesLists.flatten.toSet
    val faces2 = removeSemiDuplicates(faces)
    val faces3 = gatherHigherLevelFacesInfo(faces2, upperLevelFaces, targetDim, polyDim)
    setToSorted(faces3)
  }

  private def getCommonPoints(pointListCombination: Seq[Seq[Int]]): Seq[Int] =
    if (pointListCombination.isEmpty) Seq.empty
    else pointListCombination reduceLeft ((commonPts, pts) => commonPts filter (pts contains _))

  private def removeSemiDuplicates(faces: Set[Face]): Set[Face] = {
    faces.filterNot { face =>
      // List containing all faces but current
      val others = faces filterNot (_ == face)
      facesContainsGiven(face, others)
    }
  }

  private def gatherHigherLevelFacesInfo(
    faces:           Set[Face],
    upperLevelFaces: Set[Face],
    tagetDim:        Int,
    polyDim:         Int
  ): Set[Face] = {
    val faceOpts = faces map { currentFace =>
      val superior = facesContainingGiven(currentFace, upperLevelFaces)
      if (tagetDim == 0 && (superior.size < polyDim - 1))
        None
      else
        Some(currentFace.addUpperFaces(superior))
    }
    faceOpts.yieldDefined
  }

  /**
   * Checks, whether the list already contains a superface, which is superior to the given
   * face (in other words, contains the given face as a subface). If <code>true</code>,
   * child face should not be added to a list (as it is indirectly there).
   *
   * Example:
   *
   * Face <code>A = {1,2,4,6}</code> is superior to the face <code>B = {1,4,6}</code>, as
   * `A` contains all points of `B`
   *
   * @param face
   *            new face to test
   * @param upperLevelFaces
   *            list of known faces
   * @return <code>true</code>, if list contains a face which is superior to the given.
   * @see #facesContainingGiven(Face, Collection)
   */
  def facesContainsGiven(face: Face, upperLevelFaces: Set[Face]): Boolean =
    upperLevelFaces exists (face.pointIndices subsetOf _.pointIndices)

  /**
   * Gather faces superior to the given face. For more information, see
   * {@link #facesConatinsGiven(List, Face)}.
   *
   * The only difference is a result - this method returns a list of superior face indices
   * rather than just a boolean.
   *
   * @param face
   *            new face to test
   * @param upperLevelFaces
   *            list of known faces
   * @return list of superior faces
   * @see #facesContainsGiven(Face, Collection)
   */
  def facesContainingGiven(face: Face, upperLevelFaces: Set[Face]): Set[Face] =
    upperLevelFaces filter (face.pointIndices subsetOf _.pointIndices)
}
