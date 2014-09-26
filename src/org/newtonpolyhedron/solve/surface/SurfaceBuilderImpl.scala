package org.newtonpolyhedron.solve.surface

import scala.collection.immutable.SortedSet

import org.fs.utils.collection.table.KeyTable
import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.Surface
import org.newtonpolyhedron.entity.vector.VectorImports._

class SurfaceBuilderImpl extends SurfaceBuilder {
  override def surfaces(lookupTable: KeyTable[IntVec, Int, Boolean],
                        dim: Int): Map[Int, SortedSet[Surface]] = {
    val lookupData = extactLookupTableData(lookupTable)
    val surfacesMap = gatherSurfacesMap(dim, lookupData)
    surfacesMap
  }

  private def gatherSurfacesMap(dim: Int,
                                lookupData: Seq[Seq[Int]]): Map[Int, SortedSet[Surface]] = {
    def recurse(targetDim: Int,
                result: Map[Int, SortedSet[Surface]],
                prevSurface: Set[Surface]): Map[Int, SortedSet[Surface]] = {
      if (targetDim < 0) result
      else {
        val surfaces = findCommonSurfaces(dim, targetDim, prevSurface, lookupData)
        recurse(targetDim - 1, result + ((targetDim, surfaces)), surfaces)
      }
    }
    recurse(dim - 1, Map.empty, Set.empty)
  }

  def extactLookupTableData(lookup: KeyTable[IntVec, Int, Boolean]): Seq[Seq[Int]] = {
    import org.newtonpolyhedron.utils.ScalaJavaConversionUtils._
    for {
      point <- lookup.rowKeyList
    } yield for {
      colKey <- scala.collection.JavaConversions.asScalaBuffer(lookup.colKeyList)
      if lookup.get(point, colKey)
    } yield colKey
  }

  /**
   * Finds the common surfaces of a lesser dimension
   *
   * @param polyDim
   *            polyhedron dimension
   * @param targetDim
   *            a target dimension
   * @param upperLevelSurfaces
   *            surfaces of an upper level
   * @param lookupTableData
   *            data from lookup table (i.e. table without keys)
   * @return list of surfaces of a lower dimension
   */
  def findCommonSurfaces(polyDim: Int,
                         targetDim: Int,
                         upperLevelSurfaces: Set[Surface],
                         lookupTableData: Seq[Seq[Int]]): SortedSet[Surface] = {
    require(polyDim >= 2, "Polyhedron dimension >= 2")
    require(targetDim >= 0, "Target dimension must be nonnegative")
    require(targetDim < polyDim, "Target dimension must be < poly dimension")
    val width = polyDim - targetDim

    //
    // Forming naked unchecked surfaces
    //
    // List may contains semi-duplicates and nothing is yet known
    // about higher-dimension surface associations
    //
    val surfacesLists: Seq[Seq[Surface]] = (for {
      pointListCombination <- lookupTableData.combinations(width)
    } yield {
      val commonPoints = getCommonPoints(pointListCombination)
      val res = if (targetDim > 0) {
        // Common surface
        if (commonPoints.size > targetDim) {
          Seq(new Surface(commonPoints))
        } else {
          Seq.empty
        }
      } else {
        // Vertex
        for (commonPoint <- commonPoints) yield new Surface(Seq(commonPoint))
      }
      res
    }).toIndexedSeq
    val surfaces: Set[Surface] = surfacesLists.flatten.toSet
    val surfaces2 = removeSemiDuplicates(surfaces)
    val surfaces3 = gatherHigherLevelSurfacesInfo(surfaces2, upperLevelSurfaces, targetDim, polyDim)
    surfaces3
  }

  private def getCommonPoints(pointListCombination: Seq[Seq[Int]]): Seq[Int] =
    if (pointListCombination.isEmpty) Seq.empty
    else pointListCombination reduceLeft ((commonPts, pts) => commonPts filter (pts contains _))

  private def removeSemiDuplicates(surfaces: Set[Surface]): Set[Surface] = {
    surfaces.filterNot { surface =>
      // A list, containing all surfaces but current
      val others = surfaces filterNot (_ == surface)
      surfacesContainsGiven(surface, others)
    }
  }

  private def gatherHigherLevelSurfacesInfo(surfaces: Set[Surface],
                                            upperLevelSurfaces: Set[Surface],
                                            tagetDim: Int,
                                            polyDim: Int): Set[Surface] = {
    val surfaceOpts = surfaces map { currentSurface =>
      val superior = surfacesConatiningGiven(currentSurface, upperLevelSurfaces)
      if (tagetDim == 0 && (superior.size < polyDim - 1))
        None
      else
        Some(currentSurface.addUpperSurfaces(superior))
    }
    surfaceOpts.yieldDefined
  }

  /**
   * Checks, whether the list already contains a supersurface, which is superior to the given
   * surface (in other words, contains the given surface as a subsurface). If <code>true</code>,
   * child surface should not be added to a list (as it is indirectly there).
   * <p>
   * Example:
   * <p>
   * Surface <code>A = {1,2,4,6}</code> is superior to the surface <code>B = {1,4,6}</code>, as
   * {@code A} contains all points of {@code B}
   *
   * @param upperLevelSurfaces
   *            list of known surfaces
   * @param surface
   *            new surface to test
   * @return <code>true</code>, if list contains a surface, which is superior to the given.
   * @see #surfacesConatiningGiven(Surface, Collection)
   */
  def surfacesContainsGiven(surface: Surface, upperLevelSurfaces: Set[Surface]): Boolean =
    upperLevelSurfaces exists (surface.pointIndices subsetOf _.pointIndices)

  /**
   * Gather surfaces superior to the given surface. For more information, see
   * {@link #surfacesConatinsGiven(List, Surface)}.
   * <p>
   * The only difference is a result - this method returns a list of superior surface indices
   * rather than just a boolean.
   *
   * @param upperLevelSurfaces
   *            list of known surfaces
   * @param surface
   *            new surface to test
   * @return list of superior surfaces
   * @see #surfacesContainsGiven(Surface, Collection)
   */
  def surfacesConatiningGiven(surface: Surface, upperLevelSurfaces: Set[Surface]): Set[Surface] =
    upperLevelSurfaces filter (surface.pointIndices subsetOf _.pointIndices)
}
