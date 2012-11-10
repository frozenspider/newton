package org.newtonpolyhedron.solve.minkowski;

import java.util.ArrayList;
import java.util.List;

import org.fs.utils.structure.wrap.Wrapper;
import org.newtonpolyhedron.entity.vector.IntVector;

/** @author Alexander "FS" Abdugafarov */
public class SolveMinkowskiImpl extends Thread {
	
	private double processMinkowski(List <List <IntVector>> points) {
		int dim = points.get(0).get(0).getDim();
		int len = points.size();
		
		List <Integer> pointsExclusion = new ArrayList <Integer>();
		List <Integer> cover = new ArrayList <Integer>();
		
		double minkowskiVolume = 0.0d;
		int maxC = len;
		boolean positive = true;
		while (maxC > 0) {
			int[] x = new int[maxC];
			for (int i = 0; i < maxC; i++)
				x[i] = i;
			// ==== ++ Cycle through all combinations ====
			// boolean canBeContinued = true;
			while (true) {
				pointsExclusion.clear();
				List <IntVector> currComboPtsList = points.get(0);
				if (maxC == 1) {
					cover.clear();
					Wrapper <Double> volumeWrapper = new Wrapper <Double>();
					ComputingFunctions.polyhedronFindCover(output, dim, points[x[0]],
							pointsExclusion, cover, null, null, volumeWrapper,
							CoreUtils.getCenter_Nd(dim, points[x[0]]));
					double volume = volumeWrapper.getObject();
					minkowskiVolume += positive ? volume : -volume;
				} else {
					int comboN = 1;
					while (comboN < maxC) {
						int v1L = currComboPtsList.size();
						int v2L = points[x[comboN]].size();
						List <PointNd> nextComboPtsList = new ArrayList <PointNd>(v1L * v2L);
						for (int i = 0; i < v2L; i++) {
							PointNd pt1 = points[x[comboN]].get(i);
							for (int j = 0; j < v1L; j++)
								nextComboPtsList.add(pt1.add(currComboPtsList.get(j)));
						}
						currComboPtsList = nextComboPtsList;
						comboN++;
					}
					cover.clear();
					Wrapper <Double> volumeWrapper = new Wrapper <Double>();
					ComputingFunctions.polyhedronFindCover(dim, currComboPtsList, pointsExclusion,
							cover, null, null, volumeWrapper,
							CoreUtils.getCenter_Nd(dim, currComboPtsList));
					double volume = volumeWrapper.getObject();
					minkowskiVolume += positive ? volume : -volume;
				}// */
				if (!ComputingFunctions.cycleThroughCombo_next(x, len - 1)) break;
			}
			maxC--;
			positive = !positive;
			// ==== -- Cycle through all combinations ====
		}
		return minkowskiVolume;
	}
}
