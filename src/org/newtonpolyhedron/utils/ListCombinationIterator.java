package org.newtonpolyhedron.utils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.fs.utils.ArrayUtils;

/**
 * Returns the element combination of the given width from the list. List should not be changed
 * during iteration.
 * <p>
 * Removing is not supported.
 * <p>
 * Example:
 * <p>
 * {@code ListCombinationIterator(Arrays.asList("a", "b", "c", "d"), 3)} will return
 * {@code ["a","b","c"]}, {@code ["a","b","d"]}, {@code ["a","c","d"]} and {@code ["b","c","d"]}
 * 
 * @author FS
 * @param <T>
 */
public class ListCombinationIterator<T> implements Iterator <List <T>> {
	
	private final List <T>	list;
	private final int		width;
	private int[]			indices	= null;
	private final int		listSize;
	
	public ListCombinationIterator(final List <T> list, final int width) {
		if (width <= 0) throw new IllegalArgumentException("width must be positive");
		this.list = list;
		this.width = width;
		this.listSize = list.size();
	}
	
	@Override
	public boolean hasNext() {
		if (indices == null) return width <= list.size();
		boolean lastCombinationApproached = true;
		for (int i = 0; i < width; ++i) {
			if (indices[i] != listSize - width + i) {
				lastCombinationApproached = false;
				break;
			}
		}
		if (lastCombinationApproached) return false;
		return true;
	}
	
	@Override
	public List <T> next() {
		scrollIndex();
		final List <T> result = new ArrayList <T>(indices.length);
		for (final int idx : indices) {
			result.add(list.get(idx));
		}
		return result;
	}
	
	/** @return a non-backed list of current indices (can be freely modified). */
	public List <Integer> getIndices() {
		return new ArrayList <Integer>(ArrayUtils.asList(indices));
	}
	
	private void scrollIndex() throws NoSuchElementException {
		if (indices == null) {
			if (width > listSize) throw new NoSuchElementException();
			this.indices = new int[width];
			for (int i = 0; i < width; ++i) {
				this.indices[i] = i;
			}
			return;
		}
		final int last = width - 1;
		int cPos = last;
		while (true) {
			++indices[cPos];
			int inc = 1;
			for (int i = cPos + 1; i <= last; ++i) {
				indices[i] = indices[cPos] + inc;
				++inc;
			}
			if (indices[last] >= listSize) {
				--cPos;
			} else {
				break;
			}
			if (cPos < 0) throw new NoSuchElementException();
		}
	}
	
	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}
}
