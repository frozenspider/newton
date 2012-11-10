package org.newtonpolyhedron.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.ListIterator;

public class StringUtils extends org.apache.commons.lang3.StringUtils {
	
	public static String appendToRight(int spaceWidth, CharSequence... strs) {
		List <List <String>> linesList = new ArrayList <List <String>>();
		for (CharSequence str : strs) {
			linesList.add(Arrays.asList(str.toString().split("[\\r\\n]+")));
		}
		
		// Make lists same size
		int maxSize = 0;
		for (List <String> lines : linesList) {
			maxSize = Math.max(maxSize, lines.size());
		}
		for (List <String> lines : linesList) {
			lines.addAll(Collections.nCopies(maxSize - lines.size(), ""));
		}
		if (maxSize == 0) return "";
		
		List <Integer> maxLengths = new ArrayList <Integer>();
		for (List <String> lines : linesList) {
			int maxLen = 0;
			for (String line : lines) {
				maxLen = Math.max(maxLen, line.length());
			}
			maxLengths.add(maxLen);
		}
		
		// Right-pad all lines with spaces (including separator)
		for (int i = 0; i < linesList.size() - 1; i++) {
			int maxLen = maxLengths.get(i);
			for (ListIterator <String> iter = linesList.get(i).listIterator(); iter.hasNext();) {
				iter.set(rightPad(iter.next(), maxLen + spaceWidth));
			}
		}
		
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < maxSize; i++) {
			for (List <String> lines : linesList) {
				sb.append(lines.get(i));
			}
			sb.append("\n");
		}
		return sb.toString().trim();
	}
}
