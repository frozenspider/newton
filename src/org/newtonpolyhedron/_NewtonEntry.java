package org.newtonpolyhedron;

import javax.swing.JFrame;

import org.newtonpolyhedron.ui.NewtonPolyhedronFrame;

public class _NewtonEntry {
	
	public static void main(final String[] args) {
		final NewtonPolyhedronFrame frame = new NewtonPolyhedronFrame();
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setVisible(true);
	}
}
