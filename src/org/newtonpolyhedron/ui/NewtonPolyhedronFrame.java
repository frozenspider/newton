package org.newtonpolyhedron.ui;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;

import javax.swing.*;

import org.newtonpolyhedron.NewtonLogic;
import org.newtonpolyhedron.WorkingMode;
import org.newtonpolyhedron.ex.UnknownModeException;
import org.newtonpolyhedron.ex.WrongFormatException;
import org.newtonpolyhedron.utils.Version;

public class NewtonPolyhedronFrame extends JFrame {
	
	private static final long	serialVersionUID	= 2323255354970722443L;
	private final NewtonLogic	logic;
	private final JTextField	tfPathToInput;
	private final JButton		btnStart;
	private final JCheckBox		chckbxIllustrate;
	private final JComboBox		cbMode;
	private final JTextArea		txtrOutput;
	private final PrintWriter	printWriter;
	private boolean				started				= false;
	
	public NewtonPolyhedronFrame() {
		this.printWriter = new PrintWriter(new NewtonTextAreaOutput());
		this.logic = new NewtonLogic();
		setTitle("Newton v." + Version.VERSION);
		setBounds(100, 100, 530, 400);
		{
			final JPanel panelTop = new JPanel();
			getContentPane().add(panelTop, BorderLayout.NORTH);
			panelTop.setLayout(new BorderLayout(0, 0));
			{
				final JPanel panelHeader = new JPanel();
				final FlowLayout flowLayout = (FlowLayout) panelHeader.getLayout();
				flowLayout.setAlignment(FlowLayout.LEFT);
				panelTop.add(panelHeader, BorderLayout.NORTH);
				{
					btnStart = new JButton("Start");
					btnStart.addActionListener(new StartClickedActionListener());
					panelHeader.add(btnStart);
				}
				{
					chckbxIllustrate = new JCheckBox("Illustrate if possible");
					panelHeader.add(chckbxIllustrate);
				}
			}
			{
				final JPanel panelMode = new JPanel();
				final FlowLayout flowLayout = (FlowLayout) panelMode.getLayout();
				flowLayout.setAlignment(FlowLayout.LEFT);
				panelTop.add(panelMode, BorderLayout.CENTER);
				{
					cbMode = new JComboBox();
					cbMode.setModel(new DefaultComboBoxModel(WorkingMode.values()));
					panelMode.add(cbMode);
				}
			}
			{
				final JPanel panelBrowse = new JPanel();
				panelTop.add(panelBrowse, BorderLayout.SOUTH);
				panelBrowse.setLayout(new BorderLayout(0, 0));
				{
					final JButton btnBrowse = new JButton("Browse");
					btnBrowse.addActionListener(new BrowseActionListener());
					panelBrowse.add(btnBrowse, BorderLayout.WEST);
				}
				{
					tfPathToInput = new JTextField();
					panelBrowse.add(tfPathToInput, BorderLayout.CENTER);
					tfPathToInput.setColumns(10);
				}
			}
		}
		{
			final JPanel panelCenter = new JPanel();
			getContentPane().add(panelCenter, BorderLayout.CENTER);
			panelCenter.setLayout(new BorderLayout(0, 0));
			{
				final JScrollPane scrollPane = new JScrollPane();
				panelCenter.add(scrollPane, BorderLayout.CENTER);
				{
					txtrOutput = new JTextArea();
					txtrOutput.setFont(new Font("Courier New", Font.PLAIN, 12));
					txtrOutput.setText("Press Browse button to browse for input files and press Start to begin.\n");
					scrollPane.setViewportView(txtrOutput);
				}
			}
		}
	}
	
	private final class BrowseActionListener implements ActionListener {
		
		@Override
		public void actionPerformed(final ActionEvent e) {
			final JFileChooser chooser = new JFileChooser();
			chooser.setCurrentDirectory(new File(tfPathToInput.getText().length() == 0 ? "."
					: tfPathToInput.getText()));
			final int returnVal = chooser.showOpenDialog(NewtonPolyhedronFrame.this);
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				tfPathToInput.setText(chooser.getSelectedFile().getAbsolutePath());
			}
		}
	}
	
	private final class StartClickedActionListener implements ActionListener {
		
		@Override
		public void actionPerformed(final ActionEvent e) {
			try {
				if (!started) {
					final String path = tfPathToInput.getText();
					final int selectedIdx = cbMode.getSelectedIndex();
					final boolean illustrate = chckbxIllustrate.isSelected();
					final WorkingMode mode = (WorkingMode) cbMode.getItemAt(selectedIdx);
					if (selectedIdx != -1) {
						logic.start(path, mode, illustrate, printWriter);
						btnStart.setText("Stop");
						started = true;
					}
				} else {
					logic.stop();
					btnStart.setText("Start");
					started = false;
				}
			} catch(final FileNotFoundException ex) {
				printWriter.println("File not found");
			} catch(final WrongFormatException ex) {
				printWriter.println("Illegal file format, see instruction");
			} catch(final UnknownModeException ex) {
				printWriter.println("Unknown mode selected: " + ex.getLocalizedMessage());
			} catch(final InterruptedException ex) {
				printWriter.println("Interrupted");
			} catch(final RuntimeException ex) {
				ex.printStackTrace(printWriter);
			} catch(final Exception ex) {
				ex.printStackTrace(printWriter);
			} catch(final Error er) {
				er.printStackTrace(printWriter);
			}
		}
	}
	
	public class NewtonTextAreaOutput extends Writer {
		
		public NewtonTextAreaOutput() {}
		
		@Override
		public void write(final char[] cbuf, final int off, final int len) throws IOException {
			writeImpl(new String(cbuf, off, len));
		}
		
		@Override
		public void flush() throws IOException {}
		
		@Override
		public void close() throws IOException {}
		
		/**
		 * Main printing method.
		 * 
		 * @param text
		 *            what to print
		 */
		private void writeImpl(final String text) {
			System.out.print(text);
			txtrOutput.append(text);
			txtrOutput.setCaretPosition(txtrOutput.getText().length());
		}
	}
}
