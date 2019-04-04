/**********************************************************************
 *** *                                                            * ***
 *** *  Copyright (c) 2011 Harmonia Holdings Group LLC            * ***
 *** *                                                            * ***
 *** * Permission is hereby granted to use, modify, copy, and     * ***
 *** * distribute this software and its documentation for any     * ***
 *** * purpose only without profit, provided that the above       * ***
 *** * Copyright Notice appears in all copies and that both the   * ***
 *** * Copyright Notice and this Permission Notice appears in     * ***
 *** * every copy of supporting documentation.  No title to nor   * ***
 *** * ownership of the software is transferred hereby.  The name * ***
 *** * of Harmonia Holdings Group LLC shall not be used in        * ***
 *** * advertising or publicity related to the distribution of    * ***
 *** * the software without specific, written, prior permission.  * ***
 *** * This software is provided as-delivered without expressed   * ***
 *** * or implied warranty.  Harmonia Holdings Group LLC          * ***
 *** * makes no representation about the suitability of this      * ***
 *** * software for any purpose and accepts no responsibility for * ***
 *** * its use.                                                   * ***
 *** *                                                            * ***
 *** ************************************************************** ***
 *** *                                                            * ***
 *** * This program is free software; you can redistribute it     * ***
 *** * and/or modify it under the terms of the GNU General Public * ***
 *** * License as published by the Free Software Foundation;      * ***
 *** * either version 2 of the License, or (at your option) any   * ***
 *** * later version.                                             * ***
 *** *                                                            * ***
 *** * This program is distributed in the hope that it will be    * ***
 *** * useful, but WITHOUT ANY WARRANTY; without even the implied * ***
 *** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ***
 *** * PURPOSE.  See the GNU General Public License for more      * ***
 *** * details.                                                   * ***
 *** *                                                            * ***
 *** * You should have received a copy of the GNU General Public  * ***
 *** * License along with this program; if not, write to the Free * ***
 *** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ***
 *** * Floor, Boston, MA 02110-1301, USA.                         * ***
 *** *                                                            * ***
 *** * For more information: http://www.gnu.org/licenses/gpl.html * ***
 *** *                                                            * ***
 **********************************************************************/

/*
 * SimDTConsole.java
 *
 * Created on Jun 17, 2010, 10:24:16 AM
 */
package org.etexascode.dtsimulator.gui;
import java.awt.Dimension;
import java.awt.KeyboardFocusManager;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.URISyntaxException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.SwingWorker;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.table.DefaultTableModel;
import org.etexascode.api.BSMVerboseProducer;
import org.etexascode.api.MessageModule;
import org.etexascode.api.MessageProducer;
import org.etexascode.api.ModelData;
import org.etexascode.api.eTEXAS;
import org.etexascode.gui.eTEXASRunner;
import org.etexascode.intercom.spi.InterComTx;
import org.etexascode.intercom.spi.InterComTxFactory;
import org.etexascode.interrep.InterRep;
import org.etexascode.interrep.UtilsInterRep;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.Signal;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.j2735.BasicSafetyMessageVerbose;

/**
 * A window which allows a user to step through a simulation using the TEXAS
 * model. 
 *
 * @author bbadillo
 * @author cbrown
 * @author egaebel
 */
public class DTSimConsole extends javax.swing.JFrame implements PropertyChangeListener 
{
	/** Serial ID. */
	private static final long serialVersionUID = -6162426019997520112L;
	
	
	/**
	 * The main object of the TEXAS model Java API
	 */
	private eTEXAS texasSim;
	/**
	 * The intersection representation
	 */
	private InterRep interRep;
	/**
	 * The manager for the simulation. 
	 */
	private SimulatorInterface simManager;
	/**
	 * A possible optionMode value which indicates no option is yet chosen.
	 */
	final static private int SIM_OPTION_UNINITIALIZED = -1;
	/**
	 * A possible optionMode value which indicates normal mode simulation.
	 */
	final static private int SIM_OPTION_NORMAL = 0;
	/**
	 * A value indicating what configuration of DT Simulator to use
	 */
	private int optionMode = SIM_OPTION_UNINITIALIZED;
	/**
	 * A value used to determine the percentage of vehicles that should be equipped
	 * with Intellidrive capabilities. 
	 */
	int randomDSRC = 100;
	/**
	 * String to configure HTTP transmission. 
	 */
	private static final String HTTP_TX_ENABLED = "org.etexascode.http.TX_ENABLED";
	/**
	 * String to configure HTTP reception. 
	 */
	private static final String HTTP_RX_ENABLED = "org.etexascode.http.RX_ENABLED";
	
	
	/** 
	 * Creates new form SimDTConsole
	 */
	public DTSimConsole() {
		initComponents();

		Set<String> configParams = InterComTxFactory.getConfigParams(InterComTx.class);
		if (!configParams.contains(HTTP_TX_ENABLED)) {
			enableCheckBox.setEnabled(false);
		}

		//Add tab out for table to support Section 508 Compliance
		String tabOutId = "tabOut";
		Action tabOut = new AbstractAction() {
			/** Serial ID. */
			private static final long serialVersionUID = 5398477947686574572L;

			@Override
			public void actionPerformed(ActionEvent ae) {
				KeyboardFocusManager.getCurrentKeyboardFocusManager().focusNextComponent();
			}
		};
		vehicleTable.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0), tabOutId);
		vehicleTable.getActionMap().put(tabOutId, tabOut);
	}

	/**
	 * Gets the simulation manager.
	 * @return SimManager.
	 */
	public SimulatorInterface getSimManager() { return simManager; }

	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
	@SuppressWarnings({ "unchecked", "serial" })
	// <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
	private void initComponents() {

		optionDialog = new javax.swing.JDialog();
		startDTSimButton = new javax.swing.JButton();
		dsrcNormalLabel = new javax.swing.JLabel();
		dsrcNormalSpinner = new javax.swing.JSpinner();
		hostLabel = new javax.swing.JLabel();
		hostTextField = new javax.swing.JTextField();
		portTextField = new javax.swing.JTextField();
		portLabel = new javax.swing.JLabel();
		enableCheckBox = new javax.swing.JCheckBox();
		jScrollPane1 = new javax.swing.JScrollPane();
		vehicleTable = new javax.swing.JTable();
		simDTButton = new javax.swing.JButton();
		dtNumLabel = new javax.swing.JLabel();
		dtNumSpinner = new javax.swing.JSpinner();
		timeLabel = new javax.swing.JLabel();
		timeTextField = new javax.swing.JTextField();
		holdSignalSpinner = new javax.swing.JSpinner();
		holdSignalButton = new javax.swing.JButton();
		changeSignalButton = new javax.swing.JButton();
		finSimButton = new javax.swing.JButton();
		progressBar = new javax.swing.JProgressBar();

		optionDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
		optionDialog.setTitle("Simulation Configuration");
		optionDialog.setMinimumSize(new java.awt.Dimension(350, 329));
		optionDialog.setModal(true);
		optionDialog.setResizable(false);
		Dimension scrn = getToolkit().getScreenSize();
		optionDialog.setLocation(((scrn.width / 2) - (optionDialog.getWidth()/2)),
				((scrn.height / 2) - (optionDialog.getHeight()/2)));
		optionDialog.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosed(java.awt.event.WindowEvent evt) {
				optionDialogWindowClosed(evt);
			}
		});

		startDTSimButton.setMnemonic('s');
		startDTSimButton.setText("Start Simulation");
		startDTSimButton.setToolTipText("Start Simulation");
		startDTSimButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				startDTSimButtonActionPerformed(evt);
			}
		});

		dsrcNormalLabel.setText("Percentage of DSRC vehicles:");

		dsrcNormalSpinner.setModel(new javax.swing.SpinnerNumberModel(100, 0, 100, 1));
		dsrcNormalSpinner.setToolTipText("Perentage of DSRC vehicles");
		dsrcNormalSpinner.setMinimumSize(new java.awt.Dimension(80, 20));
		dsrcNormalSpinner.setPreferredSize(new java.awt.Dimension(80, 20));

		hostLabel.setText("Host:");
		hostLabel.setEnabled(false);

		hostTextField.setText("http://localhost");
		hostTextField.setEnabled(false);

		portTextField.setText("8888");
		portTextField.setEnabled(false);

		portLabel.setText("Port:");
		portLabel.setEnabled(false);

		enableCheckBox.setText("Enable Web Broadcast");
		enableCheckBox.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				enableCheckBoxActionPerformed(evt);
			}
		});

		javax.swing.GroupLayout optionDialogLayout = new javax.swing.GroupLayout(optionDialog.getContentPane());
		optionDialog.getContentPane().setLayout(optionDialogLayout);
		optionDialogLayout.setHorizontalGroup(
				optionDialogLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(optionDialogLayout.createSequentialGroup()
						.addContainerGap()
						.addGroup(optionDialogLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
								.addGroup(optionDialogLayout.createSequentialGroup()
										.addGroup(optionDialogLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
												.addGroup(javax.swing.GroupLayout.Alignment.TRAILING, optionDialogLayout.createSequentialGroup()
														.addComponent(dsrcNormalLabel)
														.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 10, Short.MAX_VALUE)
														.addComponent(dsrcNormalSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
														.addComponent(startDTSimButton, javax.swing.GroupLayout.Alignment.TRAILING))
														.addContainerGap())
														.addGroup(optionDialogLayout.createSequentialGroup()
																.addGroup(optionDialogLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
																		.addComponent(enableCheckBox)
																		.addGroup(optionDialogLayout.createSequentialGroup()
																				.addGroup(optionDialogLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
																						.addComponent(hostLabel)
																						.addComponent(portLabel))
																						.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
																						.addGroup(optionDialogLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
																								.addComponent(portTextField, javax.swing.GroupLayout.DEFAULT_SIZE, 199, Short.MAX_VALUE)
																								.addComponent(hostTextField, javax.swing.GroupLayout.DEFAULT_SIZE, 199, Short.MAX_VALUE))))
																								.addGap(14, 14, 14))))
				);
		optionDialogLayout.setVerticalGroup(
				optionDialogLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(javax.swing.GroupLayout.Alignment.TRAILING, optionDialogLayout.createSequentialGroup()
						.addContainerGap()
						.addComponent(enableCheckBox)
						.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
						.addGroup(optionDialogLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
								.addComponent(hostLabel)
								.addComponent(hostTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
								.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
								.addGroup(optionDialogLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
										.addComponent(portTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addComponent(portLabel))
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 18, Short.MAX_VALUE)
										.addGroup(optionDialogLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
												.addComponent(dsrcNormalSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
												.addComponent(dsrcNormalLabel))
												.addGap(18, 18, 18)
												.addComponent(startDTSimButton)
												.addContainerGap())
				);

		dsrcNormalSpinner.getAccessibleContext().setAccessibleDescription("Percentage of DSRC vehicles");

		setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
		setTitle("DT Simulator - eTEXAS V7.0");
		addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosed(java.awt.event.WindowEvent evt) {
				formWindowClosed(evt);
			}
			public void windowOpened(java.awt.event.WindowEvent evt) {
				formWindowOpened(evt);
			}
		});

		vehicleTable.setModel(new javax.swing.table.DefaultTableModel(
				new Object [][] {

				},
				new String [] {
						"Vehicle ID", "Vehicle Type", "Position", "Speed", "Lane ID", "Distance to Intersection", "Time to Intersection", "Signal State", "Time to Signal Change"
				}
				) {
			Class[] types = new Class [] {
					java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.String.class
			};
			boolean[] canEdit = new boolean [] {
					false, false, false, false, false, false, false, false, false
			};

			public Class getColumnClass(int columnIndex) {
				return types [columnIndex];
			}

			public boolean isCellEditable(int rowIndex, int columnIndex) {
				return canEdit [columnIndex];
			}
		});
		vehicleTable.setToolTipText("Data Table");
		vehicleTable.setNextFocusableComponent(dtNumSpinner);
		vehicleTable.getTableHeader().setReorderingAllowed(false);
		jScrollPane1.setViewportView(vehicleTable);
		vehicleTable.getColumnModel().getColumn(0).setPreferredWidth(20);
		vehicleTable.getColumnModel().getColumn(1).setPreferredWidth(40);
		vehicleTable.getColumnModel().getColumn(2).setPreferredWidth(110);
		vehicleTable.getColumnModel().getColumn(3).setPreferredWidth(50);
		vehicleTable.getColumnModel().getColumn(4).setPreferredWidth(20);
		vehicleTable.getColumnModel().getColumn(5).setPreferredWidth(90);

		simDTButton.setMnemonic('s');
		simDTButton.setText("Simulate DT");
		simDTButton.setToolTipText("Simulate DT");
		simDTButton.setNextFocusableComponent(finSimButton);
		simDTButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				simDTButtonActionPerformed(evt);
			}
		});

		dtNumLabel.setText("Number of DTs:");
		dtNumLabel.setToolTipText("Number of DTs:");

		dtNumSpinner.setModel(new javax.swing.SpinnerNumberModel(Integer.valueOf(1), Integer.valueOf(1), null, Integer.valueOf(1)));
		dtNumSpinner.setToolTipText("Choose Number of DTs");
		dtNumSpinner.setNextFocusableComponent(simDTButton);

		timeLabel.setText("Current Time:");
		timeLabel.setToolTipText("Current Time");

		timeTextField.setEditable(false);
		timeTextField.setHorizontalAlignment(javax.swing.JTextField.TRAILING);
		timeTextField.setText("0.0");
		timeTextField.setToolTipText("Time");

		holdSignalSpinner.setModel(new javax.swing.SpinnerNumberModel(Double.valueOf(0.0d), Double.valueOf(0.0d), null, Double.valueOf(0.1d)));
		holdSignalSpinner.setNextFocusableComponent(holdSignalButton);

		holdSignalButton.setMnemonic('h');
		holdSignalButton.setText("Hold Signal");
		holdSignalButton.setToolTipText("Hold Signal");
		holdSignalButton.setNextFocusableComponent(changeSignalButton);
		holdSignalButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				holdSignalButtonActionPerformed(evt);
			}
		});

		changeSignalButton.setMnemonic('c');
		changeSignalButton.setText("Change Signal");
		changeSignalButton.setToolTipText("Change Signal");
		changeSignalButton.setNextFocusableComponent(vehicleTable);
		changeSignalButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				changeSignalButtonActionPerformed(evt);
			}
		});

		finSimButton.setMnemonic('f');
		finSimButton.setText("Finish Simulation");
		finSimButton.setToolTipText("Finish Simulation");
		finSimButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				finSimButtonActionPerformed(evt);
			}
		});

		progressBar.setToolTipText("Progress");

		javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
		getContentPane().setLayout(layout);
		layout.setHorizontalGroup(
				layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(layout.createSequentialGroup()
						.addContainerGap()
						.addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
								.addComponent(jScrollPane1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 1004, Short.MAX_VALUE)
								.addComponent(progressBar, javax.swing.GroupLayout.DEFAULT_SIZE, 1004, Short.MAX_VALUE)
								.addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
										.addComponent(holdSignalSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, 56, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
										.addComponent(holdSignalButton)
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
										.addComponent(changeSignalButton)
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 586, Short.MAX_VALUE)
										.addComponent(timeLabel)
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
										.addComponent(timeTextField, javax.swing.GroupLayout.PREFERRED_SIZE, 94, javax.swing.GroupLayout.PREFERRED_SIZE))
										.addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
												.addComponent(dtNumLabel)
												.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
												.addComponent(dtNumSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE)
												.addGap(18, 18, 18)
												.addComponent(simDTButton)
												.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
												.addComponent(finSimButton)))
												.addContainerGap())
				);
		layout.setVerticalGroup(
				layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(layout.createSequentialGroup()
						.addContainerGap()
						.addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
								.addComponent(timeTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addComponent(timeLabel)
								.addComponent(holdSignalSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addComponent(holdSignalButton)
								.addComponent(changeSignalButton))
								.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
								.addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 302, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addGap(18, 18, 18)
								.addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
										.addComponent(finSimButton)
										.addComponent(simDTButton)
										.addComponent(dtNumLabel)
										.addComponent(dtNumSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
										.addComponent(progressBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addContainerGap())
				);

		pack();
	}// </editor-fold>//GEN-END:initComponents

	private void simDTButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_simDTButtonActionPerformed
		// Get the number of time steps to advance in the simulation
		int dtNum = (Integer) dtNumSpinner.getValue();

		SimLoopTask simLoopTask = new SimLoopTask(dtNum);
		simLoopTask.addPropertyChangeListener(this);
		simLoopTask.execute();

	}//GEN-LAST:event_simDTButtonActionPerformed

	private void holdSignalButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_holdSignalButtonActionPerformed
		Double value = (Double) holdSignalSpinner.getValue();

		texasSim.holdSignal(value);
	}//GEN-LAST:event_holdSignalButtonActionPerformed

	private void changeSignalButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_changeSignalButtonActionPerformed
		texasSim.changeSignal(0.0);
	}//GEN-LAST:event_changeSignalButtonActionPerformed

	private void finSimButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_finSimButtonActionPerformed
		//Simulate the total time steps required to finish the simulator
		//and add one to ensure finishing the simulation.
		ModelData modelData = texasSim.getModelData();
		if (modelData.getDTSize() != 0) {
			SimLoopTask simLoopTask = new SimLoopTask((int) (modelData.getMaxDT() - Math.round(texasSim.getCurrentTime() / modelData.getDTSize())) + 2);
			simLoopTask.addPropertyChangeListener(this);
			simLoopTask.execute();
		} else {

			JOptionPane.showMessageDialog(rootPane, "Please check your project setup; the current setup resulted in division by 0.");
			throw new IllegalStateException("DTSize was 0. Division by 0 is not allowed, please check your project setup to ensure it is valid.");
		}
	}//GEN-LAST:event_finSimButtonActionPerformed

	private void formWindowClosed(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_formWindowClosed
		System.gc();
	}//GEN-LAST:event_formWindowClosed

	private void formWindowOpened(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_formWindowOpened
		// Align above the center vertical line and on center horizonal line.
		Toolkit kit = Toolkit.getDefaultToolkit();
		Dimension screenSize = kit.getScreenSize();
		int screenWidth = (int) screenSize.getWidth();     // getting Screen width
		int screenHeight = (int) screenSize.getHeight();  // getting Screen height
		this.setLocation(((screenWidth / 2) - (this.getWidth() / 2)),
				Math.max((screenHeight / 2) - (this.getHeight()), 0));
		optionDialog.setAlwaysOnTop(true);
		optionDialog.setVisible(true);
	}//GEN-LAST:event_formWindowOpened

	public void startDTSimButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_startDTSimButtonActionPerformed
		try {
			//Set the simulation mode to run with a DCS algorithm
			optionMode = SIM_OPTION_NORMAL;
			randomDSRC = (Integer) dsrcNormalSpinner.getValue();

			Map<String, String> eTEXASConfig = new HashMap<String, String>();
			Map<String, String> interRepConfig = new HashMap<String, String>();
			interRepConfig.put(HTTP_RX_ENABLED, "false");

			//Set the eTEXAS model to produce web messages or not
			if (enableCheckBox.isSelected()) {
				String hostText = hostTextField.getText();
				String portText = portTextField.getText();
				// Important - keep this parseInt call in to validate integer input. 
				int portNum = Integer.parseInt(portText);
				//Initialize the simulation
				eTEXASConfig.put("org.etexascode.http.HOST", hostText);
				eTEXASConfig.put("org.etexascode.http.PORT", portText);

			} else {
				//Initialize the simulation
				eTEXASConfig.put(HTTP_TX_ENABLED, "false");
			}

			try {
				texasSim = new eTEXAS();
				texasSim.setup(eTEXASConfig);
				simManager = new DTSimController(texasSim);
				interRep = new InterRep(simManager);
			} catch (ClassNotFoundException ex) {
				LoggerFactory.getLogger(DTSimConsole.class.getName()).log(Level.SEVERE, null, ex);
			}
			//Set the random DSRC assignment to be predictable.
			MessageModule module = texasSim.getModule(BasicSafetyMessageVerbose.class.getName());
			if (module != null) {
				MessageProducer messageProducer = module.getMessageProducer();

				if (messageProducer != null && messageProducer instanceof BSMVerboseProducer) {
					BSMVerboseProducer bsmVerboseProducer = (BSMVerboseProducer) messageProducer;
					bsmVerboseProducer.resetRandomizer(1);
					bsmVerboseProducer.setDsrcPercentage(randomDSRC);
				}
			}
			optionDialog.dispose();
		} catch (NumberFormatException e) {
			// TODO: bbadillo - add an input validation dialog. 
		}
	}//GEN-LAST:event_startDTSimButtonActionPerformed

	private void optionDialogWindowClosed(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_optionDialogWindowClosed
		if (optionMode == SIM_OPTION_UNINITIALIZED) {
			this.dispose();
		}
	}//GEN-LAST:event_optionDialogWindowClosed

	private void enableCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_enableCheckBoxActionPerformed
		boolean enable = enableCheckBox.isSelected();
		hostLabel.setEnabled(enable);
		hostTextField.setEnabled(enable);
		portLabel.setEnabled(enable);
		portTextField.setEnabled(enable);
	}//GEN-LAST:event_enableCheckBoxActionPerformed

	/**
	 * A helper method to iterate through all of the vehicles currently logged into
	 * the TEXAS model and provide each vehicles information on separate rows in the
	 * table.
	 * @throws RemoteException 
	 */
	private void populateVehicleTable() throws RemoteException {

		DefaultTableModel tableModel = (DefaultTableModel) vehicleTable.getModel();

		// Iterate through all the vehicle ids from vehicle manager and reset the table data
		Set<Integer> theVehicleIds = interRep.getVehicleManager().getAllVehicleIds();

		//Must sort the ids so they can be nicely presented in DT Simulator display
		List<Integer> vehicleIds = new ArrayList(theVehicleIds);
		Collections.sort(vehicleIds);

		Iterator<Integer> iterator = vehicleIds.iterator();
		int i = 0;
		while (iterator.hasNext()) {

			Integer currVehicleId = iterator.next();
			Vehicle currVehicle = interRep.getVehicleManager().getVehicle(currVehicleId);

			//Retrieve position and string from vehicle object.
			String posString = String.format("(%1$.2f ft, %2$.2f ft)",
					currVehicle.getX(), currVehicle.getY());

			String speedString = String.format("%1$.2f mph", currVehicle.getSpeed());

			String laneString = "No DSRC";
			String distString = "No DSRC";
			String timeToIntersectionString = "No DSRC";
			String timeString = "No DSRC";
			String stateString = "No DSRC";

			//Get connected vehicle data
			Lane lane = interRep.getIntersectionManager().getLaneById(currVehicle.getLaneID());

			if (lane == null
					|| (lane != null && (Lane.INBOUND.equals(lane.getType())) || Lane.OUTBOUND.equals(lane.getType()))) {

				if (lane != null) {

					//the lane is inbound
					if (Lane.INBOUND.equals(lane.getType())) {

						Signal signal = interRep.getSignalByLaneID(currVehicle.getLaneID());
						Set<Integer> signalIndications = signal.getKeys();
						Iterator<Integer> iter = signalIndications.iterator();
						if (iter.hasNext()) {
							Integer signalIndicationId = iter.next();
							SignalIndication signalIndication = signal.getSignalIndicationById(signalIndicationId);
							Color colorIndication = signalIndication.getColorIndication();
							stateString = colorIndication.toString();
							timeString = String.format("%1$d s", ((Double) signalIndication.getTimeToChange()).intValue());
						}

						laneString = String.format("%1$d", currVehicle.getLaneID());
						distString = String.format("%1$.2f ft", UtilsInterRep.getDistToStopLine(currVehicle, 
								interRep.getIntersectionManager().getLaneById(currVehicle.getLaneID())));
						if (currVehicle.getSpeed() != 0) {
							double timeToIntersection = UtilsInterRep.getDistToStopLine(currVehicle, 
									interRep.getIntersectionManager().getLaneById(currVehicle.getLaneID())) / currVehicle.getSpeed();
							timeToIntersectionString = String.format("%1$.2f s", timeToIntersection);
						} else {
							timeToIntersectionString = "Stopped";
						}
					} //the lane is outbound
					else {

						laneString = String.valueOf(currVehicle.getLaneID());
						distString = "Past Intersection";
						timeToIntersectionString = "Past Intersection";
					}
				}

				//New entry, so add a row.
				if (tableModel.getRowCount() <= i) {
					tableModel.addRow(new Object[]{currVehicle.getVehicleID(), "N/A", posString,
							speedString, laneString, distString, timeToIntersectionString, stateString, timeString});
				} else { // Update an existing entry.
					tableModel.setValueAt(currVehicle.getVehicleID(), i, 0);
					tableModel.setValueAt("N/A", i, 1);
					tableModel.setValueAt(posString, i, 2);
					tableModel.setValueAt(speedString, i, 3);
					tableModel.setValueAt(laneString, i, 4);
					tableModel.setValueAt(distString, i, 5);
					tableModel.setValueAt(timeToIntersectionString, i, 6);
					tableModel.setValueAt(stateString, i, 7);
					tableModel.setValueAt(timeString, i, 8);
				}

				i++;
			}
		}

		while (tableModel.getRowCount() > i) {
			tableModel.removeRow(i);
		}
	}

	/**
	 * Notifies this object of progress updates from the SimLoopTask and updates
	 * the progress bar.
	 *
	 * @param evt The property change. 
	 */
	@Override
	public void propertyChange(PropertyChangeEvent evt) {
		if ("progress".equals(evt.getPropertyName())) {
			int progress = (Integer) evt.getNewValue();
			progressBar.setValue(progress);
		}
	}

	/**
	 * Toggles the simDTButton and the finSimButton to be enabled or not;
	 * depending on the passed value.
	 * 
	 * @param value the value to setEnabled to for the two buttons.
	 */
	private void simButtonToggle(boolean value) {

		simDTButton.setEnabled(value);
		finSimButton.setEnabled(value);
	}

	/**
	 * An inner class used to loop through many DTs in the TEXAS Model at a time
	 * in the background so that progress can be updated and shown. 
	 */
	class SimLoopTask extends SwingWorker<Double, Double> {

		/**
		 * The number of DTs to simulate in this background task.
		 */
		int numDTsToSimulate;

		/**
		 * Constructor to create a DT simulator looping task.
		 *
		 * @param numDTsToSimulate The number of DTs to simulate (Must be greater than or equal to 1)
		 */
		public SimLoopTask(int numDTsToSimulate) {
			if (numDTsToSimulate < 1) {
				throw new IllegalStateException("SimLoopTask: numDTsToSimulate must always be greater than or equal to 1 and is currently " + numDTsToSimulate);
			}
			this.numDTsToSimulate = numDTsToSimulate;

			//if more than 10 dts running
			if (numDTsToSimulate >= 10) {
				//Grey buttons.
				simButtonToggle(false);
			}
		}

		@Override
		protected Double doInBackground() throws Exception {
			for (int i = 0; (i < numDTsToSimulate - 1) && !texasSim.isFinished(); i++) {
				performTimeStep();
				setProgress((int) (((double) i / (double) numDTsToSimulate) * 100));
			}

			//Once the simulation loop is done processing, set progress to complete. 
			setProgress(100);

			return null;
		}

		@Override
		protected void done() {
			super.done();

			// Perform the last time step in this event thread
			performTimeStep();

			interRep.update();

			try { populateVehicleTable(); }
			catch (RemoteException e) {e.printStackTrace(); }

			if (texasSim.isFinished()) {
				simDTButton.setEnabled(false);
				LoggerFactory.getLogger(DTSimConsole.class.getName()).log(Level.INFO, "Finished Simulation...");
			}

			progressBar.setValue(0);

			//Restore buttons.
			simButtonToggle(true);
		}

		/**
		 * Procedure to execute the next DT in the TEXAS Model simulator and
		 * retrieve vehicle data. Optionally a D-CS algorithm is used in this
		 * procedure if it has been initialized. 
		 */
		private void performTimeStep() {
			//Perform a DT in the TEXAS Model simulation
			try { simManager.getStepData((long) texasSim.getCurrentTime()); }
			catch (RemoteException e) { e.printStackTrace(); }

			//Get the current time (in seconds) in the simulation
			double currTime = texasSim.getCurrentTime();
			// Prints the double as a strings
			timeTextField.setText("" + currTime);
		}
	}

	/**
	 * The main class which starts the simulator.
	 *
	 * @param args the command line arguments
	 */
	public static void main(String args[]) throws ClassNotFoundException, InstantiationException, UnsupportedLookAndFeelException, IllegalAccessException {
		UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());

		if (args.length == 2) {
			if (args[0].equals(eTEXASRunner.class.getSimpleName())) {
				// Execute the eTEXASRunner to change the current working directory
				final String libraryPath = args[1];

				java.awt.EventQueue.invokeLater(
						new Runnable() {

							@Override
							public void run() {
								try {
									File jarFile = new File(DTSimConsole.class.getProtectionDomain().getCodeSource().getLocation().toURI());
									String jarFilename = jarFile.getName();
									LoggerFactory.getLogger(DTSimConsole.class.getName()).log(Level.INFO, "Jar file location: {0}", jarFilename);
									eTEXASRunner runner = new eTEXASRunner(jarFilename, libraryPath);
									runner.setVisible(true);
								} catch (URISyntaxException ex) {
									LoggerFactory.getLogger(DTSimConsole.class.getName()).log(Level.SEVERE, null, ex);
								}
							}
						});
			}
		} else {
			// Execute the simulator; finding simpro.par in the current working directory
			java.awt.EventQueue.invokeLater(new Runnable() {

				@Override
				public void run() {
					DTSimConsole console = new DTSimConsole();
					console.setVisible(true);
				}
			});
		}
	}
	// Variables declaration - do not modify//GEN-BEGIN:variables
	private javax.swing.JButton changeSignalButton;
	private javax.swing.JLabel dsrcNormalLabel;
	private javax.swing.JSpinner dsrcNormalSpinner;
	private javax.swing.JLabel dtNumLabel;
	private javax.swing.JSpinner dtNumSpinner;
	private javax.swing.JCheckBox enableCheckBox;
	private javax.swing.JButton finSimButton;
	private javax.swing.JButton holdSignalButton;
	private javax.swing.JSpinner holdSignalSpinner;
	private javax.swing.JLabel hostLabel;
	private javax.swing.JTextField hostTextField;
	private javax.swing.JScrollPane jScrollPane1;
	private javax.swing.JDialog optionDialog;
	private javax.swing.JLabel portLabel;
	private javax.swing.JTextField portTextField;
	private javax.swing.JProgressBar progressBar;
	private javax.swing.JButton simDTButton;
	private javax.swing.JButton startDTSimButton;
	private javax.swing.JLabel timeLabel;
	private javax.swing.JTextField timeTextField;
	private javax.swing.JTable vehicleTable;
	// End of variables declaration//GEN-END:variables
}
