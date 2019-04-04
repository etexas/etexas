/**********************************************************************
 *** *                                                            * ***
 *** *  Copyright (c) 2012 Harmonia Holdings Group LLC            * ***
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
package org.etexascode.dcs.gui;

import org.etexascode.dcs.DCSAlgorithm;
import org.etexascode.dcs.bindings.SignalControllerForETEXAS;
import org.etexascode.gui.eTEXASRunner;
import org.etexascode.api.eTEXAS;
import java.awt.Dimension;
import java.awt.KeyboardFocusManager;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import org.etexascode.api.*;
import org.etexascode.intercom.spi.InterComRxFactory;
import org.etexascode.intercom.spi.MAPReceiver;
import org.etexascode.intercom.spi.SPATReceiver;
import org.etexascode.interrep.InterRep;
import org.etexascode.interrep.UtilsInterRep;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.Signal;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.j2735.BasicSafetyMessage;

/**
 * A window which allows a user to step through a simulation using the TEXAS
 * model. 
 *
 * @author bbadillo
 * @author cbrown
 */
public class DCSSimConsole extends javax.swing.JFrame implements PropertyChangeListener {

    /**
     * The main object of the TEXAS model Java API
     */
    private eTEXAS texasSim;
    /**
     * InterRep for this model that represents the intersection
     */
    private InterRep interRep;
    /**
     * An object used to recieve SAE J2735 MapData messages as they become available.
     */
    MAPReceiver mapDataReceiver = InterComRxFactory.getCommunicator(MAPReceiver.class, null);
    /**
     * An object used to recieve SAE J2735 SPAT messages as they become available.
     */
    SPATReceiver spatReceiver = InterComRxFactory.getCommunicator(SPATReceiver.class, null);
    /**
     * A collection of the current vehicles in the simulation. 
     */
    Collection<Integer> currentVehicles = null;
    /**
     * A value used to determine the percentage of vehicles that should be equipped
     * with ConnectedVehicle capabilities. 
     */
    int randomDSRC = 100;
    /**
     * The main object of D-CS. 
     */
    private DCSAlgorithm algorithm = null;
    /**
     * The GUI used to display D-CS information. 
     */
    private DCSSimulationFrame dcsSimulationFrame = null;
    /**
     * A possible optionMode value which indicates no option is yet chosen. 
     */
    final static private int SIM_OPTION_UNINITIALIZED = -1;
    /**
     * A possible optionMode value which indicates simulation with D-CS.
     */
    final static private int SIM_OPTION_DCS = 1;
    /**
     * A possible optionMode value which indicates simulation with D-CS/Intellidrive.
     */
    final static private int SIM_OPTION_DCS_INTELLIDRIVE = 2;
    /**
     * A value indicating what configuration of DT Simulator to use
     */
    private int optionMode = SIM_OPTION_UNINITIALIZED;
    /**
     * String to configure HTTP transmission. 
     */
    private static final String HTTP_TX_ENABLED = "org.etexascode.http.TX_ENABLED";
    /**
     * String to configure HTTP reception. 
     */
    private static final String HTTP_RX_ENABLED = "org.etexascode.http.RX_ENABLED";
    /**
     * Static logger
     */
    private static final Logger LOGGER=LoggerFactory.getLogger(DCSSimConsole.class);
    /** 
     * Creates new form SimDTConsole
     */
    public DCSSimConsole() throws ClassNotFoundException {
        initComponents();

        //Add tab out for table to support Section 508 Compliance
        String tabOutId = "tabOut";
        Action tabOut = new AbstractAction() {

            @Override
            public void actionPerformed(ActionEvent ae) {
                KeyboardFocusManager.getCurrentKeyboardFocusManager().focusNextComponent();
            }
        };
        vehicleTable.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0), tabOutId);
        vehicleTable.getActionMap().put(tabOutId, tabOut);

        //Initialize the simulation
        texasSim = new eTEXAS();
        interRep = new InterRep(new DCSSimController(texasSim));

    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        optionDialog = new javax.swing.JDialog();
        optionTabbedPane = new javax.swing.JTabbedPane();
        dcsOptionPanel = new javax.swing.JPanel();
        stageOneOptionSpinner = new javax.swing.JSpinner();
        stageTwoOptionSpinner = new javax.swing.JSpinner();
        distanceOptionSpinner = new javax.swing.JSpinner();
        startDCSSimButton = new javax.swing.JButton();
        distanceOptionLabel = new javax.swing.JLabel();
        stageOneOptionLabel = new javax.swing.JLabel();
        stageTwoOptionLabel = new javax.swing.JLabel();
        leg1CheckBox = new javax.swing.JCheckBox();
        leg2CheckBox = new javax.swing.JCheckBox();
        leg3CheckBox = new javax.swing.JCheckBox();
        leg4CheckBox = new javax.swing.JCheckBox();
        dsrcDCSSpinner = new javax.swing.JSpinner();
        dsrcDCSLabel = new javax.swing.JLabel();
        legCheckBoxLabel = new javax.swing.JLabel();
        hostTextField = new javax.swing.JTextField();
        portTextField = new javax.swing.JTextField();
        portLabel = new javax.swing.JLabel();
        enableCheckBox = new javax.swing.JCheckBox();
        hostLabel = new javax.swing.JLabel();
        intellidriveOptionPanel = new javax.swing.JPanel();
        intellidriveStageOneOptionSpinner = new javax.swing.JSpinner();
        intellidriveStageTwoOptionSpinner = new javax.swing.JSpinner();
        intellidriveStartDCSSimButton = new javax.swing.JButton();
        intellidriveStageOneOptionLabel = new javax.swing.JLabel();
        intellidriveStageTwoOptionLabel = new javax.swing.JLabel();
        intellidriveLeg1CheckBox = new javax.swing.JCheckBox();
        intellidriveLeg2CheckBox = new javax.swing.JCheckBox();
        intellidriveLeg3CheckBox = new javax.swing.JCheckBox();
        intellidriveLeg4CheckBox = new javax.swing.JCheckBox();
        dsrcIntellidriveSpinner = new javax.swing.JSpinner();
        dsrcIntellidriveLabel = new javax.swing.JLabel();
        legIntellidriveLabel = new javax.swing.JLabel();
        hostTextField1 = new javax.swing.JTextField();
        portTextField1 = new javax.swing.JTextField();
        portLabel1 = new javax.swing.JLabel();
        enableCheckBox1 = new javax.swing.JCheckBox();
        hostLabel1 = new javax.swing.JLabel();
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
        optionDialog.setMinimumSize(new java.awt.Dimension(350, 390));
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

        optionTabbedPane.setToolTipText("Choose Method");
        optionTabbedPane.setMinimumSize(new java.awt.Dimension(280, 320));
        optionTabbedPane.setPreferredSize(new java.awt.Dimension(280, 320));

        dcsOptionPanel.setToolTipText("D-CS Algorithm");
        dcsOptionPanel.setPreferredSize(new java.awt.Dimension(239, 123));

        stageOneOptionSpinner.setModel(new javax.swing.SpinnerNumberModel(Double.valueOf(10.0d), Double.valueOf(0.0d), null, Double.valueOf(0.1d)));
        stageOneOptionSpinner.setToolTipText("Stage 1 Timeout (in secionds)");
        stageOneOptionSpinner.setNextFocusableComponent(stageTwoOptionLabel);
        stageOneOptionSpinner.setPreferredSize(new java.awt.Dimension(80, 20));

        stageTwoOptionSpinner.setModel(new javax.swing.SpinnerNumberModel(Double.valueOf(5.0d), Double.valueOf(0.0d), null, Double.valueOf(0.1d)));
        stageTwoOptionSpinner.setToolTipText("Stage 2 Timeout (in seconds)");
        stageTwoOptionSpinner.setNextFocusableComponent(leg1CheckBox);
        stageTwoOptionSpinner.setPreferredSize(new java.awt.Dimension(80, 20));

        distanceOptionSpinner.setModel(new javax.swing.SpinnerNumberModel(Integer.valueOf(1000), Integer.valueOf(0), null, Integer.valueOf(1)));
        distanceOptionSpinner.setToolTipText("Detector Distance (in feet)");
        distanceOptionSpinner.setNextFocusableComponent(stageOneOptionSpinner);
        distanceOptionSpinner.setPreferredSize(new java.awt.Dimension(80, 20));

        startDCSSimButton.setMnemonic('s');
        startDCSSimButton.setText("Start Simulation");
        startDCSSimButton.setToolTipText("Start Simulation");
        startDCSSimButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                startDCSSimButtonActionPerformed(evt);
            }
        });

        distanceOptionLabel.setText("Detector Distance (in feet):");

        stageOneOptionLabel.setText("Stage 1 Timeout (in seconds):");

        stageTwoOptionLabel.setText("Stage 2 Timeout (in seconds):");

        leg1CheckBox.setText("Leg 1");
        leg1CheckBox.setToolTipText("Leg 1");
        leg1CheckBox.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        leg1CheckBox.setHorizontalTextPosition(javax.swing.SwingConstants.LEADING);

        leg2CheckBox.setText("Leg 2");
        leg2CheckBox.setToolTipText("Leg 2");
        leg2CheckBox.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        leg2CheckBox.setHorizontalTextPosition(javax.swing.SwingConstants.LEADING);

        leg3CheckBox.setText("Leg 3");
        leg3CheckBox.setToolTipText("Leg 3");
        leg3CheckBox.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        leg3CheckBox.setHorizontalTextPosition(javax.swing.SwingConstants.LEADING);

        leg4CheckBox.setText("Leg 4");
        leg4CheckBox.setToolTipText("Leg 4");
        leg4CheckBox.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        leg4CheckBox.setHorizontalTextPosition(javax.swing.SwingConstants.LEADING);

        dsrcDCSSpinner.setModel(new javax.swing.SpinnerNumberModel(100, 0, 100, 1));
        dsrcDCSSpinner.setToolTipText("Percentage of DSRC vehicles");
        dsrcDCSSpinner.setMinimumSize(new java.awt.Dimension(80, 20));
        dsrcDCSSpinner.setPreferredSize(new java.awt.Dimension(80, 20));

        dsrcDCSLabel.setText("Percentage of DSRC vehicles:");

        legCheckBoxLabel.setText("Apply D-CS to:");

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

        hostLabel.setText("Host:");
        hostLabel.setEnabled(false);

        javax.swing.GroupLayout dcsOptionPanelLayout = new javax.swing.GroupLayout(dcsOptionPanel);
        dcsOptionPanel.setLayout(dcsOptionPanelLayout);
        dcsOptionPanelLayout.setHorizontalGroup(
            dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(dcsOptionPanelLayout.createSequentialGroup()
                .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(dcsOptionPanelLayout.createSequentialGroup()
                        .addContainerGap(84, Short.MAX_VALUE)
                        .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, dcsOptionPanelLayout.createSequentialGroup()
                                .addComponent(dsrcDCSLabel)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(dsrcDCSSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, dcsOptionPanelLayout.createSequentialGroup()
                                .addComponent(distanceOptionLabel)
                                .addGap(4, 4, 4)
                                .addComponent(distanceOptionSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, dcsOptionPanelLayout.createSequentialGroup()
                                .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, dcsOptionPanelLayout.createSequentialGroup()
                                        .addComponent(stageOneOptionLabel)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED))
                                    .addGroup(dcsOptionPanelLayout.createSequentialGroup()
                                        .addComponent(stageTwoOptionLabel)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED))
                                    .addGroup(dcsOptionPanelLayout.createSequentialGroup()
                                        .addComponent(legCheckBoxLabel)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)))
                                .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addComponent(stageTwoOptionSpinner, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(stageOneOptionSpinner, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, dcsOptionPanelLayout.createSequentialGroup()
                                .addComponent(leg1CheckBox)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(leg2CheckBox)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(leg3CheckBox)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(leg4CheckBox))
                            .addComponent(startDCSSimButton, javax.swing.GroupLayout.Alignment.TRAILING)))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, dcsOptionPanelLayout.createSequentialGroup()
                        .addGap(82, 82, 82)
                        .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(enableCheckBox)
                            .addGroup(dcsOptionPanelLayout.createSequentialGroup()
                                .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(hostLabel)
                                    .addComponent(portLabel))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(portTextField, javax.swing.GroupLayout.DEFAULT_SIZE, 199, Short.MAX_VALUE)
                                    .addComponent(hostTextField, javax.swing.GroupLayout.DEFAULT_SIZE, 199, Short.MAX_VALUE))))))
                .addContainerGap())
        );
        dcsOptionPanelLayout.setVerticalGroup(
            dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(dcsOptionPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(dsrcDCSSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(dsrcDCSLabel))
                .addGap(18, 18, 18)
                .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(stageOneOptionSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(stageOneOptionLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(stageTwoOptionSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(stageTwoOptionLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(distanceOptionSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(distanceOptionLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(enableCheckBox)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(hostLabel)
                    .addComponent(hostTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(portTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(portLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 16, Short.MAX_VALUE)
                .addComponent(legCheckBoxLabel)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(dcsOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(leg4CheckBox)
                    .addComponent(leg3CheckBox)
                    .addComponent(leg2CheckBox)
                    .addComponent(leg1CheckBox))
                .addGap(18, 18, 18)
                .addComponent(startDCSSimButton)
                .addContainerGap())
        );

        stageOneOptionSpinner.getAccessibleContext().setAccessibleDescription("Stage 1 Timeout (in seconds)");

        optionTabbedPane.addTab("D-CS Algorithm", dcsOptionPanel);

        intellidriveOptionPanel.setToolTipText("D-CS Intellidrive");
        intellidriveOptionPanel.setPreferredSize(new java.awt.Dimension(239, 123));

        intellidriveStageOneOptionSpinner.setModel(new javax.swing.SpinnerNumberModel(Double.valueOf(10.0d), Double.valueOf(0.0d), null, Double.valueOf(0.1d)));
        intellidriveStageOneOptionSpinner.setToolTipText("Stage 1 Timeout (in seconds)");
        intellidriveStageOneOptionSpinner.setMinimumSize(new java.awt.Dimension(80, 60));
        intellidriveStageOneOptionSpinner.setPreferredSize(new java.awt.Dimension(80, 20));

        intellidriveStageTwoOptionSpinner.setModel(new javax.swing.SpinnerNumberModel(Double.valueOf(5.0d), Double.valueOf(0.0d), null, Double.valueOf(0.1d)));
        intellidriveStageTwoOptionSpinner.setToolTipText("Stage 2 Timeout (in seconds)");
        intellidriveStageTwoOptionSpinner.setPreferredSize(new java.awt.Dimension(80, 20));

        intellidriveStartDCSSimButton.setMnemonic('s');
        intellidriveStartDCSSimButton.setText("Start Simulation");
        intellidriveStartDCSSimButton.setToolTipText("Start Simulation");
        intellidriveStartDCSSimButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                intellidriveStartDCSSimButtonActionPerformed(evt);
            }
        });

        intellidriveStageOneOptionLabel.setText("Stage 1 Timeout (in seconds):");

        intellidriveStageTwoOptionLabel.setText("Stage 2 Timeout (in seconds):");

        intellidriveLeg1CheckBox.setText("Leg 1");
        intellidriveLeg1CheckBox.setToolTipText("Leg 1");
        intellidriveLeg1CheckBox.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        intellidriveLeg1CheckBox.setHorizontalTextPosition(javax.swing.SwingConstants.LEADING);

        intellidriveLeg2CheckBox.setText("Leg 2");
        intellidriveLeg2CheckBox.setToolTipText("Leg 2");
        intellidriveLeg2CheckBox.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        intellidriveLeg2CheckBox.setHorizontalTextPosition(javax.swing.SwingConstants.LEADING);

        intellidriveLeg3CheckBox.setText("Leg 3");
        intellidriveLeg3CheckBox.setToolTipText("Leg 3");
        intellidriveLeg3CheckBox.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        intellidriveLeg3CheckBox.setHorizontalTextPosition(javax.swing.SwingConstants.LEADING);

        intellidriveLeg4CheckBox.setText("Leg 4");
        intellidriveLeg4CheckBox.setToolTipText("Leg 4");
        intellidriveLeg4CheckBox.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        intellidriveLeg4CheckBox.setHorizontalTextPosition(javax.swing.SwingConstants.LEADING);

        dsrcIntellidriveSpinner.setModel(new javax.swing.SpinnerNumberModel(100, 0, 100, 1));
        dsrcIntellidriveSpinner.setToolTipText("Percentage of DSRC vehicles");
        dsrcIntellidriveSpinner.setMinimumSize(new java.awt.Dimension(80, 20));
        dsrcIntellidriveSpinner.setPreferredSize(new java.awt.Dimension(80, 20));

        dsrcIntellidriveLabel.setText("Percentage of DSRC vehicles:");

        legIntellidriveLabel.setText("Apply D-CS to:");

        hostTextField1.setText("http://localhost");
        hostTextField1.setEnabled(false);

        portTextField1.setText("8888");
        portTextField1.setEnabled(false);

        portLabel1.setText("Port:");
        portLabel1.setEnabled(false);

        enableCheckBox1.setText("Enable Web Broadcast");
        enableCheckBox1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                enableCheckBox1ActionPerformed(evt);
            }
        });

        hostLabel1.setText("Host:");
        hostLabel1.setEnabled(false);

        javax.swing.GroupLayout intellidriveOptionPanelLayout = new javax.swing.GroupLayout(intellidriveOptionPanel);
        intellidriveOptionPanel.setLayout(intellidriveOptionPanelLayout);
        intellidriveOptionPanelLayout.setHorizontalGroup(
            intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(intellidriveOptionPanelLayout.createSequentialGroup()
                .addGroup(intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(intellidriveOptionPanelLayout.createSequentialGroup()
                        .addContainerGap(84, Short.MAX_VALUE)
                        .addGroup(intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, intellidriveOptionPanelLayout.createSequentialGroup()
                                .addComponent(intellidriveLeg1CheckBox)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(intellidriveLeg2CheckBox)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(intellidriveLeg3CheckBox)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(intellidriveLeg4CheckBox))
                            .addComponent(intellidriveStartDCSSimButton, javax.swing.GroupLayout.Alignment.TRAILING)
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, intellidriveOptionPanelLayout.createSequentialGroup()
                                .addGroup(intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, intellidriveOptionPanelLayout.createSequentialGroup()
                                            .addComponent(intellidriveStageOneOptionLabel)
                                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED))
                                        .addGroup(intellidriveOptionPanelLayout.createSequentialGroup()
                                            .addComponent(intellidriveStageTwoOptionLabel)
                                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED))
                                        .addGroup(intellidriveOptionPanelLayout.createSequentialGroup()
                                            .addComponent(legIntellidriveLabel)
                                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)))
                                    .addGroup(intellidriveOptionPanelLayout.createSequentialGroup()
                                        .addComponent(dsrcIntellidriveLabel)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)))
                                .addGroup(intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addComponent(dsrcIntellidriveSpinner, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(intellidriveStageTwoOptionSpinner, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(intellidriveStageOneOptionSpinner, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, intellidriveOptionPanelLayout.createSequentialGroup()
                        .addGap(86, 86, 86)
                        .addGroup(intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(enableCheckBox1)
                            .addGroup(intellidriveOptionPanelLayout.createSequentialGroup()
                                .addGroup(intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(hostLabel1)
                                    .addComponent(portLabel1))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(portTextField1, javax.swing.GroupLayout.DEFAULT_SIZE, 199, Short.MAX_VALUE)
                                    .addComponent(hostTextField1, javax.swing.GroupLayout.DEFAULT_SIZE, 199, Short.MAX_VALUE))))))
                .addContainerGap())
        );
        intellidriveOptionPanelLayout.setVerticalGroup(
            intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(intellidriveOptionPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(dsrcIntellidriveSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(dsrcIntellidriveLabel))
                .addGap(18, 18, 18)
                .addGroup(intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(intellidriveStageOneOptionSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(intellidriveStageOneOptionLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(intellidriveStageTwoOptionSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(intellidriveStageTwoOptionLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(enableCheckBox1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(hostLabel1)
                    .addComponent(hostTextField1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(portTextField1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(portLabel1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 42, Short.MAX_VALUE)
                .addComponent(legIntellidriveLabel)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(intellidriveOptionPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(intellidriveLeg4CheckBox)
                    .addComponent(intellidriveLeg3CheckBox)
                    .addComponent(intellidriveLeg2CheckBox)
                    .addComponent(intellidriveLeg1CheckBox))
                .addGap(18, 18, 18)
                .addComponent(intellidriveStartDCSSimButton)
                .addContainerGap())
        );

        optionTabbedPane.addTab("D-CS Intellidrive", intellidriveOptionPanel);

        javax.swing.GroupLayout optionDialogLayout = new javax.swing.GroupLayout(optionDialog.getContentPane());
        optionDialog.getContentPane().setLayout(optionDialogLayout);
        optionDialogLayout.setHorizontalGroup(
            optionDialogLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(optionDialogLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(optionTabbedPane, javax.swing.GroupLayout.DEFAULT_SIZE, 326, Short.MAX_VALUE)
                .addContainerGap())
        );
        optionDialogLayout.setVerticalGroup(
            optionDialogLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(optionDialogLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(optionTabbedPane, javax.swing.GroupLayout.PREFERRED_SIZE, 334, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("D-CS Simulator - eTEXAS V7.0");
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

    private void optionDialogWindowClosed(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_optionDialogWindowClosed
        if (optionMode == SIM_OPTION_UNINITIALIZED) {
            this.dispose();
        }
    }//GEN-LAST:event_optionDialogWindowClosed

    private void startDCSSimButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_startDCSSimButtonActionPerformed
        //Set the simulation mode to run with a DCS algorithm
        try {
            //Set the simulation mode to run with a DCS algorithm
            optionMode = SIM_OPTION_DCS;

            Map<String, String> eTEXASConfig = new HashMap<String, String>();
            Map<String, String> interRepConfig = new HashMap<String, String>();

            interRepConfig.put(HTTP_RX_ENABLED, "false");
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
                interRep = new InterRep(new DCSSimController(texasSim));
            } catch (ClassNotFoundException ex) {
                LOGGER.debug(ex.toString());
            }


            //Obtain the simulation configuration data from UI components and dispose of dialog
            randomDSRC = (Integer) dsrcDCSSpinner.getValue();
//            Integer detectorDistance = (Integer) distanceOptionSpinner.getValue();
            Double stage1Timeout = (Double) stageOneOptionSpinner.getValue();
            Double stage2Timeout = (Double) stageTwoOptionSpinner.getValue();

            //Set the random DSRC assignment to be predictable.
            MessageModule module = texasSim.getModule(BasicSafetyMessage.class.getName());
            if (module != null) {
                MessageProducer messageProducer = module.getMessageProducer();

                if (messageProducer != null && messageProducer instanceof BSMProducer) {
                    BSMProducer bsmProducer = (BSMProducer) messageProducer;
                    bsmProducer.resetRandomizer(1);
                    bsmProducer.setDsrcPercentage(randomDSRC);
                }
            }
            optionDialog.dispose();

            //Initialize the DCS algorithm
            algorithm = new DCSAlgorithm(new SignalControllerForETEXAS(texasSim, interRep), interRep);
            algorithm.configureTimeouts(stage1Timeout, stage2Timeout);

            //Make sure to dispose of any previous frames



        } catch (NumberFormatException e) {
            // TODO: bbadillo - add an input validation dialog. 
        }
    }//GEN-LAST:event_startDCSSimButtonActionPerformed

    /**
     * This function is necessary because when the startDCS button is performed, there are still no lanes to iterate through and put in the
     * BTTE table, therefore this function is called after the first message is sent to initialize and display the frame.
     */
    private void initializeSimulationFrame() {
        boolean leg1Selected, leg2Selected, leg3Selected, leg4Selected;
        if (optionMode == SIM_OPTION_DCS) {
            leg1Selected = leg1CheckBox.isSelected();
            leg2Selected = leg2CheckBox.isSelected();
            leg3Selected = leg3CheckBox.isSelected();
            leg4Selected = leg4CheckBox.isSelected();
        } else{
            leg1Selected = intellidriveLeg1CheckBox.isSelected();
            leg2Selected = intellidriveLeg2CheckBox.isSelected();
            leg3Selected = intellidriveLeg3CheckBox.isSelected();
            leg4Selected = intellidriveLeg4CheckBox.isSelected();
        }

        Iterator<Integer> laneIDs = interRep.getIntersectionManager().getLanes().keySet().iterator();
        List<Integer> listOfLanes = new LinkedList<Integer>();
        while (laneIDs.hasNext()) {
            Lane currLane = interRep.getIntersectionManager().getLaneById(laneIDs.next());
            if (Lane.INBOUND.equals(currLane.getType())) {
                int approachId = currLane.getApproachId();
                if ((leg1Selected && (approachId == 1)) || (leg2Selected && (approachId == 2)) || (leg3Selected && (approachId == 3)) || (leg4Selected && (approachId == 4))) {
                    listOfLanes.add(currLane.getLaneId());
                }
            }
        }
        int[] lanes = new int[listOfLanes.size()];
        for (int i = 0; i < listOfLanes.size(); i++) {
            lanes[i] = listOfLanes.get(i);
        }

        if (dcsSimulationFrame != null) {
            dcsSimulationFrame.dispose();
        }
        dcsSimulationFrame = new DCSSimulationFrame(lanes, algorithm.getTimeInterval());

        dcsSimulationFrame.setVisible(true);
    }

    private void intellidriveStartDCSSimButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_intellidriveStartDCSSimButtonActionPerformed
        //Set the simulation mode to run with a DCS algorithm
        optionMode = SIM_OPTION_DCS_INTELLIDRIVE;

        Map<String, String> eTEXASConfig = new HashMap<String, String>();
        Map<String, String> interRepConfig = new HashMap<String, String>();
        interRepConfig.put(HTTP_RX_ENABLED, "false");
        eTEXASConfig.put(HTTP_TX_ENABLED, "false");

        try {
            texasSim = new eTEXAS();
            texasSim.setup(eTEXASConfig);
            interRep = new InterRep(new DCSSimController(texasSim));
        } catch (ClassNotFoundException ex) {
            LOGGER.debug(ex.toString());


        //Obtain the simulation configuration data from UI components and dispose of dialog
        randomDSRC = (Integer) dsrcDCSSpinner.getValue();
//            Integer detectorDistance = (Integer) distanceOptionSpinner.getValue();
        Double stage1Timeout = (Double) stageOneOptionSpinner.getValue();
        Double stage2Timeout = (Double) stageTwoOptionSpinner.getValue();

        MessageModule module = texasSim.getModule(BasicSafetyMessage.class.getName());
        if (module != null) {
            MessageProducer messageProducer = module.getMessageProducer();

            if (messageProducer != null && messageProducer instanceof BSMProducer) {
                BSMProducer bsmProducer = (BSMProducer) messageProducer;
                bsmProducer.resetRandomizer(1);
                bsmProducer.setDsrcPercentage(randomDSRC);
            }
        }
        optionDialog.dispose();

        algorithm = new DCSAlgorithm(new SignalControllerForETEXAS(texasSim, interRep), interRep);
        algorithm.configureTimeouts(stage1Timeout, stage2Timeout);
    }//GEN-LAST:event_intellidriveStartDCSSimButtonActionPerformed

    private void enableCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_enableCheckBoxActionPerformed

        boolean enable = enableCheckBox.isSelected();         hostLabel.setEnabled(enable);         hostTextField.setEnabled(enable);         portLabel.setEnabled(enable);         portTextField.setEnabled(enable);     }//GEN-LAST:event_enableCheckBoxActionPerformed

    private void enableCheckBox1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_enableCheckBox1ActionPerformed
        boolean enable1 = enableCheckBox1.isSelected();
        hostLabel1.setEnabled(enable1);
        hostTextField1.setEnabled(enable1);
        portLabel1.setEnabled(enable1);
        portTextField1.setEnabled(enable1);
        boolean enable = enableCheckBox.isSelected();         hostLabel.setEnabled(enable);         hostTextField.setEnabled(enable);         portLabel.setEnabled(enable);         portTextField.setEnabled(enable);     }//GEN-LAST:event_enableCheckBox1ActionPerformed

    //TODO egaebel -- why is this commented out!?????
//    /**
//     * An initialization method used to setup the simulation after the options
//     * have been entered. 
//     */
//    private void initializeOptions() {
//        texasSim.getBsmManager.resetRandomizer(1);
//        texasSim.getBsmManager().setDsrcPercentage(randomDSRC);
//        //Initialize the vehicle login/logout events
//        texasSim.getBsmManager().registerListener(this);
//    }
    /**
     * A helper method to iterate through all of the vehicles currently logged into
     * the TEXAS model and provide each vehicles information on separate rows in the
     * table.
     *
     * @param currentVehicles A collection which contains all of the vehicles
     * currently logged into the system. 
     */
    private void populateVehicleTable() {

        DefaultTableModel tableModel = (DefaultTableModel) vehicleTable.getModel();
        Collection<Integer> laneIDs = interRep.getIntersectionManager().getLanes().keySet();
        // Iterate through all the vehicles and reset the table data
        Set<Integer> theVehicleIds = interRep.getVehicleManager().getAllVehicleIds();
        
        //Must sort the ids so they can be nicely presented in DT Simulator display
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
            if ((lane != null && Lane.INBOUND.equals(lane.getType())) || lane == null) {
                if (lane != null) {
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
                    distString = String.format("%1$.2f ft", 
                    		UtilsInterRep.getDistToStopLine(currVehicle, interRep.getIntersectionManager().getLaneById(currVehicle.getLaneID())));
                    if (currVehicle.getSpeed() != 0) {
                        double timeToIntersection = 
                        		UtilsInterRep.getDistToStopLine(currVehicle, interRep.getIntersectionManager().getLaneById(currVehicle.getLaneID())) / 
                        		currVehicle.getSpeed();
                        timeToIntersectionString = String.format("%1$.2f s", timeToIntersection);
                    } else {
                        timeToIntersectionString = "Stopped";
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
            if (algorithm != null && dcsSimulationFrame != null) {
                //Register a listener for the last DT simulation in the loop

                algorithm.registerListener(dcsSimulationFrame);
                dcsSimulationFrame.stateChanged(algorithm.getState());
            }
            performTimeStep();
            //Remove the listener so that UI changes don't occur on looping DT simulations.
            if (algorithm != null) {
                algorithm.removeListener(dcsSimulationFrame);
            }

            // Perform tasks to finish the simulation loop

            if (currentVehicles != null) {
                populateVehicleTable();
            }

            if (texasSim.isFinished()) {
                simDTButton.setEnabled(false);
                logger.debug( "Finished Simulation...");
            }

            progressBar.setValue(0);
        }

        /**
         * Procedure to execute the next DT in the TEXAS Model simulator and
         * retrieve vehicle data. Optionally a D-CS algorithm is used in this
         * procedure if it has been initialized. 
         */
        private void performTimeStep() {
            //Perform a DT in the TEXAS Model simulation
            texasSim.nextTimeStep();

            //Get the current time (in seconds) in the simulation
            double currTime = texasSim.getCurrentTime();
            timeTextField.setText("" + currTime); // Prints the double as a string

            //Update the interRep after texasSim
            interRep.update();
            if (texasSim.getCurrentTime() == texasSim.getModelData().getDTSize()) {
                initializeSimulationFrame();
            }
            // Get the vehicles currently in the simulation
            currentVehicles = interRep.getVehicleManager().getAllVehicleIds();

            // Perform a D-CS step using the current TEXAS Model simulation values
            if (algorithm != null) {
                algorithm.performDetectionControlLoop(currTime);
            }
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
                java.awt.EventQueue.invokeLater(new Runnable() {

                    @Override
                    public void run() {
                        try {
                            File jarFile = new File(DCSSimConsole.class.getProtectionDomain().getCodeSource().getLocation().toURI());
                            String jarFilename = jarFile.getName();
                            LoggerFactory.getLogger(DCSSimConsole.class.getName()).log(Level.INFO, "Jar file location: {}", jarFilename);
                            eTEXASRunner runner = new eTEXASRunner(jarFilename, libraryPath);
                            runner.setVisible(true);
                        } catch (URISyntaxException ex) {
                            LOGGER.debug(ex.toString());
                        }
                    }
                });
            }
        } else {
            // Execute the simulator; finding simpro.par in the current working directory
            java.awt.EventQueue.invokeLater(new Runnable() {

                @Override
                public void run() {
                    try {
                        DCSSimConsole console = new DCSSimConsole();
                        console.setVisible(true);
                    } catch (ClassNotFoundException ex) {
                        LOGGER.debug(ex.toString());
                    }
                }
            });
        }
    }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton changeSignalButton;
    private javax.swing.JPanel dcsOptionPanel;
    private javax.swing.JLabel distanceOptionLabel;
    private javax.swing.JSpinner distanceOptionSpinner;
    private javax.swing.JLabel dsrcDCSLabel;
    private javax.swing.JSpinner dsrcDCSSpinner;
    private javax.swing.JLabel dsrcIntellidriveLabel;
    private javax.swing.JSpinner dsrcIntellidriveSpinner;
    private javax.swing.JLabel dtNumLabel;
    private javax.swing.JSpinner dtNumSpinner;
    private javax.swing.JCheckBox enableCheckBox;
    private javax.swing.JCheckBox enableCheckBox1;
    private javax.swing.JButton finSimButton;
    private javax.swing.JButton holdSignalButton;
    private javax.swing.JSpinner holdSignalSpinner;
    private javax.swing.JLabel hostLabel;
    private javax.swing.JLabel hostLabel1;
    private javax.swing.JTextField hostTextField;
    private javax.swing.JTextField hostTextField1;
    private javax.swing.JCheckBox intellidriveLeg1CheckBox;
    private javax.swing.JCheckBox intellidriveLeg2CheckBox;
    private javax.swing.JCheckBox intellidriveLeg3CheckBox;
    private javax.swing.JCheckBox intellidriveLeg4CheckBox;
    private javax.swing.JPanel intellidriveOptionPanel;
    private javax.swing.JLabel intellidriveStageOneOptionLabel;
    private javax.swing.JSpinner intellidriveStageOneOptionSpinner;
    private javax.swing.JLabel intellidriveStageTwoOptionLabel;
    private javax.swing.JSpinner intellidriveStageTwoOptionSpinner;
    private javax.swing.JButton intellidriveStartDCSSimButton;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JCheckBox leg1CheckBox;
    private javax.swing.JCheckBox leg2CheckBox;
    private javax.swing.JCheckBox leg3CheckBox;
    private javax.swing.JCheckBox leg4CheckBox;
    private javax.swing.JLabel legCheckBoxLabel;
    private javax.swing.JLabel legIntellidriveLabel;
    private javax.swing.JDialog optionDialog;
    private javax.swing.JTabbedPane optionTabbedPane;
    private javax.swing.JLabel portLabel;
    private javax.swing.JLabel portLabel1;
    private javax.swing.JTextField portTextField;
    private javax.swing.JTextField portTextField1;
    private javax.swing.JProgressBar progressBar;
    private javax.swing.JButton simDTButton;
    private javax.swing.JLabel stageOneOptionLabel;
    private javax.swing.JSpinner stageOneOptionSpinner;
    private javax.swing.JLabel stageTwoOptionLabel;
    private javax.swing.JSpinner stageTwoOptionSpinner;
    private javax.swing.JButton startDCSSimButton;
    private javax.swing.JLabel timeLabel;
    private javax.swing.JTextField timeTextField;
    private javax.swing.JTable vehicleTable;
    // End of variables declaration//GEN-END:variables
}
