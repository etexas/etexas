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
 * DCSSimulationFrame.java
 *
 * Created on Aug 6, 2010, 10:25:28 AM
 */
package org.etexascode.dcs.gui;

import org.etexascode.dcs.DCSListener;
import org.etexascode.dcs.PhaseStatusComponent;
import org.etexascode.dcs.TimeIntervalData;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;

/**
 * A GUI class used as the interface for users to compare simulation executions
 *
 * @author bbadillo
 * @author cbrown
 */
public class DCSSimulationFrame extends javax.swing.JFrame implements DCSListener {

    /**
     * The length (in seconds) of each time interval in the time interval matrix.
     */
    double intervalLength;
    /**
     * An array which contains the lane identifiers of the lanes shown in the GUI table.
     * The index maps to the GUI table column index. 
     */
    int lanes[] = null;

    /** 
     * Creates new form DCSSimulationFrame
     *
     * @param lanes An array which contains the lane identifiers of the lanes
     * shown in the GUI table.
     * @param  intervalLength The length (in seconds) of each time interval in the time interval matrix.
     */
    public DCSSimulationFrame(int lanes[], double intervalLength) {
        initComponents();

        this.intervalLength = intervalLength;
        this.lanes = lanes;
        setLanes();

        // Align above the center vertical line and on center horizonal line.
        Toolkit kit = Toolkit.getDefaultToolkit();
        Dimension screenSize = kit.getScreenSize();
        int screenWidth = (int) screenSize.getWidth();     // getting Screen width
        int screenHeight = (int) screenSize.getHeight();  // getting Screen height
        this.setLocation(((screenWidth / 2) - (this.getWidth() / 2)),
                ((screenHeight / 2)));
    }

    /**
     * Create the GUI table model based on the lanes that are used in the construction
     * of this object. 
     */
    private void setLanes() {
        String columnTitles[] = new String[lanes.length + 1];
        final Class[] typesArray = new Class[lanes.length + 1];
        final boolean[] canEditArray = new boolean[lanes.length + 1];
        for (int i = 0; i < lanes.length; i++) {
            columnTitles[i] = "Lane " + lanes[i];
            typesArray[i] = CellObject.class;
            canEditArray[i] = false;
        }
        columnTitles[lanes.length] = "Time Interval";
        typesArray[lanes.length] = String.class;
        canEditArray[lanes.length] = false;

        timeIntervalTable.setModel(new javax.swing.table.DefaultTableModel(
                new Object[][]{},
                columnTitles) {

            Class[] types = typesArray;
            boolean[] canEdit = canEditArray;

            @Override
            public Class getColumnClass(int columnIndex) {
                return types[columnIndex];
            }

            @Override
            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return canEdit[columnIndex];
            }
        });

        timeIntervalTable.setDefaultRenderer(CellObject.class, new ColorRenderer());

        for (int i = 0; i < lanes.length; i++) {
            timeIntervalTable.getColumnModel().getColumn(i).setResizable(false);
        }
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jScrollPane1 = new javax.swing.JScrollPane();
        timeIntervalTable = new javax.swing.JTable();
        stateTextField = new javax.swing.JTextField();
        stateLabel = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Best Time to End (BTTE) Green Phase Hold");

        timeIntervalTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {

            },
            new String [] {

            }
        ));
        timeIntervalTable.getTableHeader().setReorderingAllowed(false);
        jScrollPane1.setViewportView(timeIntervalTable);

        stateTextField.setEditable(false);
        stateTextField.setToolTipText("Current State");
        stateTextField.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                stateTextFieldActionPerformed(evt);
            }
        });

        stateLabel.setText("Current State:");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 582, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(stateLabel)
                    .addComponent(stateTextField, javax.swing.GroupLayout.PREFERRED_SIZE, 69, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(stateLabel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(stateTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 271, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void stateTextFieldActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_stateTextFieldActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_stateTextFieldActionPerformed
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JLabel stateLabel;
    private javax.swing.JTextField stateTextField;
    private javax.swing.JTable timeIntervalTable;
    // End of variables declaration//GEN-END:variables

    /**
     * This method is called when a new time interval matrix is available.
     *
     * @param timeIntervalDataMap A map containing arrays of car and truck counts
     * at certain time intervals. The map is keyed by the lane ID of the lane for
     * which it is computed.
     */
    @Override
    public void matrixComputed(Map<Integer, TimeIntervalData[]> timeIntervalDataMap) {

        DefaultTableModel tableModel = (DefaultTableModel) timeIntervalTable.getModel();

        // Iterate through all the entries and reset the table data
        List<Integer> emptyLanes = new LinkedList<Integer>();
        for (int c = 0; c < lanes.length; c++) {
            int laneId = lanes[c];

            TimeIntervalData[] timeIntervalData = timeIntervalDataMap.get(laneId);

            if (timeIntervalData != null) {
                while (tableModel.getRowCount() < timeIntervalData.length) {
                    double rowCount = tableModel.getRowCount();
                    int columnCount = tableModel.getColumnCount();
                    //This loop puts cells with null values in however many places are necessary, rather than a fixed amount
                    Object[] newRow = new Object[columnCount];
                    for (int i = 0; i < columnCount - 1; i++) {
                        newRow[i] = new CellObject("", null);
                    }
                    String timeIntervalString = String.format("%1$.1fs - %2$.1fs", rowCount * intervalLength, rowCount * intervalLength + intervalLength);
                    newRow[columnCount - 1] = timeIntervalString;
                    tableModel.addRow(newRow);
                }

                for (int r = 0; r < timeIntervalData.length; r++) {
                    CellObject cellObj = (CellObject) tableModel.getValueAt(r, c);
                    cellObj.setLabel("C=" + timeIntervalData[r].getNumberOfCars() + ", T=" + timeIntervalData[r].getNumberOfTrucks());
                    cellObj.setColor(null);
                    tableModel.setValueAt(cellObj, r, c);
                }
            } else {
                emptyLanes.add(c);
            }
        }
        Iterator<Integer> iterator = emptyLanes.iterator();
        while (iterator.hasNext()) {
            Integer c = iterator.next();
            for (int r = 0; r < tableModel.getRowCount(); r++) {
                CellObject cellObj = (CellObject) tableModel.getValueAt(r, c);
                cellObj.setLabel("C=0, T=0");
                cellObj.setColor(null);
                tableModel.setValueAt(cellObj, r, c);
            }
        }

    }

    /**
     * This method is called when the "best-time-to-end" has been calculated.
     *
     * @param BTTE The index of the (best-time-to-end) for a time interval matrix.
     */
    @Override
    public void btteComputed(Integer BTTE) {
        DefaultTableModel tableModel = (DefaultTableModel) timeIntervalTable.getModel();

        // Iterate through all the entries and reset the table data
        for (int c = 0; c < lanes.length; c++) {
            if (tableModel.getRowCount() == 0) {
                int columnCount = tableModel.getColumnCount();
                //This loop puts cells with null values in however many places are necessary, rather than a fixed amount
                Object[] newRow = new Object[columnCount];
                for (int i = 0; i < columnCount - 1; i++) {
                    newRow[i] = new CellObject("", null);
                }
                String timeIntervalString = String.format("%1$.1fs - %2$.1fs", 0.0, intervalLength);
                newRow[columnCount - 1] = timeIntervalString;

                tableModel.addRow(newRow);
            }
            if (BTTE != null) {
                CellObject value = (CellObject) tableModel.getValueAt(BTTE, c);
                value.setColor(Color.GREEN);
                tableModel.setValueAt(value, BTTE, c);
            }
        }
    }

    /**
     * This method is called when the state of the algorithm has changed.
     *
     * @param currentState The current state of the algorithm.
     */
    public void stateChanged(int currentState) {
        if (currentState == PhaseStatusComponent.NO_STAGE) {
            stateTextField.setText("Waiting");
        } else if (currentState == PhaseStatusComponent.STAGE_1) {
            stateTextField.setText("Stage 1");
        } else if (currentState == PhaseStatusComponent.STAGE_2) {
            stateTextField.setText("Stage 2");
        } else if (currentState == PhaseStatusComponent.MAX_OUT) {
            stateTextField.setText("Max Out");
        } else {
            stateTextField.setText("");
        }
    }

    public class CellObject {

        String label;
        Color color;

        public CellObject(String label, Color color) {
            this.label = label;
            this.color = color;
        }

        public String getLabel() {
            return label;
        }

        public void setLabel(String label) {
            this.label = label;
        }

        public Color getColor() {
            return color;
        }

        public void setColor(Color color) {
            this.color = color;
        }
    }

    public class ColorRenderer extends JLabel
            implements TableCellRenderer {

        public ColorRenderer() {
        }

        @Override
        public Component getTableCellRendererComponent(
                JTable table, Object obj,
                boolean isSelected, boolean hasFocus,
                int row, int column) {
            CellObject cellObj = (CellObject) obj;
            if (cellObj != null) {
                setText(cellObj.getLabel());
                if (cellObj.getColor() != null) {
                    setOpaque(true);
                    setBackground(cellObj.getColor());
                } else {
                    setOpaque(false);
                }
            } else {
                setOpaque(false);
            }
            return this;
        }
    }
}
