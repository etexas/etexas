package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                     NemaRingGroupControllerDialog.java                     */
/******************************************************************************/
/*                                                                            */
/*     gdvsim COPYRIGHT (C) 2004 by Rioux Engineering, Austin, Texas USA      */
/*                                                                            */
/*   Permission is hereby granted to use, modify, copy, and distribute this   */
/*   software and its documentation for any purpose only without profit,      */
/*   provided that the above Copyright Notice appears in all copies and that  */
/*   both the Copyright Notice and this Permission Notice appears in every    */
/*   copy of supporting documentation.  No title to nor ownership of the      */
/*   software is transferred hereby.  The name of Rioux Engineering shall not */
/*   be used in advertising or publicity related to the distribution of the   */
/*   software without specific, written, prior permission.  This software is  */
/*   provided as-delivered without expressed or implied warranty.  Rioux      */
/*   Engineering makes no representation about the suitability of this        */
/*   software for any purpose and accepts no responsibility for its use.      */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*   This program is free software; you can redistribute it and/or modify     */
/*   it under the terms of the GNU General Public License as published by     */
/*   the Free Software Foundation; either version 2 of the License, or        */
/*   (at your option) any later version.                                      */
/*                                                                            */
/*   This program is distributed in the hope that it will be useful,          */
/*   but WITHOUT ANY WARRANTY; without even the implied warranty of           */
/*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            */
/*   GNU General Public License for more details.                             */
/*                                                                            */
/*   You should have received a copy of the GNU General Public License        */
/*   along with this program; if not, write to the Free Software              */
/*   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA  */
/*                                                                            */
/******************************************************************************/

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.border.*;
import java.lang.*;
import java.text.DecimalFormat;
import java.util.*;
import java.util.regex.*;
import java.io.*;
import javax.swing.JOptionPane;
import java.awt.Graphics;

public class NemaRingGroupControllerDialog extends JFrame {

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JPanel ok_panel;

    JButton okButton, closeButton;

    Font font;

    JLabel label_title;

    JRadioButton SequentialControllerRadioButton, TraditionalControllerRadioButton, FullyAdjustableControllerRadioButton;

    ButtonGroup aButtonGroup;

    OkApplyKeyListener okApplyKeyListener;

    OkApplyActionListener okApplyActionListener;

    RadioButtonKeyListener radioButtonKeyListener;

    HelpListener helpListener;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    public NemaRingGroupControllerDialog() {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP];

        titleString = lclv_tx_fmt.mstv_name;

        aFrame = new JFrame(titleString);
        container = aFrame.getContentPane();

        JPanel wholePanel = new JPanel();
        container = wholePanel;

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        aFrame.getContentPane().add(scrollpane);

        gbLayout = new GridBagLayout();
        gbConstraints = new GridBagConstraints();
        container.setLayout(gbLayout);

        label_title = new JLabel("Please select a controller:");
        font = new Font("TimesRoman", Font.BOLD, 20);
        label_title.setFont(font);

        gbConstraints.fill = GridBagConstraints.BOTH;

        SequentialControllerRadioButton = new JRadioButton("Sequential Controller (1 Ring and 1 Group)", false);
        TraditionalControllerRadioButton = new JRadioButton("Traditional Controller (2 Rings and 2 Groups)", false);
        FullyAdjustableControllerRadioButton = new JRadioButton("Fully Adjustable Controller (up to 4 Rings and 4 Groups)", true);

        SequentialControllerRadioButton.setMnemonic('S');
        TraditionalControllerRadioButton.setMnemonic('T');
        FullyAdjustableControllerRadioButton.setMnemonic('F');

        if (gdvsim.flag_nemaRingGroup_ok) {
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_rings == 1
                        && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_groups == 1) {
                    SequentialControllerRadioButton.setSelected(true);
                }
                else if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_rings == 2
                        && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_groups == 2) {
                    TraditionalControllerRadioButton.setSelected(true);
                }
                else {
                    FullyAdjustableControllerRadioButton.setSelected(true);
                }
            }

        }

        aButtonGroup = new ButtonGroup();
        aButtonGroup.add(SequentialControllerRadioButton);
        aButtonGroup.add(TraditionalControllerRadioButton);
        aButtonGroup.add(FullyAdjustableControllerRadioButton);

        SequentialControllerRadioButton.getAccessibleContext().setAccessibleName("Sequential Controller");
        SequentialControllerRadioButton.getAccessibleContext().setAccessibleDescription("Sequential Controller");

        TraditionalControllerRadioButton.getAccessibleContext().setAccessibleName("Traditional Controller");
        TraditionalControllerRadioButton.getAccessibleContext().setAccessibleDescription("Traditional Controller");

        FullyAdjustableControllerRadioButton.getAccessibleContext().setAccessibleName("Fully Adjustable Controller");
        FullyAdjustableControllerRadioButton.getAccessibleContext().setAccessibleDescription("Fully Adjustable Controller");

        okButton = new JButton("  OK   ");
        closeButton = new JButton(" Close ");

        okButton.setMnemonic(KeyEvent.VK_O);
        closeButton.setMnemonic(KeyEvent.VK_C);

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        closeButton.getAccessibleContext().setAccessibleName("Close");
        closeButton.getAccessibleContext().setAccessibleDescription("Close");

        ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(closeButton);

        gbConstraints.insets = new Insets(5, 0, 5, 0);
        addComponent(label_title, 0, 0, 1, 1);

        gbConstraints.insets = new Insets(5, 40, 5, 0);
        addComponent(SequentialControllerRadioButton, 1, 0, 1, 1);
        addComponent(TraditionalControllerRadioButton, 2, 0, 1, 1);
        addComponent(FullyAdjustableControllerRadioButton, 3, 0, 1, 1);

        gbConstraints.insets = new Insets(5, 0, 5, 0);
        addComponent(ok_panel, 4, 0, 1, 1);

        helpListener = new HelpListener();
        okApplyKeyListener = new OkApplyKeyListener();
        okApplyActionListener = new OkApplyActionListener();
        radioButtonKeyListener = new RadioButtonKeyListener();

        SequentialControllerRadioButton.addKeyListener(helpListener);
        SequentialControllerRadioButton.addKeyListener(okApplyKeyListener);
        SequentialControllerRadioButton.addKeyListener(radioButtonKeyListener);
        SequentialControllerRadioButton.addActionListener(okApplyActionListener);

        TraditionalControllerRadioButton.addKeyListener(helpListener);
        TraditionalControllerRadioButton.addKeyListener(okApplyKeyListener);
        TraditionalControllerRadioButton.addKeyListener(radioButtonKeyListener);
        TraditionalControllerRadioButton.addActionListener(okApplyActionListener);

        FullyAdjustableControllerRadioButton.addKeyListener(helpListener);
        FullyAdjustableControllerRadioButton.addKeyListener(okApplyKeyListener);
        FullyAdjustableControllerRadioButton.addKeyListener(radioButtonKeyListener);
        FullyAdjustableControllerRadioButton.addActionListener(okApplyActionListener);

        okButton.addKeyListener(helpListener);
        okButton.addKeyListener(okApplyKeyListener);
        okButton.addActionListener(okApplyActionListener);

        closeButton.addKeyListener(helpListener);
        closeButton.addKeyListener(okApplyKeyListener);
        closeButton.addActionListener(okApplyActionListener);

        Set<KeyStroke> forwardKeys = new HashSet<KeyStroke>();
        forwardKeys.add(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0));
        forwardKeys.add(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0));
        aFrame.setFocusTraversalKeys(KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS, forwardKeys);

        Set<KeyStroke> backwardKeys = new HashSet<KeyStroke>();
        backwardKeys.add(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, KeyEvent.SHIFT_DOWN_MASK));
        backwardKeys.add(KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0));
        aFrame.setFocusTraversalKeys(KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS, backwardKeys);

        aFrame.setSize(850, 680);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method NemaRingGroupControllerSelectionDialog

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);

    } // end of method addComponent

    class RadioButtonKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == SequentialControllerRadioButton) {
                    SequentialControllerRadioButton.setSelected(true);
                }

                if (event.getSource() == TraditionalControllerRadioButton) {
                    TraditionalControllerRadioButton.setSelected(true);
                }

                if (event.getSource() == FullyAdjustableControllerRadioButton) {
                    FullyAdjustableControllerRadioButton.setSelected(true);
                }

            }
        }
    } // end of RadioButtonKeyListener

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                if (event.getSource() == SequentialControllerRadioButton || event.getSource() == TraditionalControllerRadioButton || event.getSource() == FullyAdjustableControllerRadioButton) {
                    if (SequentialControllerRadioButton.isSelected()) {
                        new HelpDialog(true, "Select A Controller", "Select A Controller", " ", "SequentialControllerButton", "FullyAdjustableControllerButton", " ", " ", " ", " ");
                    }
                    else if (TraditionalControllerRadioButton.isSelected()) {
                        new HelpDialog(true, "Select A Controller", "Select A Controller", " ", "TraditionalControllerButton", "FullyAdjustableControllerButton", " ", " ", " ", " ");
                    }
                    else if (FullyAdjustableControllerRadioButton.isSelected()) {
                        new HelpDialog(true, "Select A Controller", "Select A Controller", " ", "FullyAdjustableControllerButton", "FullyAdjustableControllerButton", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == okButton) {
                        new HelpDialog(true, "OK button", "The OK button saves the data and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == closeButton) {
                        new HelpDialog(true, "Cancel button", "The Cancel button discards any changes and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                    }
                }
            }
        }
    } // end of HelpListener

    class OkApplyKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton) {
                    if (SequentialControllerRadioButton.isSelected()) {
                        new NemaRingGroupDialog("Sequential");
                    }
                    else if (TraditionalControllerRadioButton.isSelected()) {
                        new NemaRingGroupDialog("Traditional");
                    }
                    else if (FullyAdjustableControllerRadioButton.isSelected()) {
                        new NemaRingGroupDialog("FullyAdjustable");
                    }

                    aFrame.dispose();
                }

                if (event.getSource() == closeButton) {
                    aFrame.dispose();
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_ENTER)
        } // end of method keyPressed
    }// end of OkApplyKeyListener

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton) {
                if (SequentialControllerRadioButton.isSelected()) {
                    new NemaRingGroupDialog("Sequential");
                }
                else if (TraditionalControllerRadioButton.isSelected()) {
                    new NemaRingGroupDialog("Traditional");
                }
                else if (FullyAdjustableControllerRadioButton.isSelected()) {
                    new NemaRingGroupDialog("FullyAdjustable");
                }

                aFrame.dispose();
            }

            if (event.getSource() == closeButton) {
                aFrame.dispose();
            }
        } // end of method actionPerformed
    }// end of OkApplyActionListener

} // end of class IntersectionTypeDialog
