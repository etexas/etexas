package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             DetectorDialog.java                            */
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

import org.etexascode.texas.gdvsim.HelpDialog;
import org.etexascode.texas.gdvsim.gdvsim;
import java.awt.*;
import java.awt.event.*;
import java.text.DecimalFormat;
import javax.swing.*;

class DetectorDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    HelpListener helpListener;

    JButton detectorDataButton, detectorConnButton, closeButton;

    Font font;

    public DetectorDialog() {
        aFrame = new JFrame("Detectors");

        container = aFrame.getContentPane();

        JPanel wholePanel = new JPanel();
        container = wholePanel;

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        aFrame.getContentPane().add(scrollpane);

        gbLayout = new GridBagLayout();
        container.setLayout(gbLayout);
        gbConstraints = new GridBagConstraints();

        gbConstraints.fill = GridBagConstraints.BOTH;

        detectorDataButton = new JButton("Detector Data");
        detectorConnButton = new JButton("Detector Connection");

        detectorDataButton.setEnabled(gdvsim.gclv_inter.mbov_is_ic_detector_data);
        detectorConnButton.setEnabled(false);

        if (gdvsim.gclv_inter.mbov_is_ic_detector_conn) {
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det > 0) {
                detectorConnButton.setEnabled(true);
            }
        }

        closeButton = new JButton("Close");

        JPanel close_panel = new JPanel();
        close_panel.add(closeButton);

        JLabel label_title = new JLabel("Detectors", JLabel.CENTER);
        font = new Font("TimesRoman", Font.BOLD, 18);
        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        detectorDataButton.getAccessibleContext().setAccessibleName("Detector Data");
        detectorDataButton.getAccessibleContext().setAccessibleDescription("Detector Data");
        detectorConnButton.getAccessibleContext().setAccessibleName("Detector Connection");
        detectorConnButton.getAccessibleContext().setAccessibleDescription("Detector Connection");
        closeButton.getAccessibleContext().setAccessibleName("Cancel");
        closeButton.getAccessibleContext().setAccessibleDescription("Cancel");

        detectorDataButton.setMnemonic(KeyEvent.VK_D);
        detectorConnButton.setMnemonic(KeyEvent.VK_N);
        closeButton.setMnemonic(KeyEvent.VK_C);

        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();
        helpListener = new HelpListener();

        closeButton.addActionListener(componentActionListener);
        closeButton.addKeyListener(componentKeyListener);
        closeButton.addKeyListener(helpListener);

        detectorDataButton.addActionListener(componentActionListener);
        detectorDataButton.addKeyListener(componentKeyListener);
        detectorDataButton.addKeyListener(helpListener);

        detectorConnButton.addActionListener(componentActionListener);
        detectorConnButton.addKeyListener(componentKeyListener);
        detectorConnButton.addKeyListener(helpListener);

        int iRow = 0;

        gbConstraints.insets = new Insets(30, 0, 2, 0);
        addComponent(label_title, iRow++, 0, 1, 1);

        gbConstraints.insets = new Insets(2, 0, 2, 0);
        addComponent(detectorDataButton, iRow++, 0, 1, 1);
        addComponent(detectorConnButton, iRow++, 0, 1, 1);

        gbConstraints.insets = new Insets(2, 0, 2, 20);
        addComponent(close_panel, iRow++, 0, 1, 1);

        aFrame.setSize(400, 400);
        aFrame.setVisible(true);
        // aFrame.pack();
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method Detector

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                if (event.getSource() == detectorDataButton) {
                    new HelpDialog(true, "Detector Data Button", "Detector Data Button opens Detector Data Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == detectorConnButton) {
                    new HelpDialog(true, "Detector Connection Button", "Detector Connection Button opens Detector Connection Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == closeButton) {
                    new HelpDialog(true, "Cancel button", "The Close button closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
            }
        }
    } // end of class HelpListener

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == closeButton) {
                aFrame.dispose();
            }
            else if (event.getSource() == detectorDataButton) {
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                    if (gdvsim.gclv_inter.mbov_is_ic_detector_conn) {
                        new DetectorDataForDiamondDialog(detectorConnButton);
                    }
                    else if (gdvsim.gclv_inter.mbov_is_ic_TEX_DIA) {
                        new DetectorDataForTexdiaDialog();
                    }
                }
                else {
                    new DetectorDataForNonDiamondDialog(detectorConnButton);
                }
            }
            else if (event.getSource() == detectorConnButton) {
                if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT || gdvsim.gclv_inter.mbov_is_ic_FULL_ACT) {
                    new DetectorConnectionForNonNemaDialog();
                }
                else if (gdvsim.gclv_inter.mbov_is_ic_NEMA || gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
                    new DetectorConnectionForNemaDialog();
                }
            }
        } // end of method actionPerformed
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == closeButton) {
                    aFrame.dispose();
                }
                else if (event.getSource() == detectorDataButton) {
                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        if (gdvsim.gclv_inter.mbov_is_ic_detector_conn) {
                            new DetectorDataForDiamondDialog(detectorConnButton);
                        }
                        else if (gdvsim.gclv_inter.mbov_is_ic_TEX_DIA) {
                            new DetectorDataForTexdiaDialog();
                        }
                    }
                    else {
                        new DetectorDataForNonDiamondDialog(detectorConnButton);
                    }
                }
                else if (event.getSource() == detectorConnButton) {
                    if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT || gdvsim.gclv_inter.mbov_is_ic_FULL_ACT) {
                        new DetectorConnectionForNonNemaDialog();
                    }
                    else if (gdvsim.gclv_inter.mbov_is_ic_NEMA || gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
                        new DetectorConnectionForNemaDialog();
                    }
                }
            }
        } // end of keyPressed
    } // end of class ComponentKeyListener

} // end of class Detector