package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                        SketchNameListDialog.java                           */
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

class SketchNameListDialog extends JDialog {

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JLabel label_title;

    JButton cancelButton;

    JLabel[] sketchName_label = new JLabel[gdvsim.gclv_inter.msiv_num_sketches + 1];

    JButton[] sketchName_button = new JButton[gdvsim.gclv_inter.msiv_num_sketches + 1];

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    Font font;

    public SketchNameListDialog() {
        aFrame = new JFrame("Sketch Name List");

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

        font = new Font("TimesRoman", Font.BOLD, 18);

        label_title = new JLabel("Sketch Name List");
        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        for (int lsiv_i = 1; lsiv_i <= gdvsim.gclv_inter.msiv_num_sketches; lsiv_i++) {
            if (gdvsim.gclv_inter.mboa_sketch_file_OK[lsiv_i]) {
                sketchName_label[lsiv_i] = new JLabel(gdvsim.gclv_inter.msta_sketch_description[lsiv_i]);
                sketchName_button[lsiv_i] = new JButton(gdvsim.gclv_inter.msta_sketch_name[lsiv_i]);

                sketchName_button[lsiv_i].getAccessibleContext().setAccessibleName(gdvsim.gclv_inter.msta_sketch_name[lsiv_i]);
                if (gdvsim.gclv_inter.msta_sketch_help[lsiv_i].length() == 0) {
                    sketchName_button[lsiv_i].getAccessibleContext().setAccessibleDescription(gdvsim.gclv_inter.msta_sketch_description[lsiv_i]);
                }
                else {
                    sketchName_button[lsiv_i].getAccessibleContext().setAccessibleDescription(gdvsim.gclv_inter.msta_sketch_help[lsiv_i]);
                }
            }
        }

        JPanel cancel_panel = new JPanel();

        cancelButton = new JButton("Cancel");
        cancel_panel.add(cancelButton);

        cancelButton.setMnemonic(KeyEvent.VK_C);

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();

        cancelButton.addActionListener(componentActionListener);
        cancelButton.addKeyListener(componentKeyListener);

        for (int lsiv_i = 1; lsiv_i <= gdvsim.gclv_inter.msiv_num_sketches; lsiv_i++) {
            sketchName_button[lsiv_i].addActionListener(componentActionListener);
            sketchName_button[lsiv_i].addKeyListener(componentKeyListener);
        }

        int iRow = 0;

        gbConstraints.insets = new Insets(1, 1, 1, 1);
        addComponent(panel_title, iRow++, 0, 2, 1);

        gbConstraints.insets = new Insets(1, 3, 1, 3);
        for (int lsiv_i = 1; lsiv_i <= gdvsim.gclv_inter.msiv_num_sketches; lsiv_i++) {
            addComponent(sketchName_button[lsiv_i], iRow, 0, 1, 1);
            addComponent(sketchName_label[lsiv_i], iRow++, 1, 1, 1);
        }
        gbConstraints.insets = new Insets(1, 1, 1, 1);
        addComponent(cancel_panel, iRow++, 0, 2, 1);

        aFrame.setSize(950, 720);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method SketchNameList

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            for (int lsiv_i = 1; lsiv_i <= gdvsim.gclv_inter.msiv_num_sketches; lsiv_i++) {
                if (event.getSource() == sketchName_button[lsiv_i]) {
                    new SketchFileDialog(lsiv_i);
                    aFrame.dispose();
                }
            }

            if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

        } // end of method actionPerformed
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_i = 1; lsiv_i <= gdvsim.gclv_inter.msiv_num_sketches; lsiv_i++) {
                    if (event.getSource() == sketchName_button[lsiv_i]) {
                        new SketchFileDialog(lsiv_i);
                        aFrame.dispose();
                    }
                }

                if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
            }

            if (event.getKeyCode() == KeyEvent.VK_F1) {
                for (int lsiv_i = 1; lsiv_i <= gdvsim.gclv_inter.msiv_num_sketches; lsiv_i++) {
                    if (event.getSource() == sketchName_button[lsiv_i]) {
                        new HelpDialog(true, gdvsim.gclv_inter.msta_sketch_name[lsiv_i], gdvsim.gclv_inter.msta_sketch_description[lsiv_i], gdvsim.gclv_inter.msta_sketch_help[lsiv_i], " ", " ", " ",
                                " ", " ", " ");
                    }
                }

                if (event.getSource() == cancelButton) {
                    new HelpDialog(true, "Cancel button", "The Cancel button discards any changes and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
            }
        }// end of keyPressed
    }// end of class ComponentKeyListener

} // end of class SketchNameList