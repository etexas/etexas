package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                           IconListDialog.java                              */
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
import java.net.*;

class IconListDialog extends JDialog {

    int NUMOFICON = 39;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    JLabel label_title;

    JButton cancelButton;

    JLabel[] label_icon = new JLabel[NUMOFICON + 1];

    JButton[] button_icon = new JButton[NUMOFICON + 1];

    String[] imgName = new String[NUMOFICON + 1];

    ImageIcon[] imgIcon = new ImageIcon[NUMOFICON + 1];

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    Font font;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    JComboBox paramCombox = new JComboBox();

    JButton paramButton = new JButton();

    int paramVehNum;

    public IconListDialog(JComboBox pCombox, JButton pButton, int pVehNum) {
        paramCombox = pCombox;
        paramButton = pButton;
        paramVehNum = pVehNum;

        titleString = "Vehicle Classification";
        aFrame = new JFrame(titleString);

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

        font = new Font("TimesRoman", Font.BOLD, 16);

        label_title = new JLabel(titleString, JLabel.LEFT);
        label_title.setFont(font);

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY];
        String[] allImgs = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + PARAMS.TEXAS_MODEL_NVCD + 1].substring(1).split("\\|");
        URL urlUnknown = getClass().getResource("vehicle_class_unknown.jpg");
        for (int lsiv_icon = 1; lsiv_icon <= NUMOFICON; lsiv_icon++) {
            imgName[lsiv_icon] = allImgs[lsiv_icon - 1];
            URL url = getClass().getResource("vehicle_class_" + imgName[lsiv_icon] + ".jpg");
            if (url == null)
                url = urlUnknown;
            Image img = Toolkit.getDefaultToolkit().createImage(url);
            imgIcon[lsiv_icon] = new ImageIcon(img);
        }

        for (int lsiv_i = 1; lsiv_i <= NUMOFICON; lsiv_i++) {
            button_icon[lsiv_i] = new JButton(imgName[lsiv_i]);
            label_icon[lsiv_i] = new JLabel(imgIcon[lsiv_i], JLabel.LEFT);

            button_icon[lsiv_i].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + paramVehNum]);
            button_icon[lsiv_i].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + paramVehNum]);
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

        for (int lsiv_i = 1; lsiv_i <= NUMOFICON; lsiv_i++) {
            button_icon[lsiv_i].addActionListener(componentActionListener);
            button_icon[lsiv_i].addKeyListener(componentKeyListener);
        }

        int iRow = 0;
        int iCol = 3;
        int start = 0;

        gbConstraints.insets = new Insets(1, 20, 1, 1);
        addComponent(label_title, iRow++, 0, iCol, 1);

        gbConstraints.insets = new Insets(1, 5, 1, 5);
        for (int lsiv_i = 1; lsiv_i <= NUMOFICON; lsiv_i++) {
            if ((paramVehNum <= PARAMS.TEXAS_MODEL_NVCD) && (!imgName[lsiv_i].equals(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + paramVehNum]))) {
                continue;
            }
            addComponent(button_icon[lsiv_i], iRow, 0, 1, 1);
            addComponent(label_icon[lsiv_i], iRow++, 1, 1, 1);
        }

        gbConstraints.insets = new Insets(6, 1, 1, 1);
        addComponent(cancel_panel, iRow++, 0, iCol, 1);

        aFrame.setSize(750, 620);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method IconNameList

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
            for (int lsiv_i = 1; lsiv_i <= NUMOFICON; lsiv_i++) {
                if (event.getSource() == button_icon[lsiv_i]) {
                    if (paramVehNum > PARAMS.TEXAS_MODEL_NVCD) {
                        paramCombox.setSelectedItem(button_icon[lsiv_i].getText());
                        paramButton.setIcon(imgIcon[lsiv_i]);
                    }
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
                for (int lsiv_i = 1; lsiv_i <= NUMOFICON; lsiv_i++) {
                    if (event.getSource() == button_icon[lsiv_i]) {
                        if (paramVehNum > PARAMS.TEXAS_MODEL_NVCD) {
                            paramCombox.setSelectedItem(button_icon[lsiv_i].getText());
                            paramButton.setIcon(imgIcon[lsiv_i]);
                        }
                        aFrame.dispose();
                    }
                }

                if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
            }

            if (event.getKeyCode() == KeyEvent.VK_F1) {
                for (int lsiv_i = 1; lsiv_i <= NUMOFICON; lsiv_i++) {
                    if (event.getSource() == button_icon[lsiv_i]) {
                        new HelpDialog(true, button_icon[lsiv_i].getText(), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + paramVehNum],
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + paramVehNum], " ", " ", " ", " ", " ", " ");
                    }
                }

                if (event.getSource() == cancelButton) {
                    new HelpDialog(true, "Cancel button", "The Cancel button discards any changes and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
            }
        }// end of keyPressed
    }// end of class ComponentKeyListener

} // end of class IconNameList