package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                          NemaRingGroupDialog.java                          */
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

class NemaRingGroupDialog extends JDialog {

    JFrame aFrame;

    JFrame parentFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstr;

    JLabel label_title, label_ttlph, label_nring, label_ngrup;

    JPanel panel_title, panel_ttlph, panel_numrg, panel_mains;

    JComboBox cboBx_rings, cboBx_group;

    JButton okButton, applyButton, cancelButton;

    JLabel[] label_rings = new JLabel[PARAMS.TEXAS_MODEL_NRG + 1];

    JLabel[] label_group = new JLabel[PARAMS.TEXAS_MODEL_NRG + 1];

    JLabel[][][] label_phase = new JLabel[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRGP + 1];

    JPanel[] panel_rings = new JPanel[PARAMS.TEXAS_MODEL_NRG + 1];

    JPanel[] panel_group = new JPanel[PARAMS.TEXAS_MODEL_NRG + 1];

    JPanel[][] panel_phase = new JPanel[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1];

    JComboBox[][][] cboBx_phase = new JComboBox[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRGP + 1];

    int[][] default_nOfPh = new int[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1];

    int[][][] default_phase = new int[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRGP + 1];

    String[] array_rings = new String[PARAMS.TEXAS_MODEL_NRG];

    String[] array_group = new String[PARAMS.TEXAS_MODEL_NRG];

    String[][][][] array_phase = new String[PARAMS.TEXAS_MODEL_NRG][PARAMS.TEXAS_MODEL_NRG][PARAMS.TEXAS_MODEL_NRGP][PARAMS.TEXAS_MODEL_NRGP + 1];

    TX_Fmt lclv_tx_fmt;

    String controller, titleString;

    int number_of_phases, number_of_rings, number_of_groups, maxPhase;

    Font font;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    public NemaRingGroupDialog(String c) {
        controller = c;

        number_of_phases = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases;
        if (number_of_phases < 2)
            number_of_phases = 2;

        if (controller.equals("Sequential")) {
            number_of_rings = 1;
            number_of_groups = 1;
        }
        else if (controller.equals("Traditional")) {
            number_of_rings = 2;
            number_of_groups = 2;
        }
        else {
            if (gdvsim.flag_nemaRingGroup_ok) {
                number_of_rings = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_rings;
                number_of_groups = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_groups;
            }
            else {
                number_of_rings = 2;
                number_of_groups = 4;
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP];

        titleString = lclv_tx_fmt.mstv_name;

        aFrame = new JFrame(titleString);

        JPanel wholePanel = new JPanel();
        container = wholePanel;

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        aFrame.getContentPane().add(scrollpane);

        gbLayout = new GridBagLayout();
        container.setLayout(gbLayout);
        gbConstr = new GridBagConstraints();

        gbConstr.fill = GridBagConstraints.BOTH;

        label_title = new JLabel(titleString);

        label_ngrup = new JLabel("Number of Group");
        label_nring = new JLabel("Number of Ring");

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            label_rings[lsiv_ring] = new JLabel("Ring " + lsiv_ring);
        }

        for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
            label_group[lsiv_group] = new JLabel("Group " + lsiv_group);
        }

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    label_phase[lsiv_ring][lsiv_group][lsiv_phase] = new JLabel("Phase " + lsiv_phase);
                }
            }
        }

        panel_title = new JPanel();

        panel_numrg = new JPanel();

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            panel_rings[lsiv_ring] = new JPanel();
        }

        for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
            panel_group[lsiv_group] = new JPanel();
        }

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                panel_phase[lsiv_ring][lsiv_group] = new JPanel();
            }
        }

        panel_mains = new JPanel();

        if (controller.equals("Sequential")) {
            array_rings = new String[1];
            array_rings[0] = "1";

            array_group = new String[1];
            array_group[0] = "1";
        }
        else if (controller.equals("Traditional")) {
            array_rings = new String[1];
            array_rings[0] = "2";

            array_group = new String[1];
            array_group[0] = "2";
        }
        else {
            array_rings = new String[PARAMS.TEXAS_MODEL_NRG];
            for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
                array_rings[lsiv_ring - 1] = Integer.toString(lsiv_ring);
            }

            array_group = new String[PARAMS.TEXAS_MODEL_NRG];
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                array_group[lsiv_group - 1] = Integer.toString(lsiv_group);
            }
        }

        array_phase = new String[PARAMS.TEXAS_MODEL_NRG][PARAMS.TEXAS_MODEL_NRG][PARAMS.TEXAS_MODEL_NRGP][number_of_phases + 1];

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    array_phase[lsiv_ring - 1][lsiv_group - 1][lsiv_phase - 1][0] = "0";

                    for (int lsiv_phase_list = 1; lsiv_phase_list <= number_of_phases; lsiv_phase_list++) {
                        array_phase[lsiv_ring - 1][lsiv_group - 1][lsiv_phase - 1][lsiv_phase_list] = Integer.toString(lsiv_phase_list);
                    }
                }
            }
        }

        cboBx_rings = new JComboBox(array_rings);
        cboBx_group = new JComboBox(array_group);
        cboBx_rings.setSelectedItem(Integer.toString(number_of_rings));
        cboBx_group.setSelectedItem(Integer.toString(number_of_groups));

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase] = new JComboBox(array_phase[lsiv_ring - 1][lsiv_group - 1][lsiv_phase - 1]);
                }
            }
        }

        panel_numrg.setBorder(BorderFactory.createTitledBorder(""));

        GridBagLayout gbLayoutNumOfRingGroup = new GridBagLayout();
        GridBagConstraints gbConstrNumOfRingGroup = new GridBagConstraints();

        panel_numrg.setLayout(gbLayoutNumOfRingGroup);

        gbConstrNumOfRingGroup.fill = GridBagConstraints.BOTH;
        gbConstrNumOfRingGroup.insets = new Insets(1, 1, 1, 1);
        gbConstrNumOfRingGroup.gridx = 1;
        gbConstrNumOfRingGroup.gridy = 1;
        gbConstrNumOfRingGroup.gridwidth = 1;
        gbConstrNumOfRingGroup.gridheight = 1;
        gbLayoutNumOfRingGroup.setConstraints(label_nring, gbConstrNumOfRingGroup);
        panel_numrg.add(label_nring);

        gbConstrNumOfRingGroup.fill = GridBagConstraints.BOTH;
        gbConstrNumOfRingGroup.insets = new Insets(1, 1, 1, 1);
        gbConstrNumOfRingGroup.gridx = 2;
        gbConstrNumOfRingGroup.gridy = 1;
        gbConstrNumOfRingGroup.gridwidth = 1;
        gbConstrNumOfRingGroup.gridheight = 1;
        gbLayoutNumOfRingGroup.setConstraints(cboBx_rings, gbConstrNumOfRingGroup);
        panel_numrg.add(cboBx_rings);

        gbConstrNumOfRingGroup.fill = GridBagConstraints.BOTH;
        gbConstrNumOfRingGroup.insets = new Insets(1, 1, 1, 1);
        gbConstrNumOfRingGroup.gridx = 1;
        gbConstrNumOfRingGroup.gridy = 2;
        gbConstrNumOfRingGroup.gridwidth = 1;
        gbConstrNumOfRingGroup.gridheight = 1;
        gbLayoutNumOfRingGroup.setConstraints(label_ngrup, gbConstrNumOfRingGroup);
        panel_numrg.add(label_ngrup);

        gbConstrNumOfRingGroup.fill = GridBagConstraints.BOTH;
        gbConstrNumOfRingGroup.insets = new Insets(1, 1, 1, 1);
        gbConstrNumOfRingGroup.gridx = 2;
        gbConstrNumOfRingGroup.gridy = 2;
        gbConstrNumOfRingGroup.gridwidth = 1;
        gbConstrNumOfRingGroup.gridheight = 1;
        gbLayoutNumOfRingGroup.setConstraints(cboBx_group, gbConstrNumOfRingGroup);
        panel_numrg.add(cboBx_group);

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            panel_rings[lsiv_ring] = new JPanel();
            panel_rings[lsiv_ring].add(label_rings[lsiv_ring]);
            panel_rings[lsiv_ring].setBorder(BorderFactory.createTitledBorder(""));
        }

        for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
            panel_group[lsiv_group] = new JPanel();
            panel_group[lsiv_group].add(label_group[lsiv_group]);
            panel_group[lsiv_group].setBorder(BorderFactory.createTitledBorder(""));
        }

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                panel_phase[lsiv_ring][lsiv_group].setBorder(BorderFactory.createTitledBorder(""));

                GridBagLayout gbLayoutRingGroup = new GridBagLayout();
                GridBagConstraints gbConstrRingGroup = new GridBagConstraints();

                panel_phase[lsiv_ring][lsiv_group].setLayout(gbLayoutRingGroup);

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    gbConstrRingGroup.fill = GridBagConstraints.BOTH;
                    gbConstrRingGroup.insets = new Insets(1, 1, 1, 1);
                    gbConstrRingGroup.gridx = lsiv_phase;
                    gbConstrRingGroup.gridy = 1;
                    gbConstrRingGroup.gridwidth = 1;
                    gbConstrRingGroup.gridheight = 1;
                    gbLayoutRingGroup.setConstraints(label_phase[lsiv_ring][lsiv_group][lsiv_phase], gbConstrRingGroup);
                    panel_phase[lsiv_ring][lsiv_group].add(label_phase[lsiv_ring][lsiv_group][lsiv_phase]);

                    gbConstrRingGroup.fill = GridBagConstraints.BOTH;
                    gbConstrRingGroup.insets = new Insets(1, 1, 1, 1);
                    gbConstrRingGroup.gridx = lsiv_phase;
                    gbConstrRingGroup.gridy = 2;
                    gbConstrRingGroup.gridwidth = 1;
                    gbConstrRingGroup.gridheight = 1;
                    gbLayoutRingGroup.setConstraints(cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase], gbConstrRingGroup);
                    panel_phase[lsiv_ring][lsiv_group].add(cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase]);
                }
            }
        }

        panel_mains.setBorder(BorderFactory.createTitledBorder(""));

        GridBagLayout gbLayoutMains = new GridBagLayout();
        GridBagConstraints gbConstrMains = new GridBagConstraints();

        panel_mains.setLayout(gbLayoutMains);

        for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
            gbConstrMains.fill = GridBagConstraints.BOTH;
            gbConstrMains.insets = new Insets(1, 1, 1, 1);
            gbConstrMains.gridx = 1 + lsiv_group;
            gbConstrMains.gridy = 1;
            gbConstrMains.gridwidth = 1;
            gbConstrMains.gridheight = 1;
            gbLayoutMains.setConstraints(panel_group[lsiv_group], gbConstrMains);
            panel_mains.add(panel_group[lsiv_group]);
        }

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            gbConstrMains.fill = GridBagConstraints.BOTH;
            gbConstrMains.insets = new Insets(1, 1, 1, 1);
            gbConstrMains.gridx = 1;
            gbConstrMains.gridy = 1 + lsiv_ring;
            gbConstrMains.gridwidth = 1;
            gbConstrMains.gridheight = 1;
            gbLayoutMains.setConstraints(panel_rings[lsiv_ring], gbConstrMains);
            panel_mains.add(panel_rings[lsiv_ring]);
        }

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                gbConstrMains.fill = GridBagConstraints.BOTH;
                gbConstrMains.insets = new Insets(1, 1, 1, 1);
                gbConstrMains.gridx = 1 + lsiv_group;
                gbConstrMains.gridy = 1 + lsiv_ring;
                gbConstrMains.gridwidth = 1;
                gbConstrMains.gridheight = 1;
                gbLayoutMains.setConstraints(panel_phase[lsiv_ring][lsiv_group], gbConstrMains);
                panel_mains.add(panel_phase[lsiv_ring][lsiv_group]);
            }
        }

        okButton = new JButton("  OK  ");
        applyButton = new JButton(" Apply");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        cboBx_rings.getAccessibleContext().setAccessibleName("Number of rings");
        cboBx_rings.getAccessibleContext().setAccessibleDescription("Number of rings");

        cboBx_group.getAccessibleContext().setAccessibleName("Number of groups");
        cboBx_group.getAccessibleContext().setAccessibleDescription("Number of groups");

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getAccessibleContext().setAccessibleName("ring " + lsiv_ring + " group " + lsiv_group + " phase " + lsiv_phase);
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getAccessibleContext().setAccessibleDescription("ring " + lsiv_ring + " group " + lsiv_group + " phase " + lsiv_phase);
                }
            }
        }

        JPanel ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        panel_title = new JPanel();
        font = new Font("TimesRoman", Font.BOLD, 18);
        label_title.setFont(font);
        panel_title.add(label_title);

        panel_ttlph = new JPanel();
        label_ttlph = new JLabel("Total Phases = " + number_of_phases);
        panel_ttlph.add(label_ttlph);

        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();
        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();

        calculateMaxPhase();

        if (maxPhase < 1) {
            setRingGroupUnvisible();
        }
        else {
            setRingGroupVisible();

            if (gdvsim.flag_nemaRingGroup_ok) {
                if (controller.equals("Sequential")) {
                    if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_rings == 1)
                            && (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_groups == 1)) {
                        setValue();
                    }
                    else {
                        setDefaultValue();
                    }
                }
                else if (controller.equals("Traditional")) {
                    if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_rings == 2)
                            && (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_groups == 2)) {
                        setValue();
                    }
                    else {
                        setDefaultValue();
                    }
                }
                else {
                    setValue();
                }
            }
            else {
                setDefaultValue();
            }
        }

        cboBx_rings.addActionListener(componentActionListener);
        cboBx_rings.addKeyListener(componentKeyListener);
        cboBx_rings.addKeyListener(openComboMenuListener);
        cboBx_rings.addKeyListener(helpListener);

        cboBx_group.addActionListener(componentActionListener);
        cboBx_group.addKeyListener(componentKeyListener);
        cboBx_group.addKeyListener(openComboMenuListener);
        cboBx_group.addKeyListener(helpListener);

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].addActionListener(componentActionListener);
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].addKeyListener(componentKeyListener);
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].addKeyListener(openComboMenuListener);
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].addKeyListener(helpListener);
                }
            }
        }

        okButton.addActionListener(componentActionListener);
        applyButton.addActionListener(componentActionListener);
        cancelButton.addActionListener(componentActionListener);

        okButton.addKeyListener(componentKeyListener);
        applyButton.addKeyListener(componentKeyListener);
        cancelButton.addKeyListener(componentKeyListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        gbConstr.insets = new Insets(1, 1, 1, 1);

        int iRow = 0;

        addComponent(panel_title, iRow++, 0, 1, 1);
        addComponent(panel_ttlph, iRow++, 0, 1, 1);
        addComponent(panel_numrg, iRow++, 0, 1, 1);
        addComponent(panel_mains, iRow++, 0, 1, 1);
        addComponent(ok_panel, iRow++, 0, 1, 1);

        aFrame.setSize(1000, 730);
        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method NemaRingGroup

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstr.gridx = column;
        gbConstr.gridy = row;

        gbConstr.gridwidth = width;
        gbConstr.gridheight = height;

        gbLayout.setConstraints(c, gbConstr);
        container.add(c);

    } // end of method addComponent

    class OpenComboMenuListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            JComboBox cb = (JComboBox)event.getSource();

            if (event.getKeyCode() == KeyEvent.VK_UP) {
                if (cb.isPopupVisible()) {}
                else {
                    if (event.getModifiers() != InputEvent.ALT_MASK) {
                        if (cb.getSelectedIndex() == 0) {
                            cb.setSelectedIndex(cb.getSelectedIndex());
                        }
                        else {
                            cb.setSelectedIndex(cb.getSelectedIndex() - 1);
                        }
                    }
                    else {
                        cb.setSelectedIndex(cb.getSelectedIndex());
                    }
                }
            }

            if (event.getKeyCode() == KeyEvent.VK_DOWN) {
                event.consume();
                if (cb.isPopupVisible()) {
                    if (event.getModifiers() == InputEvent.ALT_MASK) {
                        cb.hidePopup();
                    }
                    else {
                        if (cb.getSelectedIndex() == (cb.getItemCount() - 1)) {
                            cb.setSelectedIndex(cb.getSelectedIndex());
                        }
                        else {
                            cb.setSelectedIndex(cb.getSelectedIndex() + 1);
                        }
                    }
                }
                else {
                    if (event.getModifiers() != InputEvent.ALT_MASK) {
                        if (cb.getSelectedIndex() == (cb.getItemCount() - 1)) {
                            cb.setSelectedIndex(cb.getSelectedIndex());
                        }
                        else {
                            cb.setSelectedIndex(cb.getSelectedIndex() + 1);
                        }
                    }
                    else {
                        cb.showPopup();
                    }
                }
            }
        }
    } // end of OpenComboMenuListener

    void setRingGroupUnvisible() {
        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            panel_rings[lsiv_ring].setVisible(false);
        }

        for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
            panel_group[lsiv_group].setVisible(false);
        }

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                panel_phase[lsiv_ring][lsiv_group].setVisible(false);

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                    label_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                }
            }
        }
    } // end of method setRingGroupUnvisible

    void setRingGroupVisible() {
        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setSelectedItem("0");
                }
            }
        }

        for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
            panel_rings[lsiv_ring].setVisible(true);
        }

        for (int lsiv_ring = number_of_rings + 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            panel_rings[lsiv_ring].setVisible(false);
        }

        for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
            panel_group[lsiv_group].setVisible(true);
        }

        for (int lsiv_group = number_of_groups + 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
            panel_group[lsiv_group].setVisible(false);
        }

        for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
                panel_phase[lsiv_ring][lsiv_group].setVisible(true);
            }

            for (int lsiv_group = number_of_groups + 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                panel_phase[lsiv_ring][lsiv_group].setVisible(false);
            }
        }

        for (int lsiv_ring = number_of_rings + 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                panel_phase[lsiv_ring][lsiv_group].setVisible(false);
            }
        }

        for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(true);
                    label_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(true);
                }

                for (int lsiv_phase = number_of_phases + 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                    label_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                }
            }

            for (int lsiv_group = number_of_groups + 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                    label_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                }
            }
        }

        for (int lsiv_ring = number_of_rings + 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                    label_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                }
            }
        }
    } // end of method setRingGroupVisible

    void setDefaultValue() {
        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                default_nOfPh[lsiv_ring][lsiv_group] = 0;
            }
        }

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    default_phase[lsiv_ring][lsiv_group][lsiv_phase] = 0;
                }
            }
        }

        int val = 0;

        while (val < number_of_phases) {
            for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
                    default_nOfPh[lsiv_ring][lsiv_group]++;
                    val++;
                    if (val == number_of_phases)
                        break;
                }

                if (val == number_of_phases)
                    break;
            }
        }

        val = 0;
        int phaseIndex = 1;

        while (val < number_of_phases) {
            for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
                    if (phaseIndex <= default_nOfPh[lsiv_ring][lsiv_group]) {
                        val++;
                        default_phase[lsiv_ring][lsiv_group][phaseIndex] = val;
                    }

                    if (phaseIndex + 1 <= default_nOfPh[lsiv_ring][lsiv_group]) {
                        val++;
                        default_phase[lsiv_ring][lsiv_group][phaseIndex + 1] = val;
                    }
                }
            }

            phaseIndex = phaseIndex + 2;
        }

        if ((number_of_groups == 4) && (number_of_rings == 2) && (number_of_phases == 16)) {
            default_phase[1][1][1] = 1;
            default_phase[1][1][2] = 2;
            default_phase[1][2][1] = 3;
            default_phase[1][2][2] = 4;
            default_phase[2][1][1] = 5;
            default_phase[2][1][2] = 6;
            default_phase[2][2][1] = 7;
            default_phase[2][2][2] = 8;
            default_phase[1][3][1] = 9;
            default_phase[1][3][2] = 10;
            default_phase[1][4][1] = 11;
            default_phase[1][4][2] = 12;
            default_phase[2][3][1] = 13;
            default_phase[2][3][2] = 14;
            default_phase[2][4][1] = 15;
            default_phase[2][4][2] = 16;
        }

        for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                    if (lsiv_phase <= (default_nOfPh[lsiv_ring][lsiv_group] + 1)) {
                        label_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(true);
                        cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(true);
                        cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setSelectedItem(Integer.toString(default_phase[lsiv_ring][lsiv_group][lsiv_phase]));
                    }
                    else {
                        label_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                        cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                    }
                }
            }
        }
    } // end of setDefaultValue

    void setValue() {
        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setSelectedItem("0");
                }
            }
        }

        for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1 - 1
                        + lsiv_group + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[lsiv_ring][lsiv_group] < number_of_phases) {
                        for (int lsiv_phase = 1; lsiv_phase <= (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[lsiv_ring][lsiv_group]
                                + 1); lsiv_phase++) {
                            label_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(true);
                            cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(true);
                            cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setSelectedItem(Integer
                                    .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[lsiv_ring][lsiv_group][lsiv_phase]));
                        }

                        for (int lsiv_phase = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[lsiv_ring][lsiv_group]
                                + 2; lsiv_phase <= number_of_phases; lsiv_phase++) {
                            label_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                            cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                        }
                    }
                    else {
                        for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1
                                    + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP
                                    + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                                label_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(true);
                                cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(true);
                                cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase]
                                        .setSelectedItem(Integer
                                                .toString(
                                                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[lsiv_ring][lsiv_group][lsiv_phase]));
                            }
                            else {
                                label_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                                cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].setVisible(false);
                            }
                        }
                    }
                } // end of
                  // if(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat
            } // end of for(int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++)
        } // end of for(int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++)
    } // end of method setValue

    void calculateMaxPhase() {
        maxPhase = number_of_phases - number_of_rings * number_of_groups + 1;

    } // end of method calculateMaxPhase

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                if (event.getSource() == okButton) {
                    new HelpDialog(true, "OK button", "The OK button saves the data and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == applyButton) {
                    new HelpDialog(true, "Apply button", "The Apply button saves the data but does not close the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == cancelButton) {
                    new HelpDialog(true, "Cancel button", "The Cancel button discards any changes and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == cboBx_rings) {
                    int min, max;

                    if (controller.equals("Sequential")) {
                        min = 1;
                        max = 1;
                    }
                    else if (controller.equals("Traditional")) {
                        min = 2;
                        max = 2;
                    }
                    else {
                        min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS];
                        max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS];
                    }

                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS], cboBx_rings
                                    .getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS]), " ", Integer.toString(min),
                            Integer.toString(max), Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS]));
                }
                else if (event.getSource() == cboBx_group) {
                    int min, max;

                    if (controller.equals("Sequential")) {
                        min = 1;
                        max = 1;
                    }
                    else if (controller.equals("Traditional")) {
                        min = 2;
                        max = 2;
                    }
                    else {
                        min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS];
                        max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS];
                    }

                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS], cboBx_group
                                    .getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS]), " ", Integer.toString(min),
                            Integer.toString(max), Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS]));
                }

                for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
                    for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                            if (event.getSource() == cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase]) {
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1 + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP + (lsiv_ring - 1)
                                        * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1
                                                + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP],
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1 + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP + (lsiv_ring - 1)
                                                * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP],
                                        cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getSelectedItem().toString(),
                                        Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1 + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP
                                                + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP]),
                                        " ",
                                        Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1 + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP
                                                + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP]),
                                        Integer.toString(number_of_phases),
                                        Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1 + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP
                                                + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP]));
                            }
                        }
                    }
                }
            }
        }
    } // end of class HelpListener

    void saveData() {
        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1 - 1 + lsiv_group
                        + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1
                            + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[lsiv_ring][lsiv_group][lsiv_phase] = 0;
                }
            }
        }

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS] = gdvsim.gclv_inter.TX_FROM_USER;

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_rings = Integer.valueOf(cboBx_rings.getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_groups = Integer.valueOf(cboBx_group.getSelectedItem().toString()).intValue();

        for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
                int numOfPhases = 0;

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    if (cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].isVisible() && (Integer.valueOf(cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getSelectedItem().toString()).intValue() != 0)) {
                        numOfPhases++;
                    }
                }

                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[lsiv_ring][lsiv_group] = numOfPhases;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1 - 1 + lsiv_group
                        + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG] = gdvsim.gclv_inter.TX_FROM_USER;

                for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                    if (cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].isVisible() && Integer.valueOf(cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getSelectedItem().toString()) != 0) {
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[lsiv_ring][lsiv_group][lsiv_phase] = Integer.valueOf(
                                cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getSelectedItem().toString()).intValue();
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1
                                + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                }
            }
        }

        gdvsim.ringGroupNumOfRingStat = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.ringGroupNumOfGroupStat = gdvsim.gclv_inter.TX_FROM_USER;

        gdvsim.ringGroupNumOfRingValue = Integer.valueOf(cboBx_rings.getSelectedItem().toString()).intValue();
        gdvsim.ringGroupNumOfGroupValue = Integer.valueOf(cboBx_group.getSelectedItem().toString()).intValue();

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                gdvsim.ringGroupNumOfPhasesStat[lsiv_ring][lsiv_group] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    gdvsim.ringGroupStat[lsiv_ring][lsiv_group][lsiv_phase] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }

        for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
                int numOfPhasesPerGroupPerRing = 0;

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    if (cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].isVisible() && Integer.valueOf(cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getSelectedItem().toString()).intValue() != 0) {
                        numOfPhasesPerGroupPerRing++;
                    }
                }

                gdvsim.ringGroupNumOfPhasesValue[lsiv_ring][lsiv_group] = numOfPhasesPerGroupPerRing;
                gdvsim.ringGroupNumOfPhasesStat[lsiv_ring][lsiv_group] = gdvsim.gclv_inter.TX_FROM_USER;

                for (int lsiv_phase = 1; lsiv_phase <= numOfPhasesPerGroupPerRing; lsiv_phase++) {
                    gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] = Integer.valueOf(cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getSelectedItem().toString()).intValue();
                    gdvsim.ringGroupStat[lsiv_ring][lsiv_group][lsiv_phase] = gdvsim.gclv_inter.TX_FROM_USER;
                }
            }
        }
    } // end of saveData()

    boolean isError() {
        int total = 0;
        int numOfPhasesPerGroupPerRing = 0;
        int numOfPhasesPerGroup = 0;

        for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
            numOfPhasesPerGroupPerRing = 0;
            numOfPhasesPerGroup = 0;

            for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    if (cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].isVisible() && Integer.valueOf(cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getSelectedItem().toString()).intValue() != 0) {
                        numOfPhasesPerGroupPerRing++;
                    }
                }
            }

            if (numOfPhasesPerGroupPerRing != 0) {
                numOfPhasesPerGroup++;
            }

            if (numOfPhasesPerGroup == 0) {
                JOptionPane.showMessageDialog(null, "The number of phases should not be equal to 0 for group " + lsiv_group + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }

        for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    if (cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].isVisible() && Integer.valueOf(cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getSelectedItem().toString()).intValue() != 0) {
                        numOfPhasesPerGroupPerRing++;
                    }
                }

                if (numOfPhasesPerGroupPerRing == 0) {
                    JOptionPane.showMessageDialog(null, "The number of phases should not be equal to 0 for ring " + lsiv_ring + " and group " + lsiv_group, "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
        }

        for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    if (cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].isVisible() && Integer.valueOf(cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getSelectedItem().toString()).intValue() != 0) {
                        total++;
                    }
                }
            }
        }

        if (total != number_of_phases) {
            JOptionPane.showMessageDialog(null, "The Total Phases should be equal to " + number_of_phases, "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
                for (int lsiv_phase = 1; lsiv_phase <= number_of_phases; lsiv_phase++) {
                    if (cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].isVisible() && Integer.valueOf(cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getSelectedItem().toString()).intValue() != 0) {
                        int checkNum = Integer.valueOf(cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getSelectedItem().toString()).intValue();
                        int repeat = 0;

                        for (int lsiv_ring_2 = 1; lsiv_ring_2 <= number_of_rings; lsiv_ring_2++) {
                            for (int lsiv_group_2 = 1; lsiv_group_2 <= number_of_groups; lsiv_group_2++) {
                                for (int lsiv_phase_2 = 1; lsiv_phase_2 <= number_of_phases; lsiv_phase_2++) {
                                    if (cboBx_phase[lsiv_ring_2][lsiv_group_2][lsiv_phase_2].isVisible()) {
                                        if (checkNum == Integer.valueOf(cboBx_phase[lsiv_ring_2][lsiv_group_2][lsiv_phase_2].getSelectedItem().toString()).intValue()) {
                                            repeat++;
                                        }

                                        if (repeat > 1) {
                                            JOptionPane.showMessageDialog(null, "Ring " + lsiv_ring + " Group " + lsiv_group + " Phase " + lsiv_phase + " should not be the same as Ring "
                                                    + lsiv_ring_2 + " Group " + lsiv_group_2 + " Phase " + lsiv_phase_2, "Error Message", JOptionPane.ERROR_MESSAGE);
                                            return true;
                                        }
                                    }
                                }
                            }
                        } // end of inner loop
                    }
                }
            }
        } // end of outer loop

        return false;

    } // end of isError()

    void NumOfRingGroupAction() {
        number_of_rings = Integer.valueOf(cboBx_rings.getSelectedItem().toString()).intValue();
        number_of_groups = Integer.valueOf(cboBx_group.getSelectedItem().toString()).intValue();

        calculateMaxPhase();

        if (maxPhase < 1) {
            setRingGroupUnvisible();
        }
        else {
            setRingGroupVisible();
            setDefaultValue();
        }
    } // end of method NumOfRingGroupAction

    void PhaseAction(int lsiv_ring, int lsiv_group, int lsiv_phase) {
        int numOfPhases = 0;
        int lsiv_phase_i = 1;

        for (lsiv_phase_i = 1; lsiv_phase_i <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase_i++) {
            if (cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase_i].isVisible()) {
                numOfPhases++;
            }
        }

        if (Integer.valueOf(cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase].getSelectedItem().toString()).intValue() == 0) {
            if (lsiv_phase < numOfPhases) {
                for (lsiv_phase_i = lsiv_phase; lsiv_phase_i < numOfPhases; lsiv_phase_i++) {
                    cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase_i].setSelectedItem(cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase_i + 1].getSelectedItem().toString());
                }

                cboBx_phase[lsiv_ring][lsiv_group][numOfPhases].setSelectedItem("0");
                cboBx_phase[lsiv_ring][lsiv_group][numOfPhases].setVisible(false);
                label_phase[lsiv_ring][lsiv_group][numOfPhases].setVisible(false);
            }
        }
        else {
            if (lsiv_phase == numOfPhases && numOfPhases < PARAMS.TEXAS_MODEL_NRGP) {
                cboBx_phase[lsiv_ring][lsiv_group][numOfPhases + 1].setVisible(true);
                label_phase[lsiv_ring][lsiv_group][numOfPhases + 1].setVisible(true);
            }
        }
    } // end of method PhaseAction

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (maxPhase < 1) {
                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
                else {
                    if (!isError()) {
                        gdvsim.flag_nemaRingGroup_ok = true;
                        saveData();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }
            else if (event.getSource() == cboBx_rings || event.getSource() == cboBx_group) {
                NumOfRingGroupAction();
            }
            else {
                for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
                    for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
                        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                            if (event.getSource() == cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase]) {
                                PhaseAction(lsiv_ring, lsiv_group, lsiv_phase);
                            }
                        }
                    }
                }
            }
        }
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    if (maxPhase < 1) {
                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                    else {
                        if (!isError()) {
                            gdvsim.flag_nemaRingGroup_ok = true;
                            saveData();

                            if (event.getSource() == okButton) {
                                aFrame.dispose();
                            }
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
                else if (event.getSource() == cboBx_rings || event.getSource() == cboBx_group) {
                    NumOfRingGroupAction();
                }
                else {
                    for (int lsiv_ring = 1; lsiv_ring <= number_of_rings; lsiv_ring++) {
                        for (int lsiv_group = 1; lsiv_group <= number_of_groups; lsiv_group++) {
                            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                                if (event.getSource() == cboBx_phase[lsiv_ring][lsiv_group][lsiv_phase]) {
                                    PhaseAction(lsiv_ring, lsiv_group, lsiv_phase);
                                }
                            } // end of for(int lsiv_phase = 1; lsiv_phase <=
                              // PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++)
                        }
                    }
                }
            }
        }
    } // end of class ComponentKeyListener

} // end of class NemaRingGroup
