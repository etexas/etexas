package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                          SketchFileDialog.java                             */
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

class SketchFileDialog extends JDialog {

    JFrame aFrame;

    Container container;

    SketchFileListener sketchFileListener;

    JLabel label_title;

    JButton okButton, cancelButton;

    int arraySize = 100;

    int index;

    int number_of_lines;

    Font font;

    ButtonListener buttonListener;

    public SketchFileDialog(int fileIndex) {
        index = fileIndex;

        aFrame = new JFrame("Sketch File " + gdvsim.gclv_inter.msta_sketch_name[index] + " Data");

        JPanel wholePanel = new JPanel();
        container = wholePanel;

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        aFrame.getContentPane().add(scrollpane);

        font = new Font("TimesRoman", Font.BOLD, 18);

        label_title = new JLabel("Sketch File " + gdvsim.gclv_inter.msta_sketch_name[index]);
        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        number_of_lines = gdvsim.gclv_inter.msia_sketch_end[index] - gdvsim.gclv_inter.msia_sketch_beg[index] + 1;

        String[] lines = new String[number_of_lines];

        for (int i = gdvsim.gclv_inter.msia_sketch_beg[index]; i <= gdvsim.gclv_inter.msia_sketch_end[index]; i++) {
            lines[i - gdvsim.gclv_inter.msia_sketch_beg[index]] = new String(gdvsim.gclv_inter.mcla_sketchCards[i].mstv_card);
        }

        DrawFile myFile = new DrawFile();
        myFile.draw(lines, number_of_lines);

        JPanel ok_panel = new JPanel();
        okButton = new JButton("  OK  ");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        ok_panel.add(okButton);
        ok_panel.add(cancelButton);

        sketchFileListener = new SketchFileListener();
        okButton.addActionListener(sketchFileListener);
        cancelButton.addActionListener(sketchFileListener);

        buttonListener = new ButtonListener();
        okButton.addKeyListener(buttonListener);
        cancelButton.addKeyListener(buttonListener);

        aFrame.getContentPane().setLayout(new BorderLayout());
        aFrame.getContentPane().add(panel_title, BorderLayout.NORTH);
        aFrame.getContentPane().add(myFile, BorderLayout.CENTER);
        aFrame.getContentPane().add(ok_panel, BorderLayout.SOUTH);

        aFrame.setSize(700, 550);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method SketchNameList

    class SketchFileListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton) {
                gdvsim.gclv_inter.filesReadGDV(gdvsim.gclv_inter.msta_sketch_file[index]);

                gdvsim.gclv_inter.mstv_gdvdataFile = "";
                gdvsim.gclv_inter.mstv_simdataFile = "";

                if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS) {
                    gdvsim.flag_library = false;
                    return;
                }

                gdvsim.currZoom = 1.0;
                gdvsim.curr_screenXShift = 0.0;
                gdvsim.curr_screenYShift = 0.0;

                // bs
                gdvsim.fileMenu.remove(gdvsim.imageMenu);
                gdvsim.bi = null;
                gdvsim.fileMenu.add(gdvsim.imageMenu, gdvsim.fileMenu.getMenuComponentCount() - 2);

                gdvsim.gclv_inter.mbov_view_detectors = true;
                gdvsim.gclv_inter.mbov_view_lane_control = true;
                gdvsim.gclv_inter.mbov_view_sdrs = true;
                gdvsim.gclv_inter.mbov_view_turn_movement = true;
                gdvsim.gclv_inter.mbov_view_user_arcs = true;
                gdvsim.gclv_inter.mbov_view_user_lines = true;

                for (int lsiv_leg = 0; lsiv_leg < 6; lsiv_leg++) {
                    gdvsim.SDR_count[lsiv_leg] = 0;
                    for (int lsiv_sdr = 0; lsiv_sdr < 4; lsiv_sdr++) {
                        gdvsim.SDR_legnumber[lsiv_leg][lsiv_sdr] = 0;
                        gdvsim.SDR_setback[lsiv_leg][lsiv_sdr] = 0;
                        gdvsim.SDR_offset[lsiv_leg][lsiv_sdr] = 0;
                    }
                }
                for (int lsiv_sdr = 1; lsiv_sdr <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs; lsiv_sdr++) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_setback == 0)
                        continue;
                    int lsiv_leg = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_leg_number;
                    int lsiv_cnt = ++gdvsim.SDR_count[lsiv_leg - 1];
                    gdvsim.SDR_legnumber[lsiv_leg - 1][lsiv_cnt - 1] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_leg_number;
                    gdvsim.SDR_setback[lsiv_leg - 1][lsiv_cnt - 1] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_setback;
                    gdvsim.SDR_offset[lsiv_leg - 1][lsiv_cnt - 1] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_offset;
                }

                gdvsim.mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gdvsim.gclv_inter.IVERSN + " - Permanent Library " + gdvsim.gclv_inter.msta_sketch_name[index]);

                gdvsim.resetGeometryGlobalValue();
                gdvsim.resetSimulationGlobalValue();
                gdvsim.gclv_inter.mstv_simdataFile = "";
                gdvsim.windowAfterOpenFile();

                gdvsim.flag_library = true;
                gdvsim.flag_OpenExistGeoFile = false;
                gdvsim.flag_OpenExistSimFile = false;
                gdvsim.flag_OpenNewFile = false;

                gdvsim.gclv_inter.mbov_GDV_Data_OK = true;
                gdvsim.gclv_inter.mbov_Diamond_Lane_OK = true;
                gdvsim.gclv_inter.mbov_Diamond_Leg_OK = true;
                gdvsim.gclv_inter.mbov_Free_UTurns_OK = true;

                gdvsim.flag_arc_ok = true;
                gdvsim.flag_line_ok = true;
                gdvsim.flag_drvVehByVeh_ok = true;
                gdvsim.flag_drvVehByDrv_ok = true;
                gdvsim.flag_plotOpt_ok = true;
                gdvsim.flag_specialVehicle_ok = true;
                gdvsim.flag_driverMixData_ok = true;
                gdvsim.flag_driverClass_ok = true;
                gdvsim.flag_vehicleClass_ok = true;

                gdvsim.flag_simulationData_ok = false;
                gdvsim.flag_greenSequence_ok = false;
                gdvsim.flag_laneControl_ok = false;
                gdvsim.flag_pretimed_ok = false;
                gdvsim.flag_semiact_ok = false;
                gdvsim.flag_fullact_ok = false;
                gdvsim.flag_nema_ok = false;

                gdvsim.flag_diamondTiming_ok = false;
                gdvsim.flag_diamondInterval_ok = false;
                gdvsim.flag_diamondOption_ok = false;

                gdvsim.flag_nemaMovement_ok = false;
                gdvsim.flag_nemaRingGroup_ok = false;
                gdvsim.flag_nemaOverlap_ok = false;

                gdvsim.flag_detDataForNonDiamond_ok = false;
                gdvsim.flag_detDataForDiamond_ok = false;
                gdvsim.flag_detDataForTexdia_ok = false;
                gdvsim.flag_detConnForNema_ok = false;
                gdvsim.flag_detConnForNonNema_ok = false;

                gdvsim.myIntersection.setVisible(true);
                gdvsim.myIntersection.draw();

                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov = 0;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases = 0;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det = 0;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_DET] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

                TX_Fmt lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_nem2_simul_gapout = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dilemma_zone_time_beg = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dilemma_zone_time_end = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_hitl_sleep_time = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] = gdvsim.gclv_inter.TX_DEFAULT;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT] = gdvsim.gclv_inter.TX_DEFAULT;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT] = gdvsim.gclv_inter.TX_DEFAULT;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] = gdvsim.gclv_inter.TX_DEFAULT;

                if (gdvsim.flag_library) {
                    gdvsim.saveMenuItem.setEnabled(false);
                }

                aFrame.dispose();
            }

            if (event.getSource() == cancelButton) {
                new SketchNameListDialog();
                aFrame.dispose();
            }

        } // end of method actionPerformed
    } // end of class SketchNameListListener

    class ButtonListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton) {
                    gdvsim.resetGeometryGlobalValue();
                    gdvsim.resetSimulationGlobalValue();
                    gdvsim.gclv_inter.mstv_simdataFile = "";
                    gdvsim.windowAfterOpenFile();

                    gdvsim.gclv_inter.filesReadGDV(gdvsim.gclv_inter.msta_sketch_file[index]);

                    gdvsim.gclv_inter.mstv_gdvdataFile = "";

                    if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS) {
                        gdvsim.flag_library = false;
                        return;
                    }

                    gdvsim.currZoom = 1.0;
                    gdvsim.curr_screenXShift = 0.0;
                    gdvsim.curr_screenYShift = 0.0;

                    // bs
                    gdvsim.fileMenu.remove(gdvsim.imageMenu);
                    gdvsim.bi = null;
                    gdvsim.fileMenu.add(gdvsim.imageMenu, gdvsim.fileMenu.getMenuComponentCount() - 2);

                    gdvsim.mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gdvsim.gclv_inter.IVERSN + " - Permanent Library " + gdvsim.gclv_inter.msta_sketch_name[index]);

                    gdvsim.flag_library = true;

                    gdvsim.myIntersection.setVisible(true);
                    gdvsim.myIntersection.draw();

                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov = 0;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases = 0;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det = 0;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_DET] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

                    if (gdvsim.flag_library) {
                        gdvsim.saveMenuItem.setEnabled(false);
                    }

                    aFrame.dispose();
                }

                if (event.getSource() == cancelButton) {
                    new SketchNameListDialog();
                    aFrame.dispose();
                }
            }
        }// end of keyPressed
    }// end of ButtonListener

    class DrawFile extends JPanel {

        String[] lines;

        int number_of_lines;

        int number_of_columns = 80;

        int start_point_x = 20;

        int start_point_y = 20;

        int distance_between_x = 8;

        int distance_between_y = 15;

        public void paintComponent(Graphics g) {
            super.paintComponent(g);

            for (int i = 0; i < number_of_lines; i++) {
                for (int j = 0; j < (number_of_columns - 1); j++) {
                    g.drawString(lines[i].substring(j, j + 1), start_point_x + distance_between_x * j, start_point_y + distance_between_y * i);
                }
            }
        }

        public void draw(String[] str, int num) {
            lines = str;
            number_of_lines = num;

            repaint();
        }
    }

} // end of class SketchNameList
