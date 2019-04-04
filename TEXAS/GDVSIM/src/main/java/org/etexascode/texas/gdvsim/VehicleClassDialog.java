package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                         VehicleClassDialog.java                            */
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

import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.text.DecimalFormat;
import javax.swing.*;

class VehicleClassDialog extends JDialog {

    static final int VEHICLELENGTH = 1;

    static final int VEHICLEWIDTH = 2;

    static final int VEHICLEHEIGHT = 3;

    static final int OPERATIONFACTOR = 4;

    static final int CLASSIFICATION = 5;

    static final int MAXDECELERATION = 6;

    static final int MAXACCELERATION = 7;

    static final int MAXVELOCITY = 8;

    static final int MINTURNRADIUS = 9;

    static final int NUMOFFIELD = 9;

    static final int NUMOFICON = 39;

    static final int NUMOFHEADER = (PARAMS.TEXAS_MODEL_NVC + PARAMS.TEXAS_MODEL_NVCD - 1) / PARAMS.TEXAS_MODEL_NVCD;

    int MAXOFVEHICLE = PARAMS.TEXAS_MODEL_NVC;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    JComboBox[][] comboBox_vehicle = new JComboBox[PARAMS.TEXAS_MODEL_NVC + 1][NUMOFFIELD + 1];

    JLabel[] label_veh_num = new JLabel[PARAMS.TEXAS_MODEL_NVC + 1];

    JTextArea[][] lbl_fld_header = new JTextArea[NUMOFFIELD + 1][NUMOFHEADER + 1];

    JLabel[] lbl_veh_header = new JLabel[NUMOFHEADER + 1];

    JTextArea[] lbl_unt_header = new JTextArea[NUMOFHEADER + 1];

    JButton[] add = new JButton[PARAMS.TEXAS_MODEL_NVC + 1];

    JButton[] del = new JButton[PARAMS.TEXAS_MODEL_NVC + 1];

    JButton[] up = new JButton[PARAMS.TEXAS_MODEL_NVC + 1];

    JButton[] down = new JButton[PARAMS.TEXAS_MODEL_NVC + 1];

    JButton[] icon = new JButton[PARAMS.TEXAS_MODEL_NVC + 1];

    JButton[] unit = new JButton[PARAMS.TEXAS_MODEL_NVC + 1];

    String[] imgName = new String[NUMOFICON + 1];

    ImageIcon[] imgIcon = new ImageIcon[NUMOFICON + 1];

    JButton okButton, applyButton, cancelButton;

    JComboBox cbo_total;

    JLabel lbl_fld_title, label_total;

    Font font, font1;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    int[] default_veh_unit_num = new int[PARAMS.TEXAS_MODEL_NVC + 1];

    int[][] default_veh_unit_length = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_unit_width = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_unit_draw_seq = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_unit_fpd = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_unit_rpd = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_unit_rhpd = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_trans_length = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_trans_width = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    JComboBox comboBox_number_of_vehicle_classes_saved;

    JComboBox comboBox_driver_mix_saved;

    JButton button_driver_mix_saved;

    DecimalFormat twoDigits = new DecimalFormat("0.00");

    public VehicleClassDialog(JComboBox comboBox_number_of_vehicle_classes, JComboBox comboBox_driver_mix, JButton button_driver_mix) {
        comboBox_number_of_vehicle_classes_saved = comboBox_number_of_vehicle_classes;
        comboBox_driver_mix_saved = comboBox_driver_mix;
        button_driver_mix_saved = button_driver_mix;

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01 - 1 + Math.min(lsiv_veh, PARAMS.TEXAS_MODEL_NVCD + 1)];
            default_veh_unit_num[lsiv_veh] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_NUM_UNITS];

            for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
                default_veh_unit_length[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_1 - 1 + lsiv_unit];
                default_veh_unit_width[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_1 - 1 + lsiv_unit];
                default_veh_unit_draw_seq[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_1 - 1 + lsiv_unit];
                default_veh_unit_fpd[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_1 - 1 + lsiv_unit];
                default_veh_unit_rpd[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_1 - 1 + lsiv_unit];
                default_veh_unit_rhpd[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_1 - 1 + lsiv_unit];
                default_veh_trans_length[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_1 - 1 + lsiv_unit];
                default_veh_trans_width[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_1 - 1 + lsiv_unit];
            }
        }

        int lsiv_stat;
        for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
            if (lsiv_veh <= PARAMS.TEXAS_MODEL_NVCD) {
                lsiv_stat = gdvsim.gclv_inter.TX_DEFAULT;
            }
            else {
                lsiv_stat = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }

            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_NUM_UNITS] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.veh_unit_num[lsiv_veh] = default_veh_unit_num[lsiv_veh];
                gdvsim.veh_unit_num_stat[lsiv_veh] = lsiv_stat;
            }
            else {
                gdvsim.veh_unit_num[lsiv_veh] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msiv_veh_num_units;
                gdvsim.veh_unit_num_stat[lsiv_veh] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_NUM_UNITS];
            }

            for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_1 - 1
                        + lsiv_unit] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.veh_unit_length[lsiv_veh][lsiv_unit] = default_veh_unit_length[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_length_stat[lsiv_veh][lsiv_unit] = lsiv_stat;
                }
                else {
                    gdvsim.veh_unit_length[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_length[lsiv_unit];
                    gdvsim.veh_unit_length_stat[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_1
                            - 1 + lsiv_unit];
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_1 - 1
                        + lsiv_unit] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.veh_unit_width[lsiv_veh][lsiv_unit] = default_veh_unit_width[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_width_stat[lsiv_veh][lsiv_unit] = lsiv_stat;
                }
                else {
                    gdvsim.veh_unit_width[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_width[lsiv_unit];
                    gdvsim.veh_unit_width_stat[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_1
                            - 1 + lsiv_unit];
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_1 - 1
                        + lsiv_unit] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.veh_unit_draw_seq[lsiv_veh][lsiv_unit] = default_veh_unit_draw_seq[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_draw_seq_stat[lsiv_veh][lsiv_unit] = lsiv_stat;
                }
                else {
                    gdvsim.veh_unit_draw_seq[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_draw_seq[lsiv_unit];
                    gdvsim.veh_unit_draw_seq_stat[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_1
                            - 1 + lsiv_unit];
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_1 - 1
                        + lsiv_unit] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.veh_unit_fpd[lsiv_veh][lsiv_unit] = default_veh_unit_fpd[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_fpd_stat[lsiv_veh][lsiv_unit] = lsiv_stat;
                }
                else {
                    gdvsim.veh_unit_fpd[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_fpd[lsiv_unit];
                    gdvsim.veh_unit_fpd_stat[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_1
                            - 1 + lsiv_unit];
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_1 - 1
                        + lsiv_unit] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.veh_unit_rpd[lsiv_veh][lsiv_unit] = default_veh_unit_rpd[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_rpd_stat[lsiv_veh][lsiv_unit] = lsiv_stat;
                }
                else {
                    gdvsim.veh_unit_rpd[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rpd[lsiv_unit];
                    gdvsim.veh_unit_rpd_stat[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_1
                            - 1 + lsiv_unit];
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_1 - 1
                        + lsiv_unit] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.veh_unit_rhpd[lsiv_veh][lsiv_unit] = default_veh_unit_rhpd[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_rhpd_stat[lsiv_veh][lsiv_unit] = lsiv_stat;
                }
                else {
                    gdvsim.veh_unit_rhpd[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rhpd[lsiv_unit];
                    gdvsim.veh_unit_rhpd_stat[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_1
                            - 1 + lsiv_unit];
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_1 - 1
                        + lsiv_unit] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.veh_trans_length[lsiv_veh][lsiv_unit] = default_veh_trans_length[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_draw_seq_stat[lsiv_veh][lsiv_unit] = lsiv_stat;
                }
                else {
                    gdvsim.veh_trans_length[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_length[lsiv_unit];
                    gdvsim.veh_unit_draw_seq_stat[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_1
                            - 1 + lsiv_unit];
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_1 - 1
                        + lsiv_unit] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.veh_trans_width[lsiv_veh][lsiv_unit] = default_veh_trans_width[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_draw_seq_stat[lsiv_veh][lsiv_unit] = lsiv_stat;
                }
                else {
                    gdvsim.veh_trans_width[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_width[lsiv_unit];
                    gdvsim.veh_unit_draw_seq_stat[lsiv_veh][lsiv_unit] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_1
                            - 1 + lsiv_unit];
                }
            }
        }

        int numOfVehicles = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl;
        if (gdvsim.flag_vehicleClass_ok) {
            numOfVehicles = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl;
        }
        else {
            numOfVehicles = PARAMS.TEXAS_MODEL_NVCD;
        }

        MAXOFVEHICLE = Math.min(numOfVehicles + 5, PARAMS.TEXAS_MODEL_NVC);

        titleString = "User-Defined Vehicle Data";
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

        font = new Font("TimesRoman", Font.BOLD, 18);
        font1 = new Font("TimesRoman", Font.BOLD, 14);

        lbl_fld_title = new JLabel(titleString);
        lbl_fld_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(lbl_fld_title);

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY];
        String[] allImgs = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_13].substring(1).split("\\|");
        if (allImgs.length != NUMOFICON) {
            JOptionPane.showMessageDialog(null, "allImgs.length = " + allImgs.length + " is not NUMOFICON = " + NUMOFICON + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
            return;
        }
        URL urlUnknown = getClass().getResource("vehicle_class_unknown.jpg");
        for (int lsiv_icon = 1; lsiv_icon <= NUMOFICON; lsiv_icon++) {
            imgName[lsiv_icon] = allImgs[lsiv_icon - 1];
            URL url = getClass().getResource("vehicle_class_" + imgName[lsiv_icon] + ".jpg");
            if (url == null)
                url = urlUnknown;
            Image img = Toolkit.getDefaultToolkit().createImage(url);
            imgIcon[lsiv_icon] = new ImageIcon(img);
        }

        for (int lsiv_header = 1; lsiv_header <= NUMOFHEADER; lsiv_header++) {
            lbl_veh_header[lsiv_header] = new JLabel("Vehicle Class");
            lbl_veh_header[lsiv_header].setVerticalAlignment(JLabel.TOP);
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH];
        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            String desc = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh];
            int beg = desc.indexOf('(') + 1;
            int end = desc.indexOf(')') - 1;
            label_veh_num[lsiv_veh] = new JLabel(lsiv_veh + "   " + desc.substring(beg, end + 1));
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            add[lsiv_veh] = new JButton("Add");
            del[lsiv_veh] = new JButton("Delete");
            up[lsiv_veh] = new JButton("Up");
            down[lsiv_veh] = new JButton("Down");
            unit[lsiv_veh] = new JButton("Edit");
            icon[lsiv_veh] = new JButton();
        }

        label_total = new JLabel("Number of Vehicle Classes (maximum = " + PARAMS.TEXAS_MODEL_NVC + ")");

        String desc;
        int beg;
        int end;
        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;
        int count;
        int int_number;

        for (int lsiv_header = 1; lsiv_header <= NUMOFHEADER; lsiv_header++) {
            lbl_unt_header[lsiv_header] = new JTextArea("Unit(s) Definition");
            lbl_unt_header[lsiv_header].setBackground(aFrame.getBackground());
            lbl_unt_header[lsiv_header].setEditable(false);
            lbl_unt_header[lsiv_header].setFocusable(false);
            lbl_unt_header[lsiv_header].setWrapStyleWord(true);
            lbl_unt_header[lsiv_header].setLineWrap(true);
            lbl_unt_header[lsiv_header].setFont(font1);
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH];

        desc = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01];
        beg = desc.indexOf("for") - 1;
        end = desc.indexOf(')') + 1;

        for (int lsiv_header = 1; lsiv_header <= NUMOFHEADER; lsiv_header++) {
            lbl_fld_header[VEHICLELENGTH][lsiv_header] = new JTextArea();
            lbl_fld_header[VEHICLELENGTH][lsiv_header].setBackground(aFrame.getBackground());
            lbl_fld_header[VEHICLELENGTH][lsiv_header].setEditable(false);
            lbl_fld_header[VEHICLELENGTH][lsiv_header].setFocusable(false);
            lbl_fld_header[VEHICLELENGTH][lsiv_header].setWrapStyleWord(true);
            lbl_fld_header[VEHICLELENGTH][lsiv_header].setLineWrap(true);
            lbl_fld_header[VEHICLELENGTH][lsiv_header].setFont(font1);
            lbl_fld_header[VEHICLELENGTH][lsiv_header].setText(desc.substring(0, beg) + desc.substring(end));
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_veh = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_veh[lsiv_i] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            comboBox_vehicle[lsiv_veh][VEHICLELENGTH] = new JComboBox(array_veh);

            if (gdvsim.flag_vehicleClass_ok
                    && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1
                            + lsiv_veh] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_vehicle[lsiv_veh][VEHICLELENGTH].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[lsiv_veh]));
            }
            else {
                comboBox_vehicle[lsiv_veh][VEHICLELENGTH].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh]));
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH];

        desc = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01];
        beg = desc.indexOf("for") - 1;
        end = desc.indexOf(')') + 1;

        for (int lsiv_header = 1; lsiv_header <= NUMOFHEADER; lsiv_header++) {
            lbl_fld_header[VEHICLEWIDTH][lsiv_header] = new JTextArea();
            lbl_fld_header[VEHICLEWIDTH][lsiv_header].setBackground(aFrame.getBackground());
            lbl_fld_header[VEHICLEWIDTH][lsiv_header].setEditable(false);
            lbl_fld_header[VEHICLEWIDTH][lsiv_header].setFocusable(false);
            lbl_fld_header[VEHICLEWIDTH][lsiv_header].setWrapStyleWord(true);
            lbl_fld_header[VEHICLEWIDTH][lsiv_header].setLineWrap(true);
            lbl_fld_header[VEHICLEWIDTH][lsiv_header].setFont(font1);
            lbl_fld_header[VEHICLEWIDTH][lsiv_header].setText(desc.substring(0, beg) + desc.substring(end));
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_veh = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_veh[lsiv_i] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            comboBox_vehicle[lsiv_veh][VEHICLEWIDTH] = new JComboBox(array_veh);

            if (gdvsim.flag_vehicleClass_ok
                    && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1
                            + lsiv_veh] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_vehicle[lsiv_veh][VEHICLEWIDTH].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[lsiv_veh]));
            }
            else {
                comboBox_vehicle[lsiv_veh][VEHICLEWIDTH].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh]));
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT];

        desc = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01];
        beg = desc.indexOf("for") - 1;
        end = desc.indexOf(')') + 1;

        for (int lsiv_header = 1; lsiv_header <= NUMOFHEADER; lsiv_header++) {
            lbl_fld_header[VEHICLEHEIGHT][lsiv_header] = new JTextArea();
            lbl_fld_header[VEHICLEHEIGHT][lsiv_header].setBackground(aFrame.getBackground());
            lbl_fld_header[VEHICLEHEIGHT][lsiv_header].setEditable(false);
            lbl_fld_header[VEHICLEHEIGHT][lsiv_header].setFocusable(false);
            lbl_fld_header[VEHICLEHEIGHT][lsiv_header].setWrapStyleWord(true);
            lbl_fld_header[VEHICLEHEIGHT][lsiv_header].setLineWrap(true);
            lbl_fld_header[VEHICLEHEIGHT][lsiv_header].setFont(font1);
            lbl_fld_header[VEHICLEHEIGHT][lsiv_header].setText(desc.substring(0, beg) + desc.substring(end));
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_veh = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_veh[lsiv_i] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            comboBox_vehicle[lsiv_veh][VEHICLEHEIGHT] = new JComboBox(array_veh);

            if (gdvsim.flag_vehicleClass_ok
                    && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1
                            + lsiv_veh] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_vehicle[lsiv_veh][VEHICLEHEIGHT].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[lsiv_veh]));
            }
            else {
                comboBox_vehicle[lsiv_veh][VEHICLEHEIGHT].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh]));
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR];

        desc = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01];
        beg = desc.indexOf("for") - 1;
        end = desc.indexOf(')') + 1;

        for (int lsiv_header = 1; lsiv_header <= NUMOFHEADER; lsiv_header++) {
            lbl_fld_header[OPERATIONFACTOR][lsiv_header] = new JTextArea();
            lbl_fld_header[OPERATIONFACTOR][lsiv_header].setBackground(aFrame.getBackground());
            lbl_fld_header[OPERATIONFACTOR][lsiv_header].setEditable(false);
            lbl_fld_header[OPERATIONFACTOR][lsiv_header].setFocusable(false);
            lbl_fld_header[OPERATIONFACTOR][lsiv_header].setWrapStyleWord(true);
            lbl_fld_header[OPERATIONFACTOR][lsiv_header].setLineWrap(true);
            lbl_fld_header[OPERATIONFACTOR][lsiv_header].setFont(font1);
            lbl_fld_header[OPERATIONFACTOR][lsiv_header].setText(desc.substring(0, beg) + desc.substring(end));
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_veh = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_veh[lsiv_i] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            comboBox_vehicle[lsiv_veh][OPERATIONFACTOR] = new JComboBox(array_veh);

            if (gdvsim.flag_vehicleClass_ok
                    && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1
                            + lsiv_veh] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_vehicle[lsiv_veh][OPERATIONFACTOR]
                        .setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[lsiv_veh]));
            }
            else {
                comboBox_vehicle[lsiv_veh][OPERATIONFACTOR].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh]));
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY];

        desc = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01];
        beg = desc.indexOf("for") - 1;
        end = desc.indexOf(')') + 1;

        for (int lsiv_header = 1; lsiv_header <= NUMOFHEADER; lsiv_header++) {
            lbl_fld_header[CLASSIFICATION][lsiv_header] = new JTextArea();
            lbl_fld_header[CLASSIFICATION][lsiv_header].setBackground(aFrame.getBackground());
            lbl_fld_header[CLASSIFICATION][lsiv_header].setEditable(false);
            lbl_fld_header[CLASSIFICATION][lsiv_header].setFocusable(false);
            lbl_fld_header[CLASSIFICATION][lsiv_header].setWrapStyleWord(true);
            lbl_fld_header[CLASSIFICATION][lsiv_header].setLineWrap(true);
            lbl_fld_header[CLASSIFICATION][lsiv_header].setFont(font1);
            lbl_fld_header[CLASSIFICATION][lsiv_header].setText(desc.substring(0, beg) + desc.substring(end));
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            String[] array_veh = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + lsiv_veh].substring(1).split("\\|");
            comboBox_vehicle[lsiv_veh][CLASSIFICATION] = new JComboBox(array_veh);

            if (gdvsim.flag_vehicleClass_ok
                    && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1
                            + lsiv_veh] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_vehicle[lsiv_veh][CLASSIFICATION].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[lsiv_veh]);
            }
            else {
                comboBox_vehicle[lsiv_veh][CLASSIFICATION].setSelectedItem(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + lsiv_veh]);
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL];

        desc = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01];
        beg = desc.indexOf("for") - 1;
        end = desc.indexOf(')') + 1;

        for (int lsiv_header = 1; lsiv_header <= NUMOFHEADER; lsiv_header++) {
            lbl_fld_header[MAXDECELERATION][lsiv_header] = new JTextArea();
            lbl_fld_header[MAXDECELERATION][lsiv_header].setBackground(aFrame.getBackground());
            lbl_fld_header[MAXDECELERATION][lsiv_header].setEditable(false);
            lbl_fld_header[MAXDECELERATION][lsiv_header].setFocusable(false);
            lbl_fld_header[MAXDECELERATION][lsiv_header].setWrapStyleWord(true);
            lbl_fld_header[MAXDECELERATION][lsiv_header].setLineWrap(true);
            lbl_fld_header[MAXDECELERATION][lsiv_header].setFont(font1);
            lbl_fld_header[MAXDECELERATION][lsiv_header].setText(desc.substring(0, beg) + desc.substring(end));
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_veh = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_veh[lsiv_i] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            comboBox_vehicle[lsiv_veh][MAXDECELERATION] = new JComboBox(array_veh);

            if (gdvsim.flag_vehicleClass_ok
                    && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1
                            + lsiv_veh] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_vehicle[lsiv_veh][MAXDECELERATION]
                        .setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[lsiv_veh]));
            }
            else {
                comboBox_vehicle[lsiv_veh][MAXDECELERATION].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh]));
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL];

        desc = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01];
        beg = desc.indexOf("for") - 1;
        end = desc.indexOf(')') + 1;

        for (int lsiv_header = 1; lsiv_header <= NUMOFHEADER; lsiv_header++) {
            lbl_fld_header[MAXACCELERATION][lsiv_header] = new JTextArea();
            lbl_fld_header[MAXACCELERATION][lsiv_header].setBackground(aFrame.getBackground());
            lbl_fld_header[MAXACCELERATION][lsiv_header].setEditable(false);
            lbl_fld_header[MAXACCELERATION][lsiv_header].setFocusable(false);
            lbl_fld_header[MAXACCELERATION][lsiv_header].setWrapStyleWord(true);
            lbl_fld_header[MAXACCELERATION][lsiv_header].setLineWrap(true);
            lbl_fld_header[MAXACCELERATION][lsiv_header].setFont(font1);
            lbl_fld_header[MAXACCELERATION][lsiv_header].setText(desc.substring(0, beg) + desc.substring(end));
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_veh = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_veh[lsiv_i] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            comboBox_vehicle[lsiv_veh][MAXACCELERATION] = new JComboBox(array_veh);

            if (gdvsim.flag_vehicleClass_ok
                    && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1
                            + lsiv_veh] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_vehicle[lsiv_veh][MAXACCELERATION]
                        .setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[lsiv_veh]));
            }
            else {
                comboBox_vehicle[lsiv_veh][MAXACCELERATION].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh]));
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL];

        desc = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01];
        beg = desc.indexOf("for") - 1;
        end = desc.indexOf(')') + 1;

        for (int lsiv_header = 1; lsiv_header <= NUMOFHEADER; lsiv_header++) {
            lbl_fld_header[MAXVELOCITY][lsiv_header] = new JTextArea();
            lbl_fld_header[MAXVELOCITY][lsiv_header].setBackground(aFrame.getBackground());
            lbl_fld_header[MAXVELOCITY][lsiv_header].setEditable(false);
            lbl_fld_header[MAXVELOCITY][lsiv_header].setFocusable(false);
            lbl_fld_header[MAXVELOCITY][lsiv_header].setWrapStyleWord(true);
            lbl_fld_header[MAXVELOCITY][lsiv_header].setLineWrap(true);
            lbl_fld_header[MAXVELOCITY][lsiv_header].setFont(font1);
            lbl_fld_header[MAXVELOCITY][lsiv_header].setText(desc.substring(0, beg) + desc.substring(end));
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_veh = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_veh[lsiv_i] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            comboBox_vehicle[lsiv_veh][MAXVELOCITY] = new JComboBox(array_veh);

            if (gdvsim.flag_vehicleClass_ok
                    && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1
                            + lsiv_veh] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_vehicle[lsiv_veh][MAXVELOCITY].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[lsiv_veh]));
            }
            else {
                comboBox_vehicle[lsiv_veh][MAXVELOCITY].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh]));
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD];

        desc = lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01];
        beg = desc.indexOf("for") - 1;
        end = desc.indexOf(')') + 1;

        for (int lsiv_header = 1; lsiv_header <= NUMOFHEADER; lsiv_header++) {
            lbl_fld_header[MINTURNRADIUS][lsiv_header] = new JTextArea();
            lbl_fld_header[MINTURNRADIUS][lsiv_header].setBackground(aFrame.getBackground());
            lbl_fld_header[MINTURNRADIUS][lsiv_header].setEditable(false);
            lbl_fld_header[MINTURNRADIUS][lsiv_header].setFocusable(false);
            lbl_fld_header[MINTURNRADIUS][lsiv_header].setWrapStyleWord(true);
            lbl_fld_header[MINTURNRADIUS][lsiv_header].setLineWrap(true);
            lbl_fld_header[MINTURNRADIUS][lsiv_header].setFont(font1);
            lbl_fld_header[MINTURNRADIUS][lsiv_header].setText(desc.substring(0, beg) + desc.substring(end));
        }

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh];
            lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh];
            lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_veh = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_veh[lsiv_i] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            comboBox_vehicle[lsiv_veh][MINTURNRADIUS] = new JComboBox(array_veh);

            if (gdvsim.flag_vehicleClass_ok
                    && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1
                            + lsiv_veh] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_vehicle[lsiv_veh][MINTURNRADIUS].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[lsiv_veh]));
            }
            else {
                comboBox_vehicle[lsiv_veh][MINTURNRADIUS].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh]));
            }
        }

        setStatus(numOfVehicles);

        cbo_total = new JComboBox();
        cbo_total.addItem(Integer.toString(getNumberOfVehicles()));

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            icon[lsiv_veh].setIcon(getImageIcon(comboBox_vehicle[lsiv_veh][CLASSIFICATION].getSelectedItem().toString()));
        }

        okButton = new JButton("OK");
        applyButton = new JButton("Apply");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        JPanel ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            add[lsiv_veh].addActionListener(componentActionListener);
            del[lsiv_veh].addActionListener(componentActionListener);
            up[lsiv_veh].addActionListener(componentActionListener);
            down[lsiv_veh].addActionListener(componentActionListener);
            icon[lsiv_veh].addActionListener(componentActionListener);
            unit[lsiv_veh].addActionListener(componentActionListener);

            add[lsiv_veh].addKeyListener(componentKeyListener);
            del[lsiv_veh].addKeyListener(componentKeyListener);
            up[lsiv_veh].addKeyListener(componentKeyListener);
            down[lsiv_veh].addKeyListener(componentKeyListener);
            icon[lsiv_veh].addKeyListener(componentKeyListener);
            unit[lsiv_veh].addKeyListener(componentKeyListener);

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_vehicle[lsiv_veh][lsiv_field].addKeyListener(openComboMenuListener);
                comboBox_vehicle[lsiv_veh][lsiv_field].addKeyListener(helpListener);
                if (lsiv_field == CLASSIFICATION) {
                    comboBox_vehicle[lsiv_veh][lsiv_field].addActionListener(componentActionListener);
                    comboBox_vehicle[lsiv_veh][lsiv_field].addKeyListener(componentKeyListener);
                }
            }

            add[lsiv_veh].addKeyListener(helpListener);
            del[lsiv_veh].addKeyListener(helpListener);
            up[lsiv_veh].addKeyListener(helpListener);
            down[lsiv_veh].addKeyListener(helpListener);
            icon[lsiv_veh].addKeyListener(helpListener);
            unit[lsiv_veh].addKeyListener(helpListener);
        }

        cbo_total.addKeyListener(helpListener);
        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okButton.addKeyListener(componentKeyListener);
        applyButton.addKeyListener(componentKeyListener);
        cancelButton.addKeyListener(componentKeyListener);

        okButton.addActionListener(componentActionListener);
        applyButton.addActionListener(componentActionListener);
        cancelButton.addActionListener(componentActionListener);

        setAccessiblility();
        setSize();

        int iRow = 0;
        int iCol = 5 + NUMOFFIELD + 2;
        int ic;
        int lsiv_header = 1;

        gbConstraints.insets = new Insets(1, 0, 0, 0);
        addComponent(panel_title, iRow++, 0, iCol, 1);

        gbConstraints.insets = new Insets(5, 1, 1, 1);

        addComponent(lbl_veh_header[lsiv_header], iRow, 0, 1, 1);

        ic = 5;
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == CLASSIFICATION) {
                addComponent(lbl_fld_header[lsiv_field][lsiv_header], iRow, ic++, 2, 1);
                ic++;
            }
            else {
                addComponent(lbl_fld_header[lsiv_field][lsiv_header], iRow, ic++, 1, 1);
            }
        }

        addComponent(lbl_unt_header[lsiv_header], iRow, ic, 1, 1);

        iRow++;

        gbConstraints.insets = new Insets(25, 1, 25, 1);

        for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVCD; lsiv_veh++) {
            addComponent(label_veh_num[lsiv_veh], iRow, 0, 5, 1);

            ic = 5;
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                addComponent(comboBox_vehicle[lsiv_veh][lsiv_field], iRow, ic++, 1, 1);

                if (lsiv_field == CLASSIFICATION) {
                    gbConstraints.insets = new Insets(1, 1, 1, 1);
                    addComponent(icon[lsiv_veh], iRow, ic++, 1, 1);
                    gbConstraints.insets = new Insets(25, 1, 25, 1);
                }
                else if (lsiv_field == MINTURNRADIUS) {
                    addComponent(unit[lsiv_veh], iRow, ic++, 1, 1);
                }
            }

            iRow++;
        }

        for (int lsiv_veh = PARAMS.TEXAS_MODEL_NVCD + 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            if (((int)((lsiv_veh - 1) / PARAMS.TEXAS_MODEL_NVCD) * PARAMS.TEXAS_MODEL_NVCD) == (lsiv_veh - 1)) {
                lsiv_header++;
                addComponent(lbl_veh_header[lsiv_header], iRow, 0, 1, 1);
                ic = 5;
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (lsiv_field == CLASSIFICATION) {
                        addComponent(lbl_fld_header[lsiv_field][lsiv_header], iRow, ic++, 2, 1);
                        ic++;
                    }
                    else {
                        addComponent(lbl_fld_header[lsiv_field][lsiv_header], iRow, ic++, 1, 1);
                    }
                }
                addComponent(lbl_unt_header[lsiv_header], iRow, ic, 1, 1);
                iRow++;
            }

            gbConstraints.insets = new Insets(25, 1, 25, 1);
            addComponent(label_veh_num[lsiv_veh], iRow, 0, 1, 1);
            addComponent(add[lsiv_veh], iRow, 1, 1, 1);
            addComponent(del[lsiv_veh], iRow, 2, 1, 1);
            addComponent(up[lsiv_veh], iRow, 3, 1, 1);
            addComponent(down[lsiv_veh], iRow, 4, 1, 1);

            ic = 5;
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                addComponent(comboBox_vehicle[lsiv_veh][lsiv_field], iRow, ic++, 1, 1);

                if (lsiv_field == CLASSIFICATION) {
                    gbConstraints.insets = new Insets(1, 1, 1, 1);
                    addComponent(icon[lsiv_veh], iRow, ic++, 1, 1);
                    gbConstraints.insets = new Insets(25, 1, 25, 1);
                }
                else if (lsiv_field == MINTURNRADIUS) {
                    addComponent(unit[lsiv_veh], iRow, ic++, 1, 1);
                }
            }
            iRow++;
        }

        gbConstraints.insets = new Insets(1, 1, 1, 1);
        addComponent(cbo_total, iRow, 1, 1, 1);
        addComponent(label_total, iRow++, 2, iCol - 2, 1);
        addComponent(ok_panel, iRow++, 0, iCol, 1);

        aFrame.setSize(1000, 700);
        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method VehicleClassDialog()

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    ImageIcon getImageIcon(String vehicleName) {
        ImageIcon icn = new ImageIcon();

        for (int lsiv_icon = 1; lsiv_icon <= NUMOFICON; lsiv_icon++) {
            if (vehicleName.equals(imgName[lsiv_icon])) {
                icn = imgIcon[lsiv_icon];
            }
        }
        return icn;
    } // end of method getImageIcon

    int getNumberOfVehicles() {
        int sum = 0;

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            if (comboBox_vehicle[lsiv_veh][1].isEnabled())
                sum++;
        }
        return sum;

    } // end of method getNumberOfVehicles()

    void setSize() {
        lbl_fld_header[VEHICLELENGTH][1].setPreferredSize(new Dimension(50, 60));
        lbl_fld_header[VEHICLEWIDTH][1].setPreferredSize(new Dimension(65, 60));
        lbl_fld_header[VEHICLEHEIGHT][1].setPreferredSize(new Dimension(65, 60));
        lbl_fld_header[OPERATIONFACTOR][1].setPreferredSize(new Dimension(75, 60));
        lbl_fld_header[CLASSIFICATION][1].setPreferredSize(new Dimension(500, 60));
        lbl_fld_header[MAXDECELERATION][1].setPreferredSize(new Dimension(80, 60));
        lbl_fld_header[MAXACCELERATION][1].setPreferredSize(new Dimension(80, 60));
        lbl_fld_header[MAXVELOCITY][1].setPreferredSize(new Dimension(75, 60));
        lbl_fld_header[MINTURNRADIUS][1].setPreferredSize(new Dimension(60, 60));

        lbl_fld_header[VEHICLELENGTH][1].setMaximumSize(new Dimension(50, 60));
        lbl_fld_header[VEHICLEWIDTH][1].setMaximumSize(new Dimension(65, 60));
        lbl_fld_header[VEHICLEHEIGHT][1].setMaximumSize(new Dimension(65, 60));
        lbl_fld_header[OPERATIONFACTOR][1].setMaximumSize(new Dimension(75, 60));
        lbl_fld_header[CLASSIFICATION][1].setMaximumSize(new Dimension(500, 60));
        lbl_fld_header[MAXDECELERATION][1].setMaximumSize(new Dimension(80, 60));
        lbl_fld_header[MAXACCELERATION][1].setMaximumSize(new Dimension(80, 60));
        lbl_fld_header[MAXVELOCITY][1].setMaximumSize(new Dimension(75, 60));
        lbl_fld_header[MINTURNRADIUS][1].setMaximumSize(new Dimension(60, 60));

        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            label_veh_num[lsiv_veh].setPreferredSize(new Dimension(100, 25));
            label_veh_num[lsiv_veh].setMaximumSize(new Dimension(100, 25));

            comboBox_vehicle[lsiv_veh][VEHICLELENGTH].setPreferredSize(new Dimension(50, 25));
            comboBox_vehicle[lsiv_veh][VEHICLEWIDTH].setPreferredSize(new Dimension(65, 25));
            comboBox_vehicle[lsiv_veh][VEHICLEHEIGHT].setPreferredSize(new Dimension(65, 25));
            comboBox_vehicle[lsiv_veh][OPERATIONFACTOR].setPreferredSize(new Dimension(75, 25));
            comboBox_vehicle[lsiv_veh][CLASSIFICATION].setPreferredSize(new Dimension(75, 25));
            comboBox_vehicle[lsiv_veh][MAXDECELERATION].setPreferredSize(new Dimension(80, 25));
            comboBox_vehicle[lsiv_veh][MAXACCELERATION].setPreferredSize(new Dimension(80, 25));
            comboBox_vehicle[lsiv_veh][MAXVELOCITY].setPreferredSize(new Dimension(75, 25));
            comboBox_vehicle[lsiv_veh][MINTURNRADIUS].setPreferredSize(new Dimension(60, 25));

            comboBox_vehicle[lsiv_veh][VEHICLELENGTH].setMaximumSize(new Dimension(50, 25));
            comboBox_vehicle[lsiv_veh][VEHICLEWIDTH].setMaximumSize(new Dimension(65, 25));
            comboBox_vehicle[lsiv_veh][VEHICLEHEIGHT].setMaximumSize(new Dimension(65, 25));
            comboBox_vehicle[lsiv_veh][OPERATIONFACTOR].setMaximumSize(new Dimension(75, 25));
            comboBox_vehicle[lsiv_veh][CLASSIFICATION].setMaximumSize(new Dimension(75, 25));
            comboBox_vehicle[lsiv_veh][MAXDECELERATION].setMaximumSize(new Dimension(80, 25));
            comboBox_vehicle[lsiv_veh][MAXACCELERATION].setMaximumSize(new Dimension(80, 25));
            comboBox_vehicle[lsiv_veh][MAXVELOCITY].setMaximumSize(new Dimension(75, 25));
            comboBox_vehicle[lsiv_veh][MINTURNRADIUS].setMaximumSize(new Dimension(60, 25));

            unit[lsiv_veh].setPreferredSize(new Dimension(65, 25));
            icon[lsiv_veh].setPreferredSize(new Dimension(425, 75));

            unit[lsiv_veh].setMaximumSize(new Dimension(65, 25));
            icon[lsiv_veh].setMaximumSize(new Dimension(425, 75));

            add[lsiv_veh].setPreferredSize(new Dimension(60, 25));
            del[lsiv_veh].setPreferredSize(new Dimension(75, 25));
            up[lsiv_veh].setPreferredSize(new Dimension(50, 25));
            down[lsiv_veh].setPreferredSize(new Dimension(70, 25));

            add[lsiv_veh].setMaximumSize(new Dimension(60, 25));
            del[lsiv_veh].setMaximumSize(new Dimension(75, 25));
            up[lsiv_veh].setMaximumSize(new Dimension(50, 25));
            down[lsiv_veh].setMaximumSize(new Dimension(70, 25));
        }

        okButton.setPreferredSize(new Dimension(80, 25));
        applyButton.setPreferredSize(new Dimension(80, 25));
        cancelButton.setPreferredSize(new Dimension(80, 25));

        okButton.setMaximumSize(new Dimension(80, 25));
        applyButton.setMaximumSize(new Dimension(80, 25));
        cancelButton.setMaximumSize(new Dimension(80, 25));

    } // end of method setSize()

    void setAccessiblility() {
        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH];
            comboBox_vehicle[lsiv_veh][VEHICLELENGTH].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh]);
            comboBox_vehicle[lsiv_veh][VEHICLELENGTH].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh]);

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH];
            comboBox_vehicle[lsiv_veh][VEHICLEWIDTH].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh]);
            comboBox_vehicle[lsiv_veh][VEHICLEWIDTH].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh]);

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT];
            comboBox_vehicle[lsiv_veh][VEHICLEHEIGHT].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh]);
            comboBox_vehicle[lsiv_veh][VEHICLEHEIGHT].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh]);

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR];
            comboBox_vehicle[lsiv_veh][OPERATIONFACTOR].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh]);
            comboBox_vehicle[lsiv_veh][OPERATIONFACTOR].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh]);

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY];
            comboBox_vehicle[lsiv_veh][CLASSIFICATION].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + lsiv_veh]);
            comboBox_vehicle[lsiv_veh][CLASSIFICATION].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + lsiv_veh]);

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL];
            comboBox_vehicle[lsiv_veh][MAXDECELERATION].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh]);
            comboBox_vehicle[lsiv_veh][MAXDECELERATION].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh]);

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL];
            comboBox_vehicle[lsiv_veh][MAXACCELERATION].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh]);
            comboBox_vehicle[lsiv_veh][MAXACCELERATION].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh]);

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL];
            comboBox_vehicle[lsiv_veh][MAXVELOCITY].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh]);
            comboBox_vehicle[lsiv_veh][MAXVELOCITY].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh]);

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD];
            comboBox_vehicle[lsiv_veh][MINTURNRADIUS].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh]);
            comboBox_vehicle[lsiv_veh][MINTURNRADIUS].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh]);

            add[lsiv_veh].getAccessibleContext().setAccessibleName("Add    for Vehicle Class " + lsiv_veh);
            add[lsiv_veh].getAccessibleContext().setAccessibleDescription("Add    for Vehicle Class " + lsiv_veh);
            del[lsiv_veh].getAccessibleContext().setAccessibleName("Delete for Vehicle Class " + lsiv_veh);
            del[lsiv_veh].getAccessibleContext().setAccessibleDescription("Delete for Vehicle Class " + lsiv_veh);
            up[lsiv_veh].getAccessibleContext().setAccessibleName("Up     for Vehicle Class " + lsiv_veh);
            up[lsiv_veh].getAccessibleContext().setAccessibleDescription("Up     for Vehicle Class " + lsiv_veh);
            down[lsiv_veh].getAccessibleContext().setAccessibleName("Down   for Vehicle Class " + lsiv_veh);
            down[lsiv_veh].getAccessibleContext().setAccessibleDescription("Down   for Vehicle Class " + lsiv_veh);
            icon[lsiv_veh].getAccessibleContext().setAccessibleName("icon   for Vehicle Class " + lsiv_veh);
            icon[lsiv_veh].getAccessibleContext().setAccessibleDescription("icon   for Vehicle Class " + lsiv_veh);
            unit[lsiv_veh].getAccessibleContext().setAccessibleName("unit   for Vehicle Class " + lsiv_veh);
            unit[lsiv_veh].getAccessibleContext().setAccessibleDescription("unit   for Vehicle Class " + lsiv_veh);
        }

        cbo_total.getAccessibleContext().setAccessibleName(label_total.getText());
        cbo_total.getAccessibleContext().setAccessibleDescription(label_total.getText());

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

    } // end of method setAccessiblility()

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
    } // end of class OpenComboMenuListener()

    void setStatus(int sumOfVehicles) {
        for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            if (lsiv_veh <= sumOfVehicles) {
                label_veh_num[lsiv_veh].setEnabled(true);
                icon[lsiv_veh].setEnabled(true);
                unit[lsiv_veh].setEnabled(true);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_vehicle[lsiv_veh][lsiv_field].setEnabled(true);
                }
            }
            else {
                label_veh_num[lsiv_veh].setEnabled(false);
                icon[lsiv_veh].setEnabled(false);
                unit[lsiv_veh].setEnabled(false);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_vehicle[lsiv_veh][lsiv_field].setEnabled(false);
                }
            }
        }

        if (sumOfVehicles == MAXOFVEHICLE) {
            for (int lsiv_veh = PARAMS.TEXAS_MODEL_NVCD + 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
                add[lsiv_veh].setEnabled(false);
            }
        }
        else {
            for (int lsiv_veh = PARAMS.TEXAS_MODEL_NVCD + 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
                if (lsiv_veh <= (sumOfVehicles + 1)) {
                    add[lsiv_veh].setEnabled(true);
                }
                else {
                    add[lsiv_veh].setEnabled(false);
                }
            }
        }

        for (int lsiv_veh = PARAMS.TEXAS_MODEL_NVCD + 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            if (lsiv_veh <= sumOfVehicles) {
                del[lsiv_veh].setEnabled(true);
            }
            else {
                del[lsiv_veh].setEnabled(false);
            }
        }

        for (int lsiv_veh = PARAMS.TEXAS_MODEL_NVCD + 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            if (lsiv_veh <= sumOfVehicles) {
                up[lsiv_veh].setEnabled(true);
            }
            else {
                up[lsiv_veh].setEnabled(false);
            }

            up[PARAMS.TEXAS_MODEL_NVCD + 1].setEnabled(false);
        }

        for (int lsiv_veh = PARAMS.TEXAS_MODEL_NVCD + 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
            if (lsiv_veh < sumOfVehicles) {
                down[lsiv_veh].setEnabled(true);
            }
            else {
                down[lsiv_veh].setEnabled(false);
            }
        }

        if (sumOfVehicles == PARAMS.TEXAS_MODEL_NVCD) {
            add[PARAMS.TEXAS_MODEL_NVCD + 1].requestFocus(true);
        }

    } // end of method setStatus

    void setValueAfterAdd(int sum, int index) {
        if ((sum == index - 1) && (sum != PARAMS.TEXAS_MODEL_NVCD)) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_vehicle[index][lsiv_field].setSelectedItem(comboBox_vehicle[sum][lsiv_field].getSelectedItem().toString());
            }

            icon[index].setIcon(getImageIcon(comboBox_vehicle[index][CLASSIFICATION].getSelectedItem().toString()));

            gdvsim.veh_unit_num[index] = gdvsim.veh_unit_num[sum];
            gdvsim.veh_unit_num_stat[index] = gdvsim.veh_unit_num_stat[sum];
            for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
                gdvsim.veh_unit_length[index][lsiv_unit] = gdvsim.veh_unit_length[sum][lsiv_unit];
                gdvsim.veh_unit_width[index][lsiv_unit] = gdvsim.veh_unit_width[sum][lsiv_unit];
                gdvsim.veh_unit_draw_seq[index][lsiv_unit] = gdvsim.veh_unit_draw_seq[sum][lsiv_unit];
                gdvsim.veh_unit_fpd[index][lsiv_unit] = gdvsim.veh_unit_fpd[sum][lsiv_unit];
                gdvsim.veh_unit_rpd[index][lsiv_unit] = gdvsim.veh_unit_rpd[sum][lsiv_unit];
                gdvsim.veh_unit_rhpd[index][lsiv_unit] = gdvsim.veh_unit_rhpd[sum][lsiv_unit];
                gdvsim.veh_trans_length[index][lsiv_unit] = gdvsim.veh_trans_length[sum][lsiv_unit];
                gdvsim.veh_trans_width[index][lsiv_unit] = gdvsim.veh_trans_width[sum][lsiv_unit];
                gdvsim.veh_unit_length_stat[index][lsiv_unit] = gdvsim.veh_unit_length_stat[sum][lsiv_unit];
                gdvsim.veh_unit_width_stat[index][lsiv_unit] = gdvsim.veh_unit_width_stat[sum][lsiv_unit];
                gdvsim.veh_unit_draw_seq_stat[index][lsiv_unit] = gdvsim.veh_unit_draw_seq_stat[sum][lsiv_unit];
                gdvsim.veh_unit_fpd_stat[index][lsiv_unit] = gdvsim.veh_unit_fpd_stat[sum][lsiv_unit];
                gdvsim.veh_unit_rpd_stat[index][lsiv_unit] = gdvsim.veh_unit_rpd_stat[sum][lsiv_unit];
                gdvsim.veh_unit_rhpd_stat[index][lsiv_unit] = gdvsim.veh_unit_rhpd_stat[sum][lsiv_unit];
                gdvsim.veh_trans_length_stat[index][lsiv_unit] = gdvsim.veh_trans_length_stat[sum][lsiv_unit];
                gdvsim.veh_trans_width_stat[index][lsiv_unit] = gdvsim.veh_trans_width_stat[sum][lsiv_unit];
            }
        }
        else {
            for (int lsiv_veh = sum; lsiv_veh >= index; lsiv_veh--) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_vehicle[lsiv_veh + 1][lsiv_field].setSelectedItem(comboBox_vehicle[lsiv_veh][lsiv_field].getSelectedItem().toString());
                }

                icon[lsiv_veh].setIcon(getImageIcon(comboBox_vehicle[lsiv_veh][CLASSIFICATION].getSelectedItem().toString()));

                gdvsim.veh_unit_num[lsiv_veh + 1] = gdvsim.veh_unit_num[lsiv_veh];
                gdvsim.veh_unit_num_stat[lsiv_veh + 1] = gdvsim.veh_unit_num_stat[lsiv_veh];
                for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
                    gdvsim.veh_unit_length[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_unit_length[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_width[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_unit_width[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_draw_seq[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_unit_draw_seq[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_fpd[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_unit_fpd[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_rpd[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_unit_rpd[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_rhpd[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_unit_rhpd[lsiv_veh][lsiv_unit];
                    gdvsim.veh_trans_length[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_trans_length[lsiv_veh][lsiv_unit];
                    gdvsim.veh_trans_width[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_trans_width[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_length_stat[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_unit_length_stat[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_width_stat[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_unit_width_stat[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_draw_seq_stat[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_unit_draw_seq_stat[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_fpd_stat[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_unit_fpd_stat[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_rpd_stat[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_unit_rpd_stat[lsiv_veh][lsiv_unit];
                    gdvsim.veh_unit_rhpd_stat[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_unit_rhpd_stat[lsiv_veh][lsiv_unit];
                    gdvsim.veh_trans_length_stat[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_trans_length_stat[lsiv_veh][lsiv_unit];
                    gdvsim.veh_trans_width_stat[lsiv_veh + 1][lsiv_unit] = gdvsim.veh_trans_width_stat[lsiv_veh][lsiv_unit];
                }
            }
        }
    } // end of method setValueAfterAdd()

    void setValueAfterDel(int sum, int index) {
        for (int lsiv_veh = index; lsiv_veh < sum; lsiv_veh++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_vehicle[lsiv_veh][lsiv_field].setSelectedItem(comboBox_vehicle[lsiv_veh + 1][lsiv_field].getSelectedItem().toString());
            }

            icon[lsiv_veh].setIcon(getImageIcon(comboBox_vehicle[lsiv_veh][CLASSIFICATION].getSelectedItem().toString()));

            gdvsim.veh_unit_num[lsiv_veh] = gdvsim.veh_unit_num[lsiv_veh + 1];
            gdvsim.veh_unit_num_stat[lsiv_veh] = gdvsim.veh_unit_num_stat[lsiv_veh + 1];
            for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
                gdvsim.veh_unit_length[lsiv_veh][lsiv_unit] = gdvsim.veh_unit_length[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_unit_width[lsiv_veh][lsiv_unit] = gdvsim.veh_unit_width[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_unit_draw_seq[lsiv_veh][lsiv_unit] = gdvsim.veh_unit_draw_seq[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_unit_fpd[lsiv_veh][lsiv_unit] = gdvsim.veh_unit_fpd[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_unit_rpd[lsiv_veh][lsiv_unit] = gdvsim.veh_unit_rpd[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_unit_rhpd[lsiv_veh][lsiv_unit] = gdvsim.veh_unit_rhpd[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_trans_length[lsiv_veh][lsiv_unit] = gdvsim.veh_trans_length[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_trans_width[lsiv_veh][lsiv_unit] = gdvsim.veh_trans_width[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_unit_length_stat[lsiv_veh][lsiv_unit] = gdvsim.veh_unit_length_stat[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_unit_width_stat[lsiv_veh][lsiv_unit] = gdvsim.veh_unit_width_stat[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_unit_draw_seq_stat[lsiv_veh][lsiv_unit] = gdvsim.veh_unit_draw_seq_stat[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_unit_fpd_stat[lsiv_veh][lsiv_unit] = gdvsim.veh_unit_fpd_stat[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_unit_rpd_stat[lsiv_veh][lsiv_unit] = gdvsim.veh_unit_rpd_stat[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_unit_rhpd_stat[lsiv_veh][lsiv_unit] = gdvsim.veh_unit_rhpd_stat[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_trans_length_stat[lsiv_veh][lsiv_unit] = gdvsim.veh_trans_length_stat[lsiv_veh + 1][lsiv_unit];
                gdvsim.veh_trans_width_stat[lsiv_veh][lsiv_unit] = gdvsim.veh_trans_width_stat[lsiv_veh + 1][lsiv_unit];
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH];
        comboBox_vehicle[sum][VEHICLELENGTH].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + sum]));

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH];
        comboBox_vehicle[sum][VEHICLEWIDTH].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + sum]));

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT];
        comboBox_vehicle[sum][VEHICLEHEIGHT].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + sum]));

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR];
        comboBox_vehicle[sum][OPERATIONFACTOR].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + sum]));

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY];
        comboBox_vehicle[sum][CLASSIFICATION].setSelectedItem(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + sum]);

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL];
        comboBox_vehicle[sum][MAXDECELERATION].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + sum]));

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL];
        comboBox_vehicle[sum][MAXACCELERATION].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + sum]));

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL];
        comboBox_vehicle[sum][MAXVELOCITY].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + sum]));

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD];
        comboBox_vehicle[sum][MINTURNRADIUS].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + sum]));

        icon[sum].setIcon(getImageIcon(comboBox_vehicle[sum][CLASSIFICATION].getSelectedItem().toString()));

        gdvsim.veh_unit_num[sum] = default_veh_unit_num[sum];
        gdvsim.veh_unit_num_stat[sum] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
            gdvsim.veh_unit_length[sum][lsiv_unit] = default_veh_unit_length[sum][lsiv_unit];
            gdvsim.veh_unit_width[sum][lsiv_unit] = default_veh_unit_width[sum][lsiv_unit];
            gdvsim.veh_unit_draw_seq[sum][lsiv_unit] = default_veh_unit_draw_seq[sum][lsiv_unit];
            gdvsim.veh_unit_fpd[sum][lsiv_unit] = default_veh_unit_fpd[sum][lsiv_unit];
            gdvsim.veh_unit_rpd[sum][lsiv_unit] = default_veh_unit_rpd[sum][lsiv_unit];
            gdvsim.veh_unit_rhpd[sum][lsiv_unit] = default_veh_unit_rhpd[sum][lsiv_unit];
            gdvsim.veh_trans_length[sum][lsiv_unit] = default_veh_trans_length[sum][lsiv_unit];
            gdvsim.veh_trans_width[sum][lsiv_unit] = default_veh_trans_width[sum][lsiv_unit];
            gdvsim.veh_unit_length_stat[sum][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_unit_width_stat[sum][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_unit_draw_seq_stat[sum][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_unit_fpd_stat[sum][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_unit_rpd_stat[sum][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_unit_rhpd_stat[sum][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_trans_length_stat[sum][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_trans_width_stat[sum][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    } // end of method setValueAfterDel()

    void UpAction(int lsiv_veh) {
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            String temp;
            temp = comboBox_vehicle[lsiv_veh][lsiv_field].getSelectedItem().toString();
            comboBox_vehicle[lsiv_veh][lsiv_field].setSelectedItem(comboBox_vehicle[lsiv_veh - 1][lsiv_field].getSelectedItem().toString());
            comboBox_vehicle[lsiv_veh - 1][lsiv_field].setSelectedItem(temp);
        }

        icon[lsiv_veh].setIcon(getImageIcon(comboBox_vehicle[lsiv_veh][CLASSIFICATION].getSelectedItem().toString()));
        icon[lsiv_veh - 1].setIcon(getImageIcon(comboBox_vehicle[lsiv_veh - 1][CLASSIFICATION].getSelectedItem().toString()));

        for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
            int temp1, temp2;

            temp1 = gdvsim.veh_unit_length[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_length[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_unit_length[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_length[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_width[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_width[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_unit_width[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_width[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_draw_seq[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_draw_seq[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_unit_draw_seq[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_draw_seq[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_fpd[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_fpd[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_unit_fpd[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_fpd[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_rpd[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_rpd[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_unit_rpd[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_rpd[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_rhpd[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_rhpd[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_unit_rhpd[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_rhpd[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_trans_length[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_trans_length[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_trans_length[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_trans_length[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_trans_width[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_trans_width[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_trans_width[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_trans_width[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_length_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_length_stat[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_unit_length_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_length_stat[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_width_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_width_stat[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_unit_width_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_width_stat[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_draw_seq_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_draw_seq_stat[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_unit_draw_seq_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_draw_seq_stat[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_fpd_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_fpd_stat[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_unit_fpd_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_fpd_stat[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_rpd_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_rpd_stat[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_unit_rpd_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_rpd_stat[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_rhpd_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_rhpd_stat[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_unit_rhpd_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_rhpd_stat[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_trans_length_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_trans_length_stat[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_trans_length_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_trans_length_stat[lsiv_veh - 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_trans_width_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_trans_width_stat[lsiv_veh - 1][lsiv_unit];
            gdvsim.veh_trans_width_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_trans_width_stat[lsiv_veh - 1][lsiv_unit] = temp1;
        }
    } // end of method UpAction

    void DownAction(int lsiv_veh) {
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            String temp;
            temp = comboBox_vehicle[lsiv_veh][lsiv_field].getSelectedItem().toString();
            comboBox_vehicle[lsiv_veh][lsiv_field].setSelectedItem(comboBox_vehicle[lsiv_veh + 1][lsiv_field].getSelectedItem().toString());
            comboBox_vehicle[lsiv_veh + 1][lsiv_field].setSelectedItem(temp);
        }

        icon[lsiv_veh].setIcon(getImageIcon(comboBox_vehicle[lsiv_veh][CLASSIFICATION].getSelectedItem().toString()));
        icon[lsiv_veh + 1].setIcon(getImageIcon(comboBox_vehicle[lsiv_veh + 1][CLASSIFICATION].getSelectedItem().toString()));

        int t1, t2;

        t1 = gdvsim.veh_unit_num[lsiv_veh];
        t2 = gdvsim.veh_unit_num[lsiv_veh + 1];
        gdvsim.veh_unit_num[lsiv_veh] = t2;
        gdvsim.veh_unit_num[lsiv_veh + 1] = t1;

        t1 = gdvsim.veh_unit_num_stat[lsiv_veh];
        t2 = gdvsim.veh_unit_num_stat[lsiv_veh + 1];
        gdvsim.veh_unit_num_stat[lsiv_veh] = t2;
        gdvsim.veh_unit_num_stat[lsiv_veh + 1] = t1;

        for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
            int temp1, temp2;

            temp1 = gdvsim.veh_unit_length[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_length[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_unit_length[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_length[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_width[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_width[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_unit_width[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_width[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_draw_seq[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_draw_seq[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_unit_draw_seq[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_draw_seq[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_fpd[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_fpd[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_unit_fpd[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_fpd[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_rpd[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_rpd[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_unit_rpd[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_rpd[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_rhpd[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_rhpd[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_unit_rhpd[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_rhpd[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_trans_length[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_trans_length[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_trans_length[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_trans_length[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_trans_width[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_trans_width[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_trans_width[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_trans_width[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_length_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_length_stat[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_unit_length_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_length_stat[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_width_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_width_stat[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_unit_width_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_width_stat[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_draw_seq_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_draw_seq_stat[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_unit_draw_seq_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_draw_seq_stat[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_fpd_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_fpd_stat[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_unit_fpd_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_fpd_stat[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_rpd_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_rpd_stat[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_unit_rpd_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_rpd_stat[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_unit_rhpd_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_unit_rhpd_stat[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_unit_rhpd_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_unit_rhpd_stat[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_trans_length_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_trans_length_stat[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_trans_length_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_trans_length_stat[lsiv_veh + 1][lsiv_unit] = temp1;

            temp1 = gdvsim.veh_trans_width_stat[lsiv_veh][lsiv_unit];
            temp2 = gdvsim.veh_trans_width_stat[lsiv_veh + 1][lsiv_unit];
            gdvsim.veh_trans_width_stat[lsiv_veh][lsiv_unit] = temp2;
            gdvsim.veh_trans_width_stat[lsiv_veh + 1][lsiv_unit] = temp1;
        }
    } // end of method DownAction

    void UnitAction(int lsiv_veh) {
        int veh_length = Integer.valueOf(comboBox_vehicle[lsiv_veh][VEHICLELENGTH].getSelectedItem().toString()).intValue();
        int veh_width = Integer.valueOf(comboBox_vehicle[lsiv_veh][VEHICLEWIDTH].getSelectedItem().toString()).intValue();
        String veh_classification = comboBox_vehicle[lsiv_veh][CLASSIFICATION].getSelectedItem().toString();
        ImageIcon veh_icon = (ImageIcon)icon[lsiv_veh].getIcon();

        if (lsiv_veh > PARAMS.TEXAS_MODEL_NVCD) {
            int veh_length_saved = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[lsiv_veh];
            int veh_width_saved = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[lsiv_veh];
            String veh_classification_saved = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[lsiv_veh];

            if (veh_length != veh_length_saved) {
                JOptionPane.showMessageDialog(null, "The Length for Vehicle Class " + lsiv_veh
                        + " has been modified. Please OK or Apply User-Defined Vehicle Data before accessing the Unit(s) Definition.", "Warning Message", JOptionPane.WARNING_MESSAGE);
                return;
            }
            else if (veh_width != veh_width_saved) {
                JOptionPane.showMessageDialog(null, "The Maximum Width for Vehicle Class " + lsiv_veh
                        + " has been modified. Please OK or Apply User-Defined Vehicle Data before accessing the Unit(s) Definition.", "Warning Message", JOptionPane.WARNING_MESSAGE);
                return;
            }
            else if (!veh_classification.equals(veh_classification_saved)) {
                JOptionPane.showMessageDialog(null, "The Vehicle Classification for Vehicle Class " + lsiv_veh
                        + " has been modified. Please OK or Apply User-Defined Vehicle Data before accessing the Unit(s) Definition.", "Warning Message", JOptionPane.WARNING_MESSAGE);
                return;
            }
        }

        new VehicleUnitDialog(lsiv_veh, veh_length, veh_width, veh_classification, veh_icon);

    } // end of method UnitAction

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
                else if (event.getSource() == cbo_total) {
                    new HelpDialog(true, label_total.getText(), label_total.getText(), "The user cannot specify this item.  This item is the Total Vehicles and is determined by the program.",
                            cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NVC), "1");
                }

                for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
                    if (event.getSource() == comboBox_vehicle[lsiv_veh][VEHICLELENGTH]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH];
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh], lclv_tx_fmt.mstv_name,
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh],
                                comboBox_vehicle[lsiv_veh][VEHICLELENGTH].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh]));
                        break;
                    }
                    else if (event.getSource() == comboBox_vehicle[lsiv_veh][VEHICLEWIDTH]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH];
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh], lclv_tx_fmt.mstv_name,
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh],
                                comboBox_vehicle[lsiv_veh][VEHICLEWIDTH].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh]));
                        break;
                    }
                    else if (event.getSource() == comboBox_vehicle[lsiv_veh][VEHICLEHEIGHT]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT];
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh], lclv_tx_fmt.mstv_name,
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh],
                                comboBox_vehicle[lsiv_veh][VEHICLEHEIGHT].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh]));
                        break;
                    }
                    else if (event.getSource() == comboBox_vehicle[lsiv_veh][OPERATIONFACTOR]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR];
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh], lclv_tx_fmt.mstv_name,
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh],
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh], comboBox_vehicle[lsiv_veh][OPERATIONFACTOR].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1 + lsiv_veh]));
                        break;
                    }
                    else if (event.getSource() == comboBox_vehicle[lsiv_veh][CLASSIFICATION]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY];
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + lsiv_veh], lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_veh)),
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + lsiv_veh], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + lsiv_veh],
                                comboBox_vehicle[lsiv_veh][CLASSIFICATION].getSelectedItem().toString(), lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + lsiv_veh],
                                lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + lsiv_veh], " ", " ", " ");
                        break;
                    }
                    else if (event.getSource() == comboBox_vehicle[lsiv_veh][MAXDECELERATION]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL];
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh], lclv_tx_fmt.mstv_name,
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh],
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh], comboBox_vehicle[lsiv_veh][MAXDECELERATION].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1 + lsiv_veh]));
                        break;
                    }
                    else if (event.getSource() == comboBox_vehicle[lsiv_veh][MAXACCELERATION]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL];
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh], lclv_tx_fmt.mstv_name,
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh],
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh], comboBox_vehicle[lsiv_veh][MAXACCELERATION].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1 + lsiv_veh]));
                        break;
                    }
                    else if (event.getSource() == comboBox_vehicle[lsiv_veh][MAXVELOCITY]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL];
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh], lclv_tx_fmt.mstv_name,
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh],
                                comboBox_vehicle[lsiv_veh][MAXVELOCITY].getSelectedItem().toString(),
                                Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh]));
                        break;
                    }
                    else if (event.getSource() == comboBox_vehicle[lsiv_veh][MINTURNRADIUS]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD];
                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh], lclv_tx_fmt.mstv_name,
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh],
                                comboBox_vehicle[lsiv_veh][MINTURNRADIUS].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1
                                        + lsiv_veh]),
                                " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh]));
                        break;
                    }
                    else if (event.getSource() == up[lsiv_veh]) {
                        new HelpDialog(true, "Up Button", "The Up button swaps the data for the previous vehicle class and the current vehicle class.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == down[lsiv_veh]) {
                        new HelpDialog(true, "Down Button", "The Down button swaps the data for the next vehicle class and the current vehicle class.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == del[lsiv_veh]) {
                        new HelpDialog(true, "Delete Button",
                                "The Delete button moves the data up 1 vehicle class for all vehicle classes below this vehicle class and decreases the Total Vehicle Classes by 1.", " ", " ", " ",
                                " ", " ", " ", " ");
                    }
                    else if (event.getSource() == add[lsiv_veh]) {
                        new HelpDialog(
                                true,
                                "Add Button",
                                "The Add button moves the data down 1 vehicle class for all vehicle classes below this vehicle class, inserts a new vehicle class at the current position, copies the values of all parameters from the previous vehicle class to the new vehicle class, and increases the Total Vehicle Classes by 1.",
                                " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == icon[lsiv_veh]) {
                        new HelpDialog(true, "icon Button", "The icon Button opens the icon window to allow the user to select a icon.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == unit[lsiv_veh]) {
                        new HelpDialog(true, "unit Button", "The unit Button opens the unit window to allow the user to define the unit data.", " ", " ", " ", " ", " ", " ", " ");
                    }
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_F1)
        }
    } // end of class HelpListener

    void saveData() {
        int numOfVehicles = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl = getNumberOfVehicles();

        comboBox_number_of_vehicle_classes_saved.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl));

        for (int lsiv_veh = 1; lsiv_veh <= numOfVehicles; lsiv_veh++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[lsiv_veh] = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][VEHICLELENGTH].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.msia_veh_width[lsiv_veh] = Integer.valueOf(comboBox_vehicle[lsiv_veh][VEHICLEWIDTH].getSelectedItem().toString())
                    .intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.msia_veh_height[lsiv_veh] = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][VEHICLEHEIGHT].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[lsiv_veh] = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][OPERATIONFACTOR].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.msta_veh_classification[lsiv_veh] = comboBox_vehicle[lsiv_veh][CLASSIFICATION].getSelectedItem()
                    .toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[lsiv_veh] = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][MAXDECELERATION].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[lsiv_veh] = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][MAXACCELERATION].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[lsiv_veh] = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][MAXVELOCITY].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[lsiv_veh] = Integer.valueOf(
                    comboBox_vehicle[lsiv_veh][MINTURNRADIUS].getSelectedItem().toString()).intValue();

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1 + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1 + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1 + lsiv_veh] = gdvsim.gclv_inter.TX_FROM_USER;
        } // end for lsiv_veh

        for (int lsiv_veh = numOfVehicles + 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_width.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_height.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_HEIGHT_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_classification.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        } // end for lsiv_veh

        saveUnit();
    } // end of method saveData

    void saveUnit() {
        int numOfVehicles = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl = getNumberOfVehicles();

        for (int lsiv_veh = 1; lsiv_veh <= numOfVehicles; lsiv_veh++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msiv_veh_num_units = gdvsim.veh_unit_num[lsiv_veh];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_NUM_UNITS] = gdvsim.veh_unit_num_stat[lsiv_veh];

            for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_length[lsiv_unit] = gdvsim.veh_unit_length[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_width[lsiv_unit] = gdvsim.veh_unit_width[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_draw_seq[lsiv_unit] = gdvsim.veh_unit_draw_seq[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_fpd[lsiv_unit] = gdvsim.veh_unit_fpd[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rpd[lsiv_unit] = gdvsim.veh_unit_rpd[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_unit_rhpd[lsiv_unit] = gdvsim.veh_unit_rhpd[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_length[lsiv_unit] = gdvsim.veh_trans_length[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msia_veh_trans_width[lsiv_unit] = gdvsim.veh_trans_width[lsiv_veh][lsiv_unit];

                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_1 - 1
                        + lsiv_unit] = gdvsim.veh_unit_length_stat[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_1 - 1
                        + lsiv_unit] = gdvsim.veh_unit_width_stat[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_1 - 1
                        + lsiv_unit] = gdvsim.veh_unit_draw_seq_stat[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_1 - 1
                        + lsiv_unit] = gdvsim.veh_unit_fpd_stat[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_1 - 1
                        + lsiv_unit] = gdvsim.veh_unit_rpd_stat[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_1 - 1
                        + lsiv_unit] = gdvsim.veh_unit_rhpd_stat[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_1 - 1
                        + lsiv_unit] = gdvsim.veh_trans_length_stat[lsiv_veh][lsiv_unit];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_1 - 1
                        + lsiv_unit] = gdvsim.veh_trans_width_stat[lsiv_veh][lsiv_unit];
            }
        }

        for (int lsiv_veh = numOfVehicles + 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msiv_veh_num_units = default_veh_unit_num[lsiv_veh];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_NUM_UNITS] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;

            for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_1 - 1
                        + lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_1 - 1
                        + lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_1 - 1
                        + lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_1 - 1
                        + lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_1 - 1
                        + lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_1 - 1
                        + lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_1 - 1
                        + lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_1 - 1
                        + lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }
    } // end of method saveUnit()

    void invalidDriverMixAndTrafficMix() {
        gdvsim.flag_driverMixData_ok = false;
        for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
            for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mcla_aux[lsiv_veh].msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1
                        + lsiv_drv] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        comboBox_driver_mix_saved.setSelectedItem("NO");
        button_driver_mix_saved.setEnabled(false);
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix = "NO";

        for (int lsiv_leg = 1; lsiv_leg <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs; lsiv_leg++) {
            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_INB] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                    gdvsim.gclv_inter.mboa_Leg_Traffic_Mix_OK[lsiv_leg] = false;
                    for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_mix.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1
                                + lsiv_veh] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }
        }

        gdvsim.flag_drvVehByVeh_ok = false;
        for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01 - 1
                    + lsiv_veh] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    } // end of method invalidDriverMixAndTrafficMix

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != getNumberOfVehicles()) {
                    invalidDriverMixAndTrafficMix();
                    JOptionPane
                            .showMessageDialog(
                                    null,
                                    "The Number of Vehicle Classes has changed from "
                                            + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
                                            + " to "
                                            + getNumberOfVehicles()
                                            + ".\nPlease OK or Apply User-Defined Driver Mix Data and Logout Summary for Driver-Vehicle Unit by Vehicle Class in GDV Data.\nPlease OK or Apply User-Defined Traffic Mix Data for each Leg with Inbound Lanes.\nPlease OK or Apply User-Defined Traffic Mix Data for each Leg with Varying Traffic Period Data.",
                                    "Warning Message", JOptionPane.WARNING_MESSAGE);
                }

                gdvsim.flag_vehicleClass_ok = true;
                saveData();

                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data = "YES";

                if (event.getSource() == okButton) {
                    aFrame.dispose();
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

            for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
                if (event.getSource() == add[lsiv_veh]) {
                    int numOfVehiclesBeforeClick;
                    numOfVehiclesBeforeClick = getNumberOfVehicles();
                    setStatus(numOfVehiclesBeforeClick + 1);
                    setValueAfterAdd(numOfVehiclesBeforeClick, lsiv_veh);
                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfVehicles()));
                    break;
                }
                else if (event.getSource() == del[lsiv_veh]) {
                    int numOfVehiclesBeforeClick;
                    numOfVehiclesBeforeClick = getNumberOfVehicles();
                    setValueAfterDel(numOfVehiclesBeforeClick, lsiv_veh);
                    setStatus(numOfVehiclesBeforeClick - 1);
                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfVehicles()));

                    if (!del[lsiv_veh].isEnabled()) {
                        okButton.requestFocus();
                    }
                    break;
                }
                else if (event.getSource() == up[lsiv_veh]) {
                    UpAction(lsiv_veh);
                    break;
                }
                else if (event.getSource() == down[lsiv_veh]) {
                    DownAction(lsiv_veh);
                    break;
                }
                else if (event.getSource() == comboBox_vehicle[lsiv_veh][CLASSIFICATION]) {
                    icon[lsiv_veh].setIcon(getImageIcon(comboBox_vehicle[lsiv_veh][CLASSIFICATION].getSelectedItem().toString()));
                    break;
                }
                else if (event.getSource() == icon[lsiv_veh]) {
                    new IconListDialog(comboBox_vehicle[lsiv_veh][CLASSIFICATION], icon[lsiv_veh], lsiv_veh);
                    break;
                }
                else if (event.getSource() == unit[lsiv_veh]) {
                    UnitAction(lsiv_veh);
                    break;
                }
            }
        } // end of method actionPerformed
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl != getNumberOfVehicles()) {
                        invalidDriverMixAndTrafficMix();
                        JOptionPane
                                .showMessageDialog(
                                        null,
                                        "The Number of Vehicle Classes has changed from "
                                                + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
                                                + " to "
                                                + getNumberOfVehicles()
                                                + ".\nPlease OK or Apply User-Defined Driver Mix Data and Logout Summary for Driver-Vehicle Unit by Vehicle Class in GDV Data.\nPlease OK or Apply User-Defined Traffic Mix Data for each Leg with Inbound Lanes.\nPlease OK or Apply User-Defined Traffic Mix Data for each Leg with Varying Traffic Period Data.",
                                        "Warning Message", JOptionPane.WARNING_MESSAGE);
                    }

                    gdvsim.flag_vehicleClass_ok = true;
                    saveData();

                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data = "YES";

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_veh = 1; lsiv_veh <= MAXOFVEHICLE; lsiv_veh++) {
                    if (event.getSource() == add[lsiv_veh]) {
                        int numOfVehiclesBeforeClick;
                        numOfVehiclesBeforeClick = getNumberOfVehicles();
                        setStatus(numOfVehiclesBeforeClick + 1);
                        setValueAfterAdd(numOfVehiclesBeforeClick, lsiv_veh);
                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfVehicles()));
                        break;
                    }
                    else if (event.getSource() == del[lsiv_veh]) {
                        int numOfVehiclesBeforeClick;
                        numOfVehiclesBeforeClick = getNumberOfVehicles();
                        setValueAfterDel(numOfVehiclesBeforeClick, lsiv_veh);
                        setStatus(numOfVehiclesBeforeClick - 1);
                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfVehicles()));

                        if (!del[lsiv_veh].isEnabled()) {
                            okButton.requestFocus();
                        }
                        break;
                    }
                    else if (event.getSource() == up[lsiv_veh]) {
                        UpAction(lsiv_veh);
                        break;
                    }
                    else if (event.getSource() == down[lsiv_veh]) {
                        DownAction(lsiv_veh);
                        break;
                    }
                    else if (event.getSource() == comboBox_vehicle[lsiv_veh][CLASSIFICATION]) {
                        icon[lsiv_veh].setIcon(getImageIcon(comboBox_vehicle[lsiv_veh][CLASSIFICATION].getSelectedItem().toString()));
                        break;
                    }
                    else if (event.getSource() == icon[lsiv_veh]) {
                        new IconListDialog(comboBox_vehicle[lsiv_veh][CLASSIFICATION], icon[lsiv_veh], lsiv_veh);
                        break;
                    }
                    else if (event.getSource() == unit[lsiv_veh]) {
                        UnitAction(lsiv_veh);
                        break;
                    }
                }
            }
        } // end of method actionPerformed
    } // end of class ComponentKeyListener
} // end of class VehicleClassDialog.java
