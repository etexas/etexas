package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                       VehicleUnitDialog.java.java                          */
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
import java.awt.geom.*;
import java.net.*;
import java.util.*;
import javax.swing.*;

class VehicleUnitDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    int NUMOFFIELD = 8;

    static final int UNIT_LEN = 1;

    static final int UNIT_WID = 2;

    static final int UNIT_DRWSQ = 3;

    static final int UNIT_FPD = 4;

    static final int UNIT_RPD = 5;

    static final int UNIT_RHPD = 6;

    static final int UNIT_TRNLN = 7;

    static final int UNIT_TRNWD = 8;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    JComboBox[][] cobox_unit = new JComboBox[PARAMS.TEXAS_MODEL_MNU + 1][NUMOFFIELD + 1];

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JLabel[] label_unit = new JLabel[PARAMS.TEXAS_MODEL_MNU + 1];

    JButton[] add = new JButton[PARAMS.TEXAS_MODEL_MNU + 1];

    JButton[] del = new JButton[PARAMS.TEXAS_MODEL_MNU + 1];

    JButton[] up = new JButton[PARAMS.TEXAS_MODEL_MNU + 1];

    JButton[] down = new JButton[PARAMS.TEXAS_MODEL_MNU + 1];

    JPanel draw_panel;

    /********************/

    LinkedList<Integer> lengths_list = new LinkedList<Integer>();

    LinkedList<Integer> rhpds_list = new LinkedList<Integer>();

    LinkedList<Integer> rpds_list = new LinkedList<Integer>();

    LinkedList<Integer> fpds_list = new LinkedList<Integer>();

    JButton okButton, applyButton, cancelButton;

    JLabel label_title, label_total, label_veh_length, label_veh_width, label_veh_icon, label_veh_classification;

    JComboBox cbo_total, cbo_veh_length, cbo_veh_width, cbo_veh_classification;

    Font font, font1, font2;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    int TX_FMT_GDV_VEH_UNITS_NUM_UNITS;

    int TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1;

    int TX_FMT_GDV_VEH_UNITS_UNIT_WID_1;

    int TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1;

    int TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1;

    int TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1;

    int TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1;

    int TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1;

    int TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1;

    int[] default_veh_unit_num = new int[PARAMS.TEXAS_MODEL_NVC + 1];

    int[][] default_veh_unit_length = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_unit_width = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_unit_draw_seq = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_unit_fpd = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_unit_rpd = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_unit_rhpd = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_trans_length = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int[][] default_veh_trans_width = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    int veh_class;

    int veh_length;

    int veh_width;

    String veh_classification;

    ImageIcon veh_icon;

    int max_units;

    public static int index = 1;

    public static int type = -1; // UNIT_LEN;

    public static int act = 0;

    public static int times = 0;

    public VehicleUnitDialog(int param_veh_class, int param_veh_length, int param_veh_width, String param_veh_classification, ImageIcon param_veh_icon) {
        veh_class = param_veh_class;
        veh_length = param_veh_length;
        veh_width = param_veh_width;
        veh_classification = param_veh_classification;
        veh_icon = param_veh_icon;

        label_veh_length = new JLabel("Total Vehicle Length (ft)");
        label_veh_width = new JLabel("Maximum Width (ft)");
        label_veh_icon = new JLabel(veh_icon);
        label_veh_classification = new JLabel("Vehicle Classification");

        cbo_veh_length = new JComboBox();
        cbo_veh_width = new JComboBox();
        cbo_veh_classification = new JComboBox();

        cbo_veh_length.addItem(Integer.toString(veh_length));
        cbo_veh_width.addItem(Integer.toString(veh_width));
        cbo_veh_classification.addItem(veh_classification);

        index = 1;
        type = -1;

        switch (veh_class) {
            case 1:
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01];
                times = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_01_UNIT_TRNWD_1;
                break;
            case 2:
                times = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_02_NUM_UNITS;
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_02];
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_02_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_02_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_02_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_02_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_02_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_02_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_02_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_02_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_02_UNIT_TRNWD_1;
                break;
            case 3:
                times = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_03_NUM_UNITS;
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_03];
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_03_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_03_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_03_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_03_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_03_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_03_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_03_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_03_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_03_UNIT_TRNWD_1;
                break;
            case 4:
                times = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_04_NUM_UNITS;
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_04];
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_04_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_04_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_04_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_04_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_04_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_04_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_04_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_04_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_04_UNIT_TRNWD_1;
                break;
            case 5:
                times = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_05_NUM_UNITS;
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_05];
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_05_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_05_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_05_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_05_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_05_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_05_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_05_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_05_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_05_UNIT_TRNWD_1;
                break;
            case 6:
                times = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_06_NUM_UNITS;
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_06];
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_06_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_06_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_06_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_06_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_06_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_06_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_06_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_06_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_06_UNIT_TRNWD_1;
                break;
            case 7:
                times = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_07_NUM_UNITS;
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_07];
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_07_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_07_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_07_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_07_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_07_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_07_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_07_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_07_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_07_UNIT_TRNWD_1;
                break;
            case 8:
                times = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_08_NUM_UNITS;
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_08];
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_08_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_08_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_08_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_08_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_08_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_08_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_08_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_08_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_08_UNIT_TRNWD_1;
                break;
            case 9:
                times = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_09_NUM_UNITS;
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_09];
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_09_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_09_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_09_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_09_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_09_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_09_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_09_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_09_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_09_UNIT_TRNWD_1;
                break;
            case 10:
                times = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_10_NUM_UNITS;
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_10];
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_10_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_10_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_10_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_10_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_10_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_10_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_10_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_10_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_10_UNIT_TRNWD_1;
                break;
            case 11:
                times = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_11_NUM_UNITS;
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_11];
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_11_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_11_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_11_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_11_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_11_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_11_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_11_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_11_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_11_UNIT_TRNWD_1;
                break;
            case 12:
                times = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_12_NUM_UNITS;

                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_12];
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_12_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_12_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_12_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_12_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_12_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_12_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_12_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_12_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_12_UNIT_TRNWD_1;
                break;
            case 13:
                times = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_NUM_UNITS;

                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13];
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_TRNWD_1;
                break;
            default:
                times = 0;
                lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13];
                TX_FMT_GDV_VEH_UNITS_NUM_UNITS = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_NUM_UNITS;
                TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_LEN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_WID_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_DRWSQ_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_FPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_RPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_RHPD_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_TRNLN_1;
                TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 = gdvsim.gclv_inter.TX_FMT_GDV_VEH_UNITS_13_UNIT_TRNWD_1;
        }

        for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
            default_veh_unit_num[lsiv_veh] = lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_NUM_UNITS];
            for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
                default_veh_unit_length[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 - 1 + lsiv_unit];
                default_veh_unit_width[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 - 1 + lsiv_unit];
                default_veh_unit_draw_seq[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 - 1 + lsiv_unit];
                default_veh_unit_fpd[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 - 1 + lsiv_unit];
                default_veh_unit_rpd[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 - 1 + lsiv_unit];
                default_veh_unit_rhpd[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 - 1 + lsiv_unit];
                default_veh_trans_length[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 - 1 + lsiv_unit];
                default_veh_trans_width[lsiv_veh][lsiv_unit] = lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 - 1 + lsiv_unit];
            }
        }

        int numOfUnits;

        if (gdvsim.veh_unit_num_stat[veh_class] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            numOfUnits = lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_NUM_UNITS];
        }
        else {
            numOfUnits = gdvsim.veh_unit_num[veh_class];
        }

        titleString = lclv_tx_fmt.mstv_name.replace("#", Integer.toString(veh_class));
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
        font2 = new Font("TimesRoman", Font.BOLD, 12);

        label_title = new JLabel(titleString);
        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
            label_unit[lsiv_unit] = new JLabel("Unit " + lsiv_unit);
        }

        for (int lsiv_unit = 1; lsiv_unit <= PARAMS.TEXAS_MODEL_MNU; lsiv_unit++) {
            add[lsiv_unit] = new JButton("Add");
            del[lsiv_unit] = new JButton("Delete");
            up[lsiv_unit] = new JButton("Up");
            down[lsiv_unit] = new JButton("Down");
        }

        label_total = new JLabel("Total Number of Units");

        String lblName;
        int beg;
        int end;

        lblName = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1];
        beg = lblName.indexOf("Unit 1 ") + 7;
        end = lblName.indexOf("for") - 1;
        lblName = lblName.substring(beg, end) + " (ft)";
        label_field[UNIT_LEN] = new JTextArea(lblName);

        lblName = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1];
        beg = lblName.indexOf("Unit 1 ") + 7;
        end = lblName.indexOf("for") - 1;
        lblName = lblName.substring(beg, end) + " (ft)";
        label_field[UNIT_WID] = new JTextArea(lblName);

        lblName = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1];
        beg = lblName.indexOf("Unit 1 ") + 7;
        end = lblName.indexOf("for") - 1;
        lblName = lblName.substring(beg, end) + " (ft)";
        label_field[UNIT_DRWSQ] = new JTextArea(lblName);

        lblName = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1];
        beg = lblName.indexOf("Unit 1 ") + 7;
        end = lblName.indexOf("for") - 1;
        lblName = lblName.substring(beg, end) + " (ft)";
        label_field[UNIT_FPD] = new JTextArea(lblName);

        lblName = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1];
        beg = lblName.indexOf("Unit 1 ") + 7;
        end = lblName.indexOf("for") - 1;
        lblName = lblName.substring(beg, end) + " (ft)";
        label_field[UNIT_RPD] = new JTextArea(lblName);

        lblName = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1];
        beg = lblName.indexOf("Unit 1 ") + 7;
        end = lblName.indexOf("for") - 1;
        lblName = lblName.substring(beg, end) + " (ft)";
        label_field[UNIT_RHPD] = new JTextArea(lblName);

        lblName = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1];
        beg = lblName.indexOf("Unit 1 ") + 7;
        end = lblName.indexOf("for") - 1;
        lblName = lblName.substring(beg, end) + " (ft)";
        label_field[UNIT_TRNLN] = new JTextArea(lblName);

        lblName = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1];
        beg = lblName.indexOf("Unit 1 ") + 7;
        end = lblName.indexOf("for") - 1;
        lblName = lblName.substring(beg, end) + " (ft)";
        label_field[UNIT_TRNWD] = new JTextArea(lblName);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            label_field[lsiv_field].setBackground(aFrame.getBackground());
            label_field[lsiv_field].setEditable(false);
            label_field[lsiv_field].setFocusable(false);
            label_field[lsiv_field].setWrapStyleWord(true);
            label_field[lsiv_field].setLineWrap(true);
            label_field[lsiv_field].setFont(font1);
        }

        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;
        int count;
        int int_number;

        if (veh_class <= PARAMS.TEXAS_MODEL_NVCD) {
            max_units = default_veh_unit_num[veh_class];
        }
        else {
            max_units = PARAMS.TEXAS_MODEL_MNU;
        }

        for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
            lsiv_min = lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 - 1 + lsiv_unit];
            lsiv_max = lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 - 1 + lsiv_unit];
            lsiv_inc = lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 - 1 + lsiv_unit];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_msia_veh_unit_length = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_veh_unit_length[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            lsiv_min = lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 - 1 + lsiv_unit];
            lsiv_max = lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 - 1 + lsiv_unit];
            lsiv_inc = lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 - 1 + lsiv_unit];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_msia_veh_unit_width = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_veh_unit_width[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            lsiv_min = lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 - 1 + lsiv_unit];
            lsiv_max = lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 - 1 + lsiv_unit];
            lsiv_inc = lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 - 1 + lsiv_unit];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_msia_veh_unit_draw_seq = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_veh_unit_draw_seq[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            lsiv_min = lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 - 1 + lsiv_unit];
            lsiv_max = lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 - 1 + lsiv_unit];
            lsiv_inc = lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 - 1 + lsiv_unit];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_msia_veh_unit_fpd = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_veh_unit_fpd[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            lsiv_min = lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 - 1 + lsiv_unit];
            lsiv_max = lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 - 1 + lsiv_unit];
            lsiv_inc = lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 - 1 + lsiv_unit];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_msia_veh_unit_rpd = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_veh_unit_rpd[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            lsiv_min = lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 - 1 + lsiv_unit];
            lsiv_max = lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 - 1 + lsiv_unit];
            lsiv_inc = lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 - 1 + lsiv_unit];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_msia_veh_unit_rhpd = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_veh_unit_rhpd[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            lsiv_min = lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 - 1 + lsiv_unit];
            lsiv_max = lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 - 1 + lsiv_unit];
            lsiv_inc = lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 - 1 + lsiv_unit];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_msia_veh_trans_length = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_veh_trans_length[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            lsiv_min = lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 - 1 + lsiv_unit];
            lsiv_max = lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 - 1 + lsiv_unit];
            lsiv_inc = lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 - 1 + lsiv_unit];

            count = (lsiv_max - lsiv_min) / lsiv_inc + 1;
            int_number = lsiv_min;
            String[] array_msia_veh_trans_width = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_veh_trans_width[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc;
            }

            cobox_unit[lsiv_unit][UNIT_LEN] = new JComboBox(array_msia_veh_unit_length);
            cobox_unit[lsiv_unit][UNIT_WID] = new JComboBox(array_msia_veh_unit_width);
            cobox_unit[lsiv_unit][UNIT_DRWSQ] = new JComboBox(array_msia_veh_unit_draw_seq);
            cobox_unit[lsiv_unit][UNIT_FPD] = new JComboBox(array_msia_veh_unit_fpd);
            cobox_unit[lsiv_unit][UNIT_RPD] = new JComboBox(array_msia_veh_unit_rpd);
            cobox_unit[lsiv_unit][UNIT_RHPD] = new JComboBox(array_msia_veh_unit_rhpd);
            cobox_unit[lsiv_unit][UNIT_TRNLN] = new JComboBox(array_msia_veh_trans_length);
            cobox_unit[lsiv_unit][UNIT_TRNWD] = new JComboBox(array_msia_veh_trans_width);

            if (gdvsim.veh_unit_length_stat[veh_class][lsiv_unit] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                cobox_unit[lsiv_unit][UNIT_LEN].setSelectedItem(Integer.toString(gdvsim.veh_unit_length[veh_class][lsiv_unit]));
            }
            else {
                cobox_unit[lsiv_unit][UNIT_LEN].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1]));
            }

            if (gdvsim.veh_unit_width_stat[veh_class][lsiv_unit] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                cobox_unit[lsiv_unit][UNIT_WID].setSelectedItem(Integer.toString(gdvsim.veh_unit_width[veh_class][lsiv_unit]));
            }
            else {
                cobox_unit[lsiv_unit][UNIT_WID].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1]));
            }

            if (gdvsim.veh_unit_draw_seq_stat[veh_class][lsiv_unit] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                cobox_unit[lsiv_unit][UNIT_DRWSQ].setSelectedItem(Integer.toString(gdvsim.veh_unit_draw_seq[veh_class][lsiv_unit]));
            }
            else {
                cobox_unit[lsiv_unit][UNIT_DRWSQ].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1]));
            }

            if (gdvsim.veh_unit_fpd_stat[veh_class][lsiv_unit] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                cobox_unit[lsiv_unit][UNIT_FPD].setSelectedItem(Integer.toString(gdvsim.veh_unit_fpd[veh_class][lsiv_unit]));
            }
            else {
                cobox_unit[lsiv_unit][UNIT_FPD].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1]));
            }

            if (gdvsim.veh_unit_rpd_stat[veh_class][lsiv_unit] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                cobox_unit[lsiv_unit][UNIT_RPD].setSelectedItem(Integer.toString(gdvsim.veh_unit_rpd[veh_class][lsiv_unit]));
            }
            else {
                cobox_unit[lsiv_unit][UNIT_RPD].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1]));
            }

            if (gdvsim.veh_unit_rhpd_stat[veh_class][lsiv_unit] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                cobox_unit[lsiv_unit][UNIT_RHPD].setSelectedItem(Integer.toString(gdvsim.veh_unit_rhpd[veh_class][lsiv_unit]));
            }
            else {
                cobox_unit[lsiv_unit][UNIT_RHPD].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1]));
            }

            if (gdvsim.veh_trans_length_stat[veh_class][lsiv_unit] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                cobox_unit[lsiv_unit][UNIT_TRNLN].setSelectedItem(Integer.toString(gdvsim.veh_trans_length[veh_class][lsiv_unit]));
            }
            else {
                cobox_unit[lsiv_unit][UNIT_TRNLN].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1]));
            }

            if (gdvsim.veh_trans_width_stat[veh_class][lsiv_unit] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                cobox_unit[lsiv_unit][UNIT_TRNWD].setSelectedItem(Integer.toString(gdvsim.veh_trans_width[veh_class][lsiv_unit]));
            }
            else {
                cobox_unit[lsiv_unit][UNIT_TRNWD].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1]));
            }
        }

        setStatus(numOfUnits);

        if (times != 0) {
            times = numOfUnits;
        }
        cbo_total = new JComboBox();
        cbo_total.addItem(Integer.toString(getNumberOfUnits()));

        okButton = new JButton("  OK  ");
        applyButton = new JButton(" Apply");
        cancelButton = new JButton("Cancel");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        JPanel ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        draw_panel = new JPanel() {

            public void paintComponent(Graphics g) {
                super.paintComponent(g);

            }

            public void paint(Graphics g) {

                int a;
                if (type == UNIT_LEN) {
                    a = Integer.parseInt((String)cobox_unit[index][UNIT_LEN].getSelectedItem());
                    a = (int)(a * 1.0 * getWidth() / 100.0);
                    if (lengths_list.size() >= index)
                        lengths_list.set(index - 1, new Integer(a));
                    else
                        lengths_list.add(new Integer(a));
                }
                else if (type == UNIT_RHPD) {
                    a = Integer.parseInt((String)cobox_unit[index][UNIT_RHPD].getSelectedItem());
                    a = (int)(a * 1.0 * getWidth() / 100.0);
                    if (rhpds_list.size() >= index)
                        rhpds_list.set(index - 1, new Integer(a));
                    else
                        rhpds_list.add(new Integer(a));
                }
                else if (type == UNIT_RPD) {
                    a = Integer.parseInt((String)cobox_unit[index][UNIT_RPD].getSelectedItem());
                    a = (int)(a * 1.0 * getWidth() / 100.0);
                    if (rpds_list.size() >= index)
                        rpds_list.set(index - 1, new Integer(a));
                    else
                        rpds_list.add(new Integer(a));
                }
                else if (type == UNIT_FPD) {
                    a = Integer.parseInt((String)cobox_unit[index][UNIT_FPD].getSelectedItem());
                    a = (int)(a * 1.0 * getWidth() / 100.0);
                    if (fpds_list.size() >= index)
                        fpds_list.set(index - 1, new Integer(a));
                    else
                        fpds_list.add(new Integer(a));
                }
                else {

                    if (type == -1) {
                        if (times == 0) {
                            a = Integer.parseInt((String)cobox_unit[index][UNIT_LEN].getSelectedItem());
                            a = (int)(a * 1.0 * getWidth() / 100.0);
                            if (lengths_list.size() >= index)
                                lengths_list.add(index - 1, new Integer(a));
                            else
                                lengths_list.add(new Integer(a));

                            a = Integer.parseInt((String)cobox_unit[index][UNIT_RHPD].getSelectedItem());
                            a = (int)(a * 1.0 * getWidth() / 100.0);
                            if (rhpds_list.size() >= index)
                                rhpds_list.add(index - 1, new Integer(a));
                            else
                                rhpds_list.add(new Integer(a));

                            a = Integer.parseInt((String)cobox_unit[index][UNIT_RPD].getSelectedItem());
                            a = (int)(a * 1.0 * getWidth() / 100.0);
                            if (rpds_list.size() >= index)
                                rpds_list.add(index - 1, new Integer(a));
                            else
                                rpds_list.add(new Integer(a));

                            a = Integer.parseInt((String)cobox_unit[index][UNIT_FPD].getSelectedItem());
                            a = (int)(a * 1.0 * getWidth() / 100.0);
                            if (fpds_list.size() >= index) {
                                fpds_list.add(index - 1, new Integer(a));
                            }
                            else {
                                fpds_list.add(new Integer(a));
                            }
                        }
                        else {
                            // lengths_list.clear();
                            // rhpds_list.clear();
                            // rpds_list.clear();
                            // fpds_list.clear();
                            int mtimes = times;
                            for (int q = 1; q <= mtimes; q++) {
                                int index = q;
                                a = Integer.parseInt((String)cobox_unit[index][UNIT_LEN].getSelectedItem());
                                a = (int)(a * 1.0 * getWidth() / 100.0);
                                if (lengths_list.size() >= index)
                                    lengths_list.add(index - 1, new Integer(a));
                                else
                                    lengths_list.add(new Integer(a));

                                a = Integer.parseInt((String)cobox_unit[index][UNIT_RHPD].getSelectedItem());
                                a = (int)(a * 1.0 * getWidth() / 100.0);
                                if (rhpds_list.size() >= index)
                                    rhpds_list.add(index - 1, new Integer(a));
                                else
                                    rhpds_list.add(new Integer(a));

                                a = Integer.parseInt((String)cobox_unit[index][UNIT_RPD].getSelectedItem());
                                a = (int)(a * 1.0 * getWidth() / 100.0);
                                if (rpds_list.size() >= index)
                                    rpds_list.add(index - 1, new Integer(a));
                                else
                                    rpds_list.add(new Integer(a));

                                a = Integer.parseInt((String)cobox_unit[index][UNIT_FPD].getSelectedItem());
                                a = (int)(a * 1.0 * getWidth() / 100.0);
                                if (fpds_list.size() >= index) {
                                    fpds_list.add(index - 1, new Integer(a));
                                }
                                else {
                                    fpds_list.add(new Integer(a));
                                }

                            }

                        }

                    }

                    if (type == -2) {

                        if (lengths_list.size() >= index) {
                            Integer tmp = lengths_list.get(index - 1);
                            lengths_list.set(index - 1, lengths_list.get(index - 2));
                            lengths_list.set(index - 2, tmp);
                        }

                        if (rhpds_list.size() >= index) {
                            Integer tmp = rhpds_list.get(index - 1);
                            rhpds_list.set(index - 1, rhpds_list.get(index - 2));
                            rhpds_list.set(index - 2, tmp);
                        }

                        if (rpds_list.size() >= index) {
                            Integer tmp = rpds_list.get(index - 1);
                            rpds_list.set(index - 1, rpds_list.get(index - 2));
                            rpds_list.set(index - 2, tmp);
                        }

                        if (fpds_list.size() >= index) {
                            Integer tmp = fpds_list.get(index - 1);
                            fpds_list.set(index - 1, fpds_list.get(index - 2));
                            fpds_list.set(index - 2, tmp);
                        }

                    }

                    if (type == -3) {

                        if (lengths_list.size() >= (index + 1)) {
                            Integer tmp = lengths_list.get(index - 1);
                            lengths_list.set(index - 1, lengths_list.get(index));
                            lengths_list.set(index, tmp);
                        }

                        if (rhpds_list.size() >= (index + 1)) {
                            Integer tmp = rhpds_list.get(index - 1);
                            rhpds_list.set(index - 1, rhpds_list.get(index));
                            rhpds_list.set(index, tmp);
                        }

                        if (rpds_list.size() >= (index + 1)) {
                            Integer tmp = rpds_list.get(index - 1);
                            rpds_list.set(index - 1, rpds_list.get(index));
                            rpds_list.set(index, tmp);
                        }

                        if (fpds_list.size() >= (index + 1)) {
                            Integer tmp = fpds_list.get(index - 1);
                            fpds_list.set(index - 1, fpds_list.get(index));
                            fpds_list.set(index, tmp);
                        }

                    }

                }

                Color c = getBackground();
                Color tmp = g.getColor();
                g.setColor(c);
                ((Graphics2D)g).fillRect(0, 0, getWidth(), getHeight());
                g.setColor(tmp);

                Color lastColor = ((Graphics2D)g).getColor();
                Composite tmpComposite = ((Graphics2D)g).getComposite();
                ((Graphics2D)g).setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_ATOP, 0.70f));
                ((Graphics2D)g).setColor(Color.DARK_GRAY);

                float[] df = new float[2];
                df[0] = 5.0f;
                df[1] = 5.0f;

                //

                ListIterator<Integer> l;
                l = lengths_list.listIterator(0);
                int ii = 0;
                int fh = (int)(0.9 * getHeight());

                // int fhr = (int)(0.9*getHeight());
                // GeneralPath gp_ruler = new GeneralPath();
                // gp_ruler.moveTo( 100, fhr );
                // gp_ruler.lineTo(920, fhr);

                // for(int j = 0; j < 70; j++)
                // {
                // gp_ruler.moveTo(100+ j*13, fhr);
                // if(j%5 ==0)
                // gp_ruler.lineTo(100 +j*13, fhr + 10);
                // else
                // gp_ruler.lineTo(100 +j*13, fhr + 5);
                // }
                // ((Graphics2D)g).draw(gp_ruler);

                while (l.hasNext()) {
                    int ht = 10;
                    // int ht = (int) ((getHeight()*0.8)*1.0/(8));
                    // int ht = (int) (getHeight());
                    int val = (l.next()).intValue();
                    int start = 100;
                    for (int j = ii - 1; j >= 0; j--) {
                        if (rhpds_list.size() > j)
                            start += (rhpds_list.get(j).intValue()) / 8;
                        if (j != 0 && fpds_list.size() > j)
                            start -= (fpds_list.get(j).intValue()) / 8;
                    }

                    if (ii != 0 && fpds_list.size() > ii)
                        start -= (fpds_list.get(ii).intValue()) / 8;
                    GeneralPath gp = new GeneralPath();
                    gp.moveTo(start, fh - ht * (ii));
                    gp.lineTo(start + (lengths_list.get(ii).intValue()) / 8, fh - ht * (ii));
                    gp.lineTo(start + (lengths_list.get(ii).intValue()) / 8, fh - ht * (ii + 1));
                    gp.lineTo(start, fh - ht * (ii + 1));
                    gp.closePath();

                    ((Graphics2D)g).draw(gp);
                    g.setColor(Color.BLACK);
                    ((Graphics2D)g).draw(gp);

                    g.setColor(Color.DARK_GRAY);
                    ((Graphics2D)g).setStroke(new BasicStroke(1.0f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 20.0f, df, 2.0f));

                    gp.moveTo(start + (fpds_list.get(ii).intValue()) / 8, fh - ht * (ii));
                    gp.lineTo(start + (fpds_list.get(ii).intValue()) / 8, fh - ht * (ii + 1));

                    gp.moveTo(start + (rhpds_list.get(ii).intValue()) / 8, fh - ht * (ii));
                    gp.lineTo(start + (rhpds_list.get(ii).intValue()) / 8, fh - ht * (ii + 1));

                    gp.moveTo(start + (rpds_list.get(ii).intValue()) / 8, fh - ht * (ii));
                    gp.lineTo(start + (rpds_list.get(ii).intValue()) / 8, fh - ht * (ii + 1));

                    ((Graphics2D)g).draw(gp);
                    ((Graphics2D)g).setStroke(new BasicStroke());

                    ii++;
                }
                type = 0;
                ((Graphics2D)g).setComposite(tmpComposite);
                g.setColor(lastColor);

            }

        };

        draw_panel.setSize(800, 200);

        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();

        for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
            add[lsiv_unit].addActionListener(componentActionListener);
            del[lsiv_unit].addActionListener(componentActionListener);
            up[lsiv_unit].addActionListener(componentActionListener);
            down[lsiv_unit].addActionListener(componentActionListener);

            add[lsiv_unit].addKeyListener(componentKeyListener);
            del[lsiv_unit].addKeyListener(componentKeyListener);
            up[lsiv_unit].addKeyListener(componentKeyListener);
            down[lsiv_unit].addKeyListener(componentKeyListener);

            add[lsiv_unit].addKeyListener(helpListener);
            del[lsiv_unit].addKeyListener(helpListener);
            up[lsiv_unit].addKeyListener(helpListener);
            down[lsiv_unit].addKeyListener(helpListener);

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                cobox_unit[lsiv_unit][lsiv_field].addKeyListener(helpListener);
                cobox_unit[lsiv_unit][lsiv_field].addKeyListener(openComboMenuListener);

                cobox_unit[lsiv_unit][UNIT_LEN].addActionListener(componentActionListener);
                cobox_unit[lsiv_unit][UNIT_RHPD].addActionListener(componentActionListener);
                cobox_unit[lsiv_unit][UNIT_RPD].addActionListener(componentActionListener);
                cobox_unit[lsiv_unit][UNIT_FPD].addActionListener(componentActionListener);

            }
        }

        cbo_veh_length.addKeyListener(openComboMenuListener);
        cbo_veh_width.addKeyListener(openComboMenuListener);
        cbo_veh_classification.addKeyListener(openComboMenuListener);

        cbo_veh_length.addKeyListener(helpListener);
        cbo_veh_width.addKeyListener(helpListener);
        cbo_veh_classification.addKeyListener(helpListener);

        cbo_total.addKeyListener(openComboMenuListener);
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
        int iCol = NUMOFFIELD + 5;

        label_veh_length = new JLabel("Total Vehicle Length (ft)", JLabel.LEFT);
        label_veh_width = new JLabel("Maximum Width (ft)", JLabel.LEFT);
        label_veh_icon = new JLabel(veh_icon, JLabel.LEFT);
        label_veh_classification = new JLabel("Vehicle Classification", JLabel.LEFT);

        gbConstraints.insets = new Insets(1, 1, 1, 1);
        addComponent(label_veh_icon, iRow++, 1, iCol, 1);

        gbConstraints.insets = new Insets(10, 1, 1, 1);
        addComponent(cbo_veh_length, iRow, 1, 1, 1);
        addComponent(label_veh_length, iRow, 2, 3, 1);

        gbConstraints.insets = new Insets(1, 1, 1, 1);
        addComponent(panel_title, iRow++, 5, NUMOFFIELD, 1);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == NUMOFFIELD) {
                gbConstraints.insets = new Insets(1, 1, 5, 1);
            }
            else {
                gbConstraints.insets = new Insets(1, 1, 5, 5);
            }
            addComponent(label_field[lsiv_field], iRow, 4 + lsiv_field, 1, 2);
        }

        gbConstraints.insets = new Insets(1, 1, 1, 1);
        addComponent(cbo_veh_width, iRow, 1, 1, 1);
        addComponent(label_veh_width, iRow++, 2, 3, 1);

        gbConstraints.insets = new Insets(1, 1, 5, 1);
        addComponent(cbo_veh_classification, iRow, 1, 1, 1);
        addComponent(label_veh_classification, iRow++, 2, 3, 1);

        for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
            gbConstraints.insets = new Insets(1, 5, 1, 1);
            addComponent(label_unit[lsiv_unit], iRow, 0, 1, 1);

            gbConstraints.insets = new Insets(1, 1, 1, 1);
            addComponent(add[lsiv_unit], iRow, 1, 1, 1);
            addComponent(del[lsiv_unit], iRow, 2, 1, 1);
            addComponent(up[lsiv_unit], iRow, 3, 1, 1);
            addComponent(down[lsiv_unit], iRow, 4, 1, 1);

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == NUMOFFIELD) {
                    gbConstraints.insets = new Insets(1, 1, 1, 5);
                }
                else {
                    gbConstraints.insets = new Insets(1, 1, 1, 1);
                }
                addComponent(cobox_unit[lsiv_unit][lsiv_field], iRow, 4 + lsiv_field, 1, 1);
            }

            iRow++;
        }

        gbConstraints.insets = new Insets(1, 1, 1, 1);
        addComponent(cbo_total, iRow, 1, 1, 1);

        addComponent(label_total, iRow++, 2, 4, 1);
        gbConstraints.weighty = 1;
        gbConstraints.ipady = 40;
        addComponent(draw_panel, iRow++, 0, iCol, 1);
        gbConstraints.weighty = 0;
        gbConstraints.ipady = 0;
        addComponent(ok_panel, iRow++, 0, iCol, 1);

        aFrame.setSize(850, 500);
        aFrame.setVisible(true);
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        aFrame.pack();
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);
    } // end of method VehicleUnitDialog

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    int getNumberOfUnits() {
        int sum = 0;

        for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
            if (cobox_unit[lsiv_unit][UNIT_LEN].isEnabled())
                sum++;
        }

        return sum;
    } // end of getNumberOfUnits()

    void setSize() {
        label_field[UNIT_LEN].setPreferredSize(new Dimension(65, 60));
        label_field[UNIT_WID].setPreferredSize(new Dimension(65, 60));
        label_field[UNIT_DRWSQ].setPreferredSize(new Dimension(80, 60));
        label_field[UNIT_FPD].setPreferredSize(new Dimension(75, 60));
        label_field[UNIT_RPD].setPreferredSize(new Dimension(75, 60));
        label_field[UNIT_RHPD].setPreferredSize(new Dimension(80, 60));
        label_field[UNIT_TRNLN].setPreferredSize(new Dimension(65, 60));
        label_field[UNIT_TRNWD].setPreferredSize(new Dimension(60, 60));

        label_field[UNIT_LEN].setMaximumSize(new Dimension(65, 60));
        label_field[UNIT_WID].setMaximumSize(new Dimension(65, 60));
        label_field[UNIT_DRWSQ].setMaximumSize(new Dimension(80, 60));
        label_field[UNIT_FPD].setMaximumSize(new Dimension(75, 60));
        label_field[UNIT_RPD].setMaximumSize(new Dimension(75, 60));
        label_field[UNIT_RHPD].setMaximumSize(new Dimension(80, 60));
        label_field[UNIT_TRNLN].setMaximumSize(new Dimension(65, 60));
        label_field[UNIT_TRNWD].setMaximumSize(new Dimension(60, 60));

        for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
            add[lsiv_unit].setPreferredSize(new Dimension(60, 25));
            del[lsiv_unit].setPreferredSize(new Dimension(70, 25));
            up[lsiv_unit].setPreferredSize(new Dimension(50, 25));
            down[lsiv_unit].setPreferredSize(new Dimension(70, 25));
            add[lsiv_unit].setMaximumSize(new Dimension(60, 25));
            del[lsiv_unit].setMaximumSize(new Dimension(70, 25));
            up[lsiv_unit].setMaximumSize(new Dimension(50, 25));
            down[lsiv_unit].setMaximumSize(new Dimension(70, 25));

            label_unit[lsiv_unit].setPreferredSize(new Dimension(40, 25));
            label_unit[lsiv_unit].setMaximumSize(new Dimension(40, 25));

            cobox_unit[lsiv_unit][UNIT_LEN].setPreferredSize(new Dimension(65, 25));
            cobox_unit[lsiv_unit][UNIT_WID].setPreferredSize(new Dimension(65, 25));
            cobox_unit[lsiv_unit][UNIT_DRWSQ].setPreferredSize(new Dimension(80, 25));
            cobox_unit[lsiv_unit][UNIT_FPD].setPreferredSize(new Dimension(75, 25));
            cobox_unit[lsiv_unit][UNIT_RPD].setPreferredSize(new Dimension(75, 25));
            cobox_unit[lsiv_unit][UNIT_RHPD].setPreferredSize(new Dimension(50, 25));
            cobox_unit[lsiv_unit][UNIT_TRNLN].setPreferredSize(new Dimension(65, 25));
            cobox_unit[lsiv_unit][UNIT_TRNWD].setPreferredSize(new Dimension(60, 25));

            cobox_unit[lsiv_unit][UNIT_LEN].setMaximumSize(new Dimension(65, 25));
            cobox_unit[lsiv_unit][UNIT_WID].setMaximumSize(new Dimension(65, 25));
            cobox_unit[lsiv_unit][UNIT_DRWSQ].setMaximumSize(new Dimension(80, 25));
            cobox_unit[lsiv_unit][UNIT_FPD].setMaximumSize(new Dimension(75, 25));
            cobox_unit[lsiv_unit][UNIT_RPD].setMaximumSize(new Dimension(75, 25));
            cobox_unit[lsiv_unit][UNIT_RHPD].setMaximumSize(new Dimension(50, 25));
            cobox_unit[lsiv_unit][UNIT_TRNLN].setMaximumSize(new Dimension(65, 25));
            cobox_unit[lsiv_unit][UNIT_TRNWD].setMaximumSize(new Dimension(60, 25));
        }

        cbo_total.setPreferredSize(new Dimension(60, 25));
        cbo_total.setMaximumSize(new Dimension(60, 25));
        label_total.setPreferredSize(new Dimension(60, 25));
        label_total.setMaximumSize(new Dimension(60, 25));

        okButton.setPreferredSize(new Dimension(80, 25));
        applyButton.setPreferredSize(new Dimension(80, 25));
        cancelButton.setPreferredSize(new Dimension(80, 25));

        okButton.setMaximumSize(new Dimension(80, 25));
        applyButton.setMaximumSize(new Dimension(80, 25));
        cancelButton.setMaximumSize(new Dimension(80, 25));

        draw_panel.setMaximumSize(new Dimension(800, 200));
    } // end of setSize

    void setAccessiblility() {
        if (veh_class <= PARAMS.TEXAS_MODEL_NVCD) {
            for (int lsiv_unit = 1; lsiv_unit <= 1; lsiv_unit++) {
                cobox_unit[lsiv_unit][UNIT_LEN].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_LEN].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_WID].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_WID].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_DRWSQ].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_DRWSQ].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_FPD].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_FPD].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_RPD].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_RPD].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_RHPD].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_RHPD].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_TRNLN].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_TRNLN].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_TRNWD].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_TRNWD].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1] + " for vehicle class " + veh_class);
            }
        }
        else {
            for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
                cobox_unit[lsiv_unit][UNIT_LEN].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_LEN].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_WID].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_WID].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_DRWSQ].getAccessibleContext()
                        .setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_DRWSQ].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_FPD].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_FPD].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_RPD].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_RPD].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_RHPD].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_RHPD].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_TRNLN].getAccessibleContext()
                        .setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_TRNLN].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_TRNWD].getAccessibleContext()
                        .setAccessibleName(lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
                cobox_unit[lsiv_unit][UNIT_TRNWD].getAccessibleContext().setAccessibleDescription(
                        lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 - 1 + lsiv_unit] + " for vehicle class " + veh_class);
            }
        }

        for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
            add[lsiv_unit].getAccessibleContext().setAccessibleName("Add    for vehicle class " + veh_class + " unit " + lsiv_unit);
            add[lsiv_unit].getAccessibleContext().setAccessibleDescription("Add    for vehicle class " + veh_class + " unit " + lsiv_unit);
            del[lsiv_unit].getAccessibleContext().setAccessibleName("Delete for vehicle class " + veh_class + " unit " + lsiv_unit);
            del[lsiv_unit].getAccessibleContext().setAccessibleDescription("Delete for vehicle class " + veh_class + " unit " + lsiv_unit);
            up[lsiv_unit].getAccessibleContext().setAccessibleName("Up     for vehicle class " + veh_class + " unit " + lsiv_unit);
            up[lsiv_unit].getAccessibleContext().setAccessibleDescription("Up     for vehicle class " + veh_class + " unit " + lsiv_unit);
            down[lsiv_unit].getAccessibleContext().setAccessibleName("Down   for vehicle class " + veh_class + " unit " + lsiv_unit);
            down[lsiv_unit].getAccessibleContext().setAccessibleDescription("Down   for vehicle class " + veh_class + " unit " + lsiv_unit);
        }

        cbo_veh_length.getAccessibleContext().setAccessibleName(label_veh_length.getText() + " for vehicle class " + veh_class);
        cbo_veh_length.getAccessibleContext().setAccessibleDescription(label_veh_length.getText() + " for vehicle class " + veh_class);
        cbo_veh_width.getAccessibleContext().setAccessibleName(label_veh_width.getText() + " for vehicle class " + veh_class);
        cbo_veh_width.getAccessibleContext().setAccessibleDescription(label_veh_width.getText() + " for vehicle class " + veh_class);
        cbo_veh_classification.getAccessibleContext().setAccessibleName(label_veh_classification.getText() + " for vehicle class " + veh_class);
        cbo_veh_classification.getAccessibleContext().setAccessibleDescription(label_veh_classification.getText() + " for vehicle class " + veh_class);

        cbo_total.getAccessibleContext().setAccessibleName(label_total.getText());
        cbo_total.getAccessibleContext().setAccessibleDescription(label_total.getText());

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

    } // end of setAccessiblility()

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
    } // end of OpenComboMenuListener()

    void setStatus(int sumOfUnits) {
        for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
            if (lsiv_unit <= sumOfUnits) {
                label_unit[lsiv_unit].setEnabled(true);
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    cobox_unit[lsiv_unit][lsiv_field].setEnabled(true);
                }
            }
            else {
                label_unit[lsiv_unit].setEnabled(false);
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    cobox_unit[lsiv_unit][lsiv_field].setEnabled(false);
                }
            }
        }

        if (sumOfUnits == max_units) {
            for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
                add[lsiv_unit].setEnabled(false);
            }
        }
        else {
            for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
                if (lsiv_unit <= (sumOfUnits + 1)) {
                    add[lsiv_unit].setEnabled(true);
                }
                else {
                    add[lsiv_unit].setEnabled(false);
                }
            }
        }

        for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
            if (lsiv_unit <= sumOfUnits) {
                del[lsiv_unit].setEnabled(true);
            }
            else {
                del[lsiv_unit].setEnabled(false);
            }
        }

        for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
            if (lsiv_unit <= sumOfUnits) {
                up[lsiv_unit].setEnabled(true);
            }
            else {
                up[lsiv_unit].setEnabled(false);
            }

            up[1].setEnabled(false);
        }

        for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
            if (lsiv_unit < sumOfUnits) {
                down[lsiv_unit].setEnabled(true);
            }
            else {
                down[lsiv_unit].setEnabled(false);
            }
        }

        if (sumOfUnits <= 1) {
            del[1].setEnabled(false);
            label_unit[1].setEnabled(true);
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                cobox_unit[1][lsiv_field].setEnabled(true);
            }
        }

        if (veh_class <= PARAMS.TEXAS_MODEL_NVCD) {
            for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
                add[lsiv_unit].setEnabled(false);
                del[lsiv_unit].setEnabled(false);
                up[lsiv_unit].setEnabled(false);
                down[lsiv_unit].setEnabled(false);
            }
        }
    } // end of method setStatus

    void setValueAfterAdd(int sum, int index) {
        for (int lsiv_unit = sum; lsiv_unit >= index; lsiv_unit--) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                cobox_unit[lsiv_unit + 1][lsiv_field].setSelectedItem(cobox_unit[lsiv_unit][lsiv_field].getSelectedItem().toString());
            }
        }
    } // end of method setValueAfterAdd()

    void setValueAfterDel(int sum, int index) {
        for (int lsiv_unit = index; lsiv_unit < sum; lsiv_unit++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                cobox_unit[lsiv_unit][lsiv_field].setSelectedItem(cobox_unit[lsiv_unit + 1][lsiv_field].getSelectedItem().toString());
            }
        }

        cobox_unit[sum][UNIT_LEN].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1]));
        cobox_unit[sum][UNIT_WID].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1]));
        cobox_unit[sum][UNIT_DRWSQ].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1]));
        cobox_unit[sum][UNIT_FPD].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1]));
        cobox_unit[sum][UNIT_RPD].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1]));
        cobox_unit[sum][UNIT_RHPD].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1]));
        cobox_unit[sum][UNIT_TRNLN].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1]));
        cobox_unit[sum][UNIT_TRNWD].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1]));
    } // end of method setValueAfterDel()

    void UpAction(int lsiv_unit) {
        String temp;
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            temp = cobox_unit[lsiv_unit][lsiv_field].getSelectedItem().toString();
            cobox_unit[lsiv_unit][lsiv_field].setSelectedItem(cobox_unit[lsiv_unit - 1][lsiv_field].getSelectedItem().toString());
            cobox_unit[lsiv_unit - 1][lsiv_field].setSelectedItem(temp);
        }
    } // end of method UpAction

    void DownAction(int lsiv_unit) {
        String temp;
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            temp = cobox_unit[lsiv_unit][lsiv_field].getSelectedItem().toString();
            cobox_unit[lsiv_unit][lsiv_field].setSelectedItem(cobox_unit[lsiv_unit + 1][lsiv_field].getSelectedItem().toString());
            cobox_unit[lsiv_unit + 1][lsiv_field].setSelectedItem(temp);
        }
    } // end of method DownAction

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
                    new HelpDialog(true, label_total.getText(), label_total.getText(), "The user cannot specify this item.  This item is the " + label_total.getText()
                            + " and is determined by the program.", cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NLS), "1");

                }
                else if (event.getSource() == cbo_veh_length) {
                    TX_Fmt lclv_tx_fmt_help = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH];
                    new HelpDialog(lclv_tx_fmt_help.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + veh_class], lclv_tx_fmt_help.mstv_name,
                            lclv_tx_fmt_help.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1 + veh_class], lclv_tx_fmt_help.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VEH_LENGTH_01 - 1
                                    + veh_class],
                            cbo_veh_length.getSelectedItem().toString(), cbo_veh_length.getSelectedItem().toString(), " ", cbo_veh_length.getSelectedItem().toString(),
                            cbo_veh_length.getSelectedItem().toString(), cbo_veh_length.getSelectedItem().toString());
                }
                else if (event.getSource() == cbo_veh_width) {
                    TX_Fmt lclv_tx_fmt_help = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH];
                    new HelpDialog(lclv_tx_fmt_help.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + veh_class], lclv_tx_fmt_help.mstv_name,
                            lclv_tx_fmt_help.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + veh_class],
                            lclv_tx_fmt_help.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VEH_WIDTH_01 - 1 + veh_class], cbo_veh_width.getSelectedItem().toString(), cbo_veh_width.getSelectedItem()
                                    .toString(),
                            " ", cbo_veh_width.getSelectedItem().toString(), cbo_veh_width.getSelectedItem().toString(), cbo_veh_width.getSelectedItem().toString());
                }
                else if (event.getSource() == cbo_veh_classification) {
                    TX_Fmt lclv_tx_fmt_help = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY];
                    new HelpDialog(lclv_tx_fmt_help.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + veh_class], lclv_tx_fmt_help.mstv_name.replace("#", Integer.toString(veh_class)),
                            lclv_tx_fmt_help.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1 + veh_class], lclv_tx_fmt_help.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VEH_CLASSIFY_01 - 1
                                    + veh_class],
                            cbo_veh_classification.getSelectedItem().toString(), cbo_veh_classification.getSelectedItem().toString(), cbo_veh_classification.getSelectedItem()
                                    .toString(),
                            " ", " ", " ");
                }

                for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
                    String name;
                    String desc;

                    if (event.getSource() == cobox_unit[lsiv_unit][UNIT_LEN]) {
                        name = lclv_tx_fmt.mstv_name.replace("#", Integer.toString(veh_class));
                        desc = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 - 1 + lsiv_unit].replace("#", Integer.toString(veh_class));

                        new HelpDialog(lclv_tx_fmt.mboa_def[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 - 1 + lsiv_unit], name, desc, lclv_tx_fmt.msta_help[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 - 1 + lsiv_unit],
                                cobox_unit[lsiv_unit][UNIT_LEN].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 - 1 + lsiv_unit]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 - 1 + lsiv_unit]), Integer.toString(lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 - 1
                                        + lsiv_unit]),
                                Integer.toString(lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_LEN_1 - 1 + lsiv_unit]));
                        break;
                    }
                    else if (event.getSource() == cobox_unit[lsiv_unit][UNIT_WID]) {
                        name = lclv_tx_fmt.mstv_name.replace("#", Integer.toString(veh_class));
                        desc = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 - 1 + lsiv_unit].replace("#", Integer.toString(veh_class));

                        new HelpDialog(lclv_tx_fmt.mboa_def[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 - 1 + lsiv_unit], name, desc, lclv_tx_fmt.msta_help[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 - 1 + lsiv_unit],
                                cobox_unit[lsiv_unit][UNIT_WID].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 - 1 + lsiv_unit]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 - 1 + lsiv_unit]), Integer.toString(lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 - 1
                                        + lsiv_unit]),
                                Integer.toString(lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_WID_1 - 1 + lsiv_unit]));
                        break;
                    }
                    else if (event.getSource() == cobox_unit[lsiv_unit][UNIT_DRWSQ]) {
                        name = lclv_tx_fmt.mstv_name.replace("#", Integer.toString(veh_class));
                        desc = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 - 1 + lsiv_unit].replace("#", Integer.toString(veh_class));

                        new HelpDialog(lclv_tx_fmt.mboa_def[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 - 1 + lsiv_unit], name, desc, lclv_tx_fmt.msta_help[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 - 1 + lsiv_unit],
                                cobox_unit[lsiv_unit][UNIT_DRWSQ].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 - 1 + lsiv_unit]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 - 1 + lsiv_unit]), Integer.toString(lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 - 1
                                        + lsiv_unit]),
                                Integer.toString(lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_DRWSQ_1 - 1 + lsiv_unit]));
                        break;
                    }
                    else if (event.getSource() == cobox_unit[lsiv_unit][UNIT_FPD]) {
                        name = lclv_tx_fmt.mstv_name.replace("#", Integer.toString(veh_class));
                        desc = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 - 1 + lsiv_unit].replace("#", Integer.toString(veh_class));

                        new HelpDialog(lclv_tx_fmt.mboa_def[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 - 1 + lsiv_unit], name, desc, lclv_tx_fmt.msta_help[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 - 1 + lsiv_unit],
                                cobox_unit[lsiv_unit][UNIT_FPD].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 - 1 + lsiv_unit]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 - 1 + lsiv_unit]), Integer.toString(lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 - 1
                                        + lsiv_unit]),
                                Integer.toString(lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_FPD_1 - 1 + lsiv_unit]));
                        break;
                    }
                    else if (event.getSource() == cobox_unit[lsiv_unit][UNIT_RPD]) {
                        name = lclv_tx_fmt.mstv_name.replace("#", Integer.toString(veh_class));
                        desc = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 - 1 + lsiv_unit].replace("#", Integer.toString(veh_class));

                        new HelpDialog(lclv_tx_fmt.mboa_def[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 - 1 + lsiv_unit], name, desc, lclv_tx_fmt.msta_help[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 - 1 + lsiv_unit],
                                cobox_unit[lsiv_unit][UNIT_RPD].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 - 1 + lsiv_unit]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 - 1 + lsiv_unit]), Integer.toString(lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 - 1
                                        + lsiv_unit]),
                                Integer.toString(lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_RPD_1 - 1 + lsiv_unit]));
                        break;
                    }
                    else if (event.getSource() == cobox_unit[lsiv_unit][UNIT_RHPD]) {
                        name = lclv_tx_fmt.mstv_name.replace("#", Integer.toString(veh_class));
                        desc = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 - 1 + lsiv_unit].replace("#", Integer.toString(veh_class));

                        new HelpDialog(lclv_tx_fmt.mboa_def[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 - 1 + lsiv_unit], name, desc, lclv_tx_fmt.msta_help[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 - 1 + lsiv_unit],
                                cobox_unit[lsiv_unit][UNIT_RHPD].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 - 1 + lsiv_unit]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 - 1 + lsiv_unit]), Integer.toString(lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 - 1
                                        + lsiv_unit]),
                                Integer.toString(lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_RHPD_1 - 1 + lsiv_unit]));
                        break;
                    }
                    else if (event.getSource() == cobox_unit[lsiv_unit][UNIT_TRNLN]) {
                        name = lclv_tx_fmt.mstv_name.replace("#", Integer.toString(veh_class));
                        desc = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 - 1 + lsiv_unit].replace("#", Integer.toString(veh_class));

                        new HelpDialog(lclv_tx_fmt.mboa_def[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 - 1 + lsiv_unit], name, desc, lclv_tx_fmt.msta_help[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 - 1 + lsiv_unit],
                                cobox_unit[lsiv_unit][UNIT_TRNLN].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 - 1 + lsiv_unit]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 - 1 + lsiv_unit]), Integer.toString(lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 - 1
                                        + lsiv_unit]),
                                Integer.toString(lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNLN_1 - 1 + lsiv_unit]));
                        break;
                    }
                    else if (event.getSource() == cobox_unit[lsiv_unit][UNIT_TRNWD]) {
                        name = lclv_tx_fmt.mstv_name.replace("#", Integer.toString(veh_class));
                        desc = lclv_tx_fmt.msta_desc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 - 1 + lsiv_unit].replace("#", Integer.toString(veh_class));

                        new HelpDialog(lclv_tx_fmt.mboa_def[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 - 1 + lsiv_unit], name, desc, lclv_tx_fmt.msta_help[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 - 1 + lsiv_unit],
                                cobox_unit[lsiv_unit][UNIT_TRNWD].getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 - 1 + lsiv_unit]), " ",
                                Integer.toString(lclv_tx_fmt.msia_min[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 - 1 + lsiv_unit]), Integer.toString(lclv_tx_fmt.msia_max[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 - 1
                                        + lsiv_unit]),
                                Integer.toString(lclv_tx_fmt.msia_inc[TX_FMT_GDV_VEH_UNITS_UNIT_TRNWD_1 - 1 + lsiv_unit]));
                        break;
                    }
                    else if (event.getSource() == up[lsiv_unit]) {
                        new HelpDialog(true, "Up Button", "The Up button swaps the data for the previous unit and the current unit.", " ", " ", " ", " ", " ", " ", " ");
                        break;
                    }
                    else if (event.getSource() == down[lsiv_unit]) {
                        new HelpDialog(true, "Down Button", "The Down button swaps the data for the next unit and the current unit.", " ", " ", " ", " ", " ", " ", " ");
                        break;
                    }
                    else if (event.getSource() == del[lsiv_unit]) {
                        new HelpDialog(true, "Delete Button", "The Delete button moves the data up 1 line for all units below this unit and decreases the Total units by 1.", " ", " ", " ", " ", " ",
                                " ", " ");
                        break;
                    }
                    else if (event.getSource() == add[lsiv_unit]) {
                        new HelpDialog(
                                true,
                                "Add Button",
                                "The Add button moves the data down 1 unit for all units below this unit, inserts a new unit at the current position, copies the values of all parameters from the previous units to the new unit, and increases the Total units by 1.",
                                " ", " ", " ", " ", " ", " ", " ");
                        break;
                    }
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_F1)
        }
    } // end of class HelpListener

    void saveData() {
        int numOfUnits = getNumberOfUnits();

        if (numOfUnits > 0) {
            gdvsim.veh_unit_num[veh_class] = numOfUnits;
            gdvsim.veh_unit_num_stat[veh_class] = gdvsim.gclv_inter.TX_FROM_USER;
        }
        else {
            gdvsim.veh_unit_num[veh_class] = default_veh_unit_num[veh_class];
            gdvsim.veh_unit_num_stat[veh_class] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }

        for (int lsiv_unit = 1; lsiv_unit <= numOfUnits; lsiv_unit++) {
            gdvsim.veh_unit_length[veh_class][lsiv_unit] = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_LEN].getSelectedItem().toString()).intValue();
            gdvsim.veh_unit_width[veh_class][lsiv_unit] = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_WID].getSelectedItem().toString()).intValue();
            gdvsim.veh_unit_draw_seq[veh_class][lsiv_unit] = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_DRWSQ].getSelectedItem().toString()).intValue();
            gdvsim.veh_unit_fpd[veh_class][lsiv_unit] = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_FPD].getSelectedItem().toString()).intValue();
            gdvsim.veh_unit_rpd[veh_class][lsiv_unit] = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_RPD].getSelectedItem().toString()).intValue();
            gdvsim.veh_unit_rhpd[veh_class][lsiv_unit] = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_RHPD].getSelectedItem().toString()).intValue();
            gdvsim.veh_trans_length[veh_class][lsiv_unit] = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_TRNLN].getSelectedItem().toString()).intValue();
            gdvsim.veh_trans_width[veh_class][lsiv_unit] = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_TRNWD].getSelectedItem().toString()).intValue();

            gdvsim.veh_unit_length_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.veh_unit_width_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.veh_unit_draw_seq_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.veh_unit_fpd_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.veh_unit_rpd_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.veh_unit_rhpd_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.veh_trans_length_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
            gdvsim.veh_trans_width_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        for (int lsiv_unit = numOfUnits + 1; lsiv_unit <= max_units; lsiv_unit++) {
            gdvsim.veh_unit_length[veh_class][lsiv_unit] = default_veh_unit_length[veh_class][lsiv_unit];
            gdvsim.veh_unit_width[veh_class][lsiv_unit] = default_veh_unit_width[veh_class][lsiv_unit];
            gdvsim.veh_unit_draw_seq[veh_class][lsiv_unit] = default_veh_unit_draw_seq[veh_class][lsiv_unit];
            gdvsim.veh_unit_fpd[veh_class][lsiv_unit] = default_veh_unit_fpd[veh_class][lsiv_unit];
            gdvsim.veh_unit_rpd[veh_class][lsiv_unit] = default_veh_unit_rpd[veh_class][lsiv_unit];
            gdvsim.veh_unit_rhpd[veh_class][lsiv_unit] = default_veh_unit_rhpd[veh_class][lsiv_unit];
            gdvsim.veh_trans_length[veh_class][lsiv_unit] = default_veh_trans_length[veh_class][lsiv_unit];
            gdvsim.veh_trans_width[veh_class][lsiv_unit] = default_veh_trans_width[veh_class][lsiv_unit];

            gdvsim.veh_unit_length_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_unit_width_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_unit_draw_seq_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_unit_fpd_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_unit_rpd_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_unit_rhpd_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_trans_length_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.veh_trans_width_stat[veh_class][lsiv_unit] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
        }
    } // end of method saveData()

    boolean isError() {
        int numOfUnits = getNumberOfUnits();

        int sum_unit_length;
        if (numOfUnits == 1) {
            sum_unit_length = Integer.valueOf(cobox_unit[1][UNIT_LEN].getSelectedItem().toString()).intValue();
        }
        else {
            sum_unit_length = Integer.valueOf(cobox_unit[1][UNIT_RHPD].getSelectedItem().toString()).intValue();
            for (int lsiv_unit = 2; lsiv_unit <= (numOfUnits - 1); lsiv_unit++) {
                sum_unit_length += (Integer.valueOf(cobox_unit[lsiv_unit][UNIT_RHPD].getSelectedItem().toString()).intValue() - Integer.valueOf(
                        cobox_unit[lsiv_unit][UNIT_FPD].getSelectedItem().toString()).intValue());
            }
            sum_unit_length += (Integer.valueOf(cobox_unit[numOfUnits][UNIT_LEN].getSelectedItem().toString()).intValue() - Integer.valueOf(
                    cobox_unit[numOfUnits][UNIT_FPD].getSelectedItem().toString()).intValue());
            if (sum_unit_length != veh_length) {
                JOptionPane.showMessageDialog(null, "The sum of " + label_field[UNIT_LEN].getText() + " = " + sum_unit_length + " should be equal to the length of the vehicle = " + veh_length + ".",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }

        int minDrawingSequence = 99;
        int maxDrawingSequence = 0;
        boolean[] drawingSequenceUsed = new boolean[PARAMS.TEXAS_MODEL_MNU + 1];
        for (int lsiv_draw_seq = 1; lsiv_draw_seq <= PARAMS.TEXAS_MODEL_MNU; lsiv_draw_seq++) {
            drawingSequenceUsed[lsiv_draw_seq] = false;
        }
        for (int lsiv_unit = 1; lsiv_unit <= numOfUnits; lsiv_unit++) {
            int drawSequence = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_DRWSQ].getSelectedItem().toString()).intValue();
            if (drawSequence < minDrawingSequence)
                minDrawingSequence = drawSequence;
            if (drawSequence > maxDrawingSequence)
                maxDrawingSequence = drawSequence;
            drawingSequenceUsed[drawSequence] = true;
        }
        if (minDrawingSequence != 1) {
            JOptionPane.showMessageDialog(null, label_field[UNIT_DRWSQ].getText() + " minimum value = " + minDrawingSequence + " is not 1.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }
        for (int lsiv_draw_seq = 1; lsiv_draw_seq <= maxDrawingSequence; lsiv_draw_seq++) {
            if (!drawingSequenceUsed[lsiv_draw_seq]) {
                JOptionPane.showMessageDialog(null, label_field[UNIT_DRWSQ].getText() + " " + lsiv_draw_seq + " is not used.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }

        int u_len_max = 0;
        int u_len_sum = 0;
        for (int lsiv_unit = 1; lsiv_unit <= numOfUnits; lsiv_unit++) {
            int u_len = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_LEN].getSelectedItem().toString()).intValue();
            int u_wid = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_WID].getSelectedItem().toString()).intValue();
            int u_fpd = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_FPD].getSelectedItem().toString()).intValue();
            int u_rpd = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_RPD].getSelectedItem().toString()).intValue();
            int u_rhpd = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_RHPD].getSelectedItem().toString()).intValue();
            int u_trnln = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_TRNLN].getSelectedItem().toString()).intValue();
            int u_trnwd = Integer.valueOf(cobox_unit[lsiv_unit][UNIT_TRNWD].getSelectedItem().toString()).intValue();

            u_len_max = veh_length - u_len_sum - 3 * (numOfUnits - lsiv_unit);
            if (lsiv_unit >= 2) {
                u_len_max += u_fpd;
            }

            if (u_len > u_len_max) {
                JOptionPane.showMessageDialog(null, "For Unit " + lsiv_unit + ", " + label_field[UNIT_LEN].getText() + " = " + u_len + " should be less than or equal to " + u_len_max + ".",
                        "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (u_wid > veh_width) {
                JOptionPane.showMessageDialog(null, "For Unit " + lsiv_unit + ", " + label_field[UNIT_WID].getText() + " = " + u_wid + " should be less than or equal to the width of the vehicle = "
                        + veh_width + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (u_fpd > u_len) {
                JOptionPane.showMessageDialog(null, "For Unit " + lsiv_unit + ", " + label_field[UNIT_FPD].getText() + " = " + u_fpd + " should be less than or equal to the unit length = " + u_len
                        + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (u_rpd > u_len) {
                JOptionPane.showMessageDialog(null, "For Unit " + lsiv_unit + ", " + label_field[UNIT_RPD].getText() + " = " + u_rpd + " should be less than or equal to the unit length = " + u_len
                        + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (u_rhpd > u_len) {
                JOptionPane.showMessageDialog(null, "For Unit " + lsiv_unit + ", " + label_field[UNIT_RHPD].getText() + " = " + u_rhpd + " should be less than or equal to the unit length = " + u_len
                        + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (u_trnln > u_len) {
                JOptionPane.showMessageDialog(null, "For Unit " + lsiv_unit + ", " + label_field[UNIT_TRNLN].getText() + " = " + u_trnln + " should be less than or equal to the unit length = "
                        + u_len + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (u_trnwd > u_wid) {
                JOptionPane.showMessageDialog(null, "For Unit " + lsiv_unit + ", " + label_field[UNIT_TRNWD].getText() + " = " + u_trnwd + " should be less than or equal to the unit width = " + u_wid
                        + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (u_fpd >= u_rpd) {
                JOptionPane.showMessageDialog(null, "For Unit " + lsiv_unit + ", " + label_field[UNIT_FPD].getText() + " = " + u_fpd + " should be less than " + label_field[UNIT_RPD].getText()
                        + " = " + u_rpd + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if ((lsiv_unit < numOfUnits) && (u_fpd >= u_rhpd)) {
                JOptionPane.showMessageDialog(null, "For Unit " + lsiv_unit + ", " + label_field[UNIT_FPD].getText() + " = " + u_fpd + " should be less than " + label_field[UNIT_RHPD].getText()
                        + " = " + u_rhpd + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if ((u_trnln == 0) && (u_trnwd > 0)) {
                JOptionPane.showMessageDialog(null, "For Unit " + lsiv_unit + ", " + label_field[UNIT_TRNLN].getText() + " = zero and " + label_field[UNIT_TRNWD].getText() + " = " + u_trnwd
                        + " which should be zero.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if ((u_trnwd == 0) && (u_trnln > 0)) {
                JOptionPane.showMessageDialog(null, "For Unit " + lsiv_unit + ", " + label_field[UNIT_TRNWD].getText() + " = zero and " + label_field[UNIT_TRNLN].getText() + " = " + u_trnln
                        + " which should be zero.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (lsiv_unit == 1) {
                u_len_sum += (u_rhpd);
            }
            else {
                u_len_sum += (u_rhpd - u_fpd);
            }
        }
        return false;
    } // end of method isError()

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    if (veh_class > PARAMS.TEXAS_MODEL_NVCD) {
                        saveData();
                    }

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }
            for (int i = 1; i <= NUMOFFIELD; i++) {
                if (event.getSource() == cobox_unit[i][UNIT_LEN]) {
                    index = i;
                    type = UNIT_LEN;
                    draw_panel.repaint();
                    break;
                }
            }

            for (int i = 1; i <= NUMOFFIELD; i++) {
                if (event.getSource() == cobox_unit[i][UNIT_RHPD]) {
                    index = i;
                    type = UNIT_RHPD;
                    draw_panel.repaint();
                    break;
                }
            }

            for (int i = 1; i <= NUMOFFIELD; i++) {
                if (event.getSource() == cobox_unit[i][UNIT_RPD]) {
                    index = i;
                    type = UNIT_RPD;
                    draw_panel.repaint();
                    break;
                }
            }

            for (int i = 1; i <= NUMOFFIELD; i++) {
                if (event.getSource() == cobox_unit[i][UNIT_FPD]) {
                    index = i;
                    type = UNIT_FPD;
                    draw_panel.repaint();
                    break;
                }
            }

            for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
                if (event.getSource() == down[lsiv_unit]) {
                    DownAction(lsiv_unit);
                    type = -3;
                    index = lsiv_unit;
                    draw_panel.repaint();
                    break;
                }
                else if (event.getSource() == up[lsiv_unit]) {
                    UpAction(lsiv_unit);
                    type = -2;
                    index = lsiv_unit;
                    draw_panel.repaint();
                    break;
                }
                else if (event.getSource() == del[lsiv_unit]) {
                    if (event.getSource() == del[lsiv_unit]) {
                        int numOfUnitsBeforeClick;
                        numOfUnitsBeforeClick = getNumberOfUnits();
                        setStatus(numOfUnitsBeforeClick - 1);
                        setValueAfterDel(numOfUnitsBeforeClick, lsiv_unit);

                        act = 1;
                        type = 0;
                        index = lsiv_unit;

                        if (lengths_list.size() >= lsiv_unit)
                            lengths_list.remove(lsiv_unit - 1);
                        if (rhpds_list.size() >= lsiv_unit)
                            rhpds_list.remove(lsiv_unit - 1);
                        if (rpds_list.size() >= lsiv_unit)
                            rpds_list.remove(lsiv_unit - 1);
                        if (fpds_list.size() >= lsiv_unit)
                            fpds_list.remove(lsiv_unit - 1);

                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfUnits()));

                        if (!del[lsiv_unit].isEnabled()) {
                            okButton.requestFocus();
                        }

                        draw_panel.repaint();
                        break;
                    }
                }
                else if (event.getSource() == add[lsiv_unit]) {
                    int numOfUnitsBeforeClick;
                    numOfUnitsBeforeClick = getNumberOfUnits();
                    setStatus(numOfUnitsBeforeClick + 1);
                    setValueAfterAdd(numOfUnitsBeforeClick, lsiv_unit);

                    type = -1;
                    index = lsiv_unit;

                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfUnits()));
                    draw_panel.repaint();
                    break;
                }

            }
        } // end of method actionPerformed
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    if (!isError()) {
                        if (veh_class > PARAMS.TEXAS_MODEL_NVCD) {
                            saveData();
                        }

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_unit = 1; lsiv_unit <= max_units; lsiv_unit++) {
                    if (event.getSource() == down[lsiv_unit]) {
                        DownAction(lsiv_unit);
                        break;
                    }
                    else if (event.getSource() == up[lsiv_unit]) {
                        UpAction(lsiv_unit);
                        break;
                    }
                    else if (event.getSource() == del[lsiv_unit]) {
                        if (event.getSource() == del[lsiv_unit]) {
                            int numOfUnitsBeforeClick;
                            numOfUnitsBeforeClick = getNumberOfUnits();
                            setStatus(numOfUnitsBeforeClick - 1);
                            setValueAfterDel(numOfUnitsBeforeClick, lsiv_unit);

                            cbo_total.removeAllItems();
                            cbo_total.addItem(Integer.toString(getNumberOfUnits()));

                            if (!del[lsiv_unit].isEnabled()) {
                                okButton.requestFocus();
                            }
                            break;
                        }
                    }
                    else if (event.getSource() == add[lsiv_unit]) {
                        int numOfUnitsBeforeClick;
                        numOfUnitsBeforeClick = getNumberOfUnits();
                        setStatus(numOfUnitsBeforeClick + 1);
                        setValueAfterAdd(numOfUnitsBeforeClick, lsiv_unit);

                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfUnits()));
                        break;
                    }
                }
            }
        } // end of method actionPerformed
    } // end of class ComponentKeyListener

} // end of class VehicleUnitDialog
