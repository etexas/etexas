package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                       SimulationDataDialog.java                            */
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
import java.text.DecimalFormat;
import java.util.*;
import javax.swing.JOptionPane;

class SimulationDataDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstr;

    JButton okButton, applyButton, cancelButton, VMSmesgButton;

    JTextField text_sim_title;

    JCheckBox acceptDefaultCheckbox;

    JLabel label_sim_title, label_SIM_Par_Opt, label_SIM_Par_Opt_2,

            label_mdfv_start_time, label_mdfv_sim_time, label_mdfv_dt, label_mstv_inter_control, label_mstv_turn_sum, label_mstv_appro_sum, label_mstv_compressed, label_mstv_pva_data,
            label_msiv_pva_end,
            label_mstv_wide, label_mstv_lt_pullout, label_mdfv_dilemma_zone_time_beg, label_mdfv_dilemma_zone_time_end, label_msiv_hils_sleep_time,

            label_msiv_ds_speed, label_msiv_queue_dist, label_mdfv_lambda, label_mdfv_mu, label_msiv_alpha, label_mdfv_t_lead, label_mdfv_t_lag, label_mdfv_hesitation_factor, label_mstv_majcol_stop,
            label_msiv_majcol_percent_des_speed, label_msiv_majcol_evasive_act_mean, label_msiv_majcol_evasive_act_stddev, label_mstv_enable_ssam_file;

    JComboBox cbobx_mdfv_start_time, cbobx_mdfv_sim_time, cbobx_mdfv_dt, cbobx_mstv_inter_control, cbobx_mstv_turn_sum, cbobx_mstv_appro_sum, cbobx_mstv_compressed, cbobx_mstv_pva_data,
            cbobx_msiv_pva_end, cbobx_mstv_wide, cbobx_mstv_lt_pullout, cbobx_mdfv_dilemma_zone_time_beg, cbobx_mdfv_dilemma_zone_time_end, cbobx_msiv_hils_sleep_time,

            cbobx_msiv_ds_speed, cbobx_msiv_queue_dist, cbobx_mdfv_lambda, cbobx_mdfv_mu, cbobx_msiv_alpha, cbobx_mdfv_t_lead, cbobx_mdfv_t_lag, cbobx_mdfv_hesitation_factor, cbobx_mstv_majcol_stop,
            cbobx_msiv_majcol_percent_des_speed, cbobx_msiv_majcol_evasive_act_mean, cbobx_msiv_majcol_evasive_act_stddev, cbobx_mstv_enable_ssam_file;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    HelpListener helpListener;

    OpenComboMenuListener openComboMenuListener;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    DecimalFormat oneDigits = new DecimalFormat("0.0");

    DecimalFormat twoDigits = new DecimalFormat("0.00");

    DecimalFormat threeDigits = new DecimalFormat("0.000");

    public SimulationDataDialog() {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];

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

        Font font1 = new Font("TimesRoman", Font.BOLD, 16);

        int lsiv_min;
        int lsiv_max;
        int lsiv_inc;

        double lsiv_min_double;
        double lsiv_max_double;
        double lsiv_inc_double;

        String stringToTokenizer;
        StringTokenizer tokens;

        int arrayIndex, intArrayElementValue, SizeOfArray;
        double doubleArrayElementValue;

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_mdfv_start_time = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_mdfv_start_time[arrayIndex] = oneDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_mdfv_sim_time = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_mdfv_sim_time[arrayIndex] = oneDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_mdfv_dt = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_mdfv_dt[arrayIndex] = twoDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        stringToTokenizer = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_INTER_CONTROL];
        tokens = new StringTokenizer(stringToTokenizer, "|");

        SizeOfArray = tokens.countTokens();
        if (!gdvsim.gclv_inter.mbov_is_diamond_interchange)
            SizeOfArray--;
        String[] array_mstv_inter_control = new String[SizeOfArray];
        arrayIndex = 0;
        while (tokens.hasMoreTokens()) {
            String token = tokens.nextToken();
            if ((!gdvsim.gclv_inter.mbov_is_diamond_interchange) && (token.equals("TEX-DIA")))
                continue;
            array_mstv_inter_control[arrayIndex++] = token;
        }

        String[] array_mstv_turn_sum = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_TURN_SUM].substring(1).split("\\|");
        String[] array_mstv_appro_sum = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_APPRO_SUM].substring(1).split("\\|");
        String[] array_mstv_compressed = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_COMPRESSED].substring(1).split("\\|");
        String[] array_mstv_pva_data = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_DATA].substring(1).split("\\|");

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END];

        SizeOfArray = (int)((lsiv_max - lsiv_min) / lsiv_inc) + 1;
        intArrayElementValue = lsiv_min;
        String[] array_msiv_pva_end = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_msiv_pva_end[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        String[] array_mstv_wide = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_WIDE].substring(1).split("\\|");
        String[] array_mstv_lt_pullout = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_LT_PULLOUT].substring(1).split("\\|");

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_mdfv_dilemma_zone_time_beg = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_mdfv_dilemma_zone_time_beg[arrayIndex] = oneDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_mdfv_dilemma_zone_time_end = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_mdfv_dilemma_zone_time_end[arrayIndex] = oneDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_msiv_hils_sleep_time = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_msiv_hils_sleep_time[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_msiv_ds_speed = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_msiv_ds_speed[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_msiv_queue_dist = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_msiv_queue_dist[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_mdfv_lambda = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_mdfv_lambda[arrayIndex] = threeDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_mdfv_mu = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_mdfv_mu[arrayIndex] = threeDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_msiv_alpha = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_msiv_alpha[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_mdfv_t_lead = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_mdfv_t_lead[arrayIndex] = twoDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_mdfv_t_lag = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_mdfv_t_lag[arrayIndex] = twoDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_mdfv_hesitation_factor = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_mdfv_hesitation_factor[arrayIndex] = oneDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        String[] array_mstv_majcol_stop = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP].substring(1).split("\\|");

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] arry_msiv_majcol_percent_des_speed = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            arry_msiv_majcol_percent_des_speed[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_msiv_majcol_evasive_act_mean = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_msiv_majcol_evasive_act_mean[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        lsiv_min = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD];
        lsiv_max = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD];
        lsiv_inc = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD];

        SizeOfArray = (lsiv_max - lsiv_min) / lsiv_inc + 1;
        intArrayElementValue = lsiv_min;
        String[] array_msiv_majcol_evasive_act_stddev = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_msiv_majcol_evasive_act_stddev[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc;
        }

        String[] array_mstv_enable_ssam_file = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE].substring(1).split("\\|");

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_TITLE];

        label_sim_title = new JLabel(lclv_tx_fmt.mstv_name + " (" + lclv_tx_fmt.msia_fs[gdvsim.gclv_inter.TX_FMT_SIM_TITLE_TEXT] + " max)");
        label_sim_title.setFont(font1);

        text_sim_title = new JTextField(lclv_tx_fmt.msia_fs[gdvsim.gclv_inter.TX_FMT_SIM_TITLE_TEXT]);

        label_sim_title.setLabelFor(text_sim_title);

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_title.mclv_text.mstv_card != null) {
            text_sim_title.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_title.mclv_text.mstv_card.trim());
        }
        else {
            text_sim_title.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_title.mclv_text.mstv_card.trim());
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];

        label_SIM_Par_Opt = new JLabel(lclv_tx_fmt.mstv_name);
        label_SIM_Par_Opt.setFont(font1);

        label_mdfv_start_time = new JLabel(" 1. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME]);
        label_mdfv_sim_time = new JLabel(" 2. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME]);
        label_mdfv_dt = new JLabel(" 3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT]);
        label_mstv_inter_control = new JLabel(" 4. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_INTER_CONTROL]);
        label_mstv_turn_sum = new JLabel(" 5. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_TURN_SUM]);
        label_mstv_appro_sum = new JLabel(" 6. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_APPRO_SUM]);
        label_mstv_compressed = new JLabel(" 7. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_COMPRESSED]);
        label_mstv_pva_data = new JLabel(" 8. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_DATA]);
        label_msiv_pva_end = new JLabel(" 9. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END]);
        label_mstv_wide = new JLabel("10. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_WIDE]);
        label_mstv_lt_pullout = new JLabel("11. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_LT_PULLOUT]);
        label_mdfv_dilemma_zone_time_beg = new JLabel("12. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT]);
        label_mdfv_dilemma_zone_time_end = new JLabel("13. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT]);
        label_msiv_hils_sleep_time = new JLabel("14. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME]);

        cbobx_mdfv_start_time = new JComboBox(array_mdfv_start_time);
        cbobx_mdfv_sim_time = new JComboBox(array_mdfv_sim_time);
        cbobx_mdfv_dt = new JComboBox(array_mdfv_dt);
        cbobx_mstv_inter_control = new JComboBox(array_mstv_inter_control);
        cbobx_mstv_turn_sum = new JComboBox(array_mstv_turn_sum);
        cbobx_mstv_appro_sum = new JComboBox(array_mstv_appro_sum);
        cbobx_mstv_compressed = new JComboBox(array_mstv_compressed);
        cbobx_mstv_pva_data = new JComboBox(array_mstv_pva_data);
        cbobx_msiv_pva_end = new JComboBox(array_msiv_pva_end);
        cbobx_mstv_wide = new JComboBox(array_mstv_wide);
        cbobx_mstv_lt_pullout = new JComboBox(array_mstv_lt_pullout);
        cbobx_mdfv_dilemma_zone_time_beg = new JComboBox(array_mdfv_dilemma_zone_time_beg);
        cbobx_mdfv_dilemma_zone_time_end = new JComboBox(array_mdfv_dilemma_zone_time_end);
        cbobx_msiv_hils_sleep_time = new JComboBox(array_msiv_hils_sleep_time);

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            cbobx_mstv_inter_control.setMaximumRowCount(10);
        }
        else {
            cbobx_mstv_inter_control.setMaximumRowCount(9);
        }

        if (gdvsim.flag_simulationData_ok) {
            cbobx_mdfv_start_time.setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_start_time));
            cbobx_mdfv_sim_time.setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_sim_time));
            cbobx_mdfv_dt.setSelectedItem(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dt));
            cbobx_mstv_inter_control.setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            cbobx_mstv_turn_sum.setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_turn_sum);
            cbobx_mstv_appro_sum.setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_appro_sum);
            cbobx_mstv_compressed.setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_compressed);
            cbobx_mstv_pva_data.setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_pva_data);
            cbobx_msiv_pva_end.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_pva_end));
            cbobx_mstv_wide.setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_wide);
            cbobx_mstv_lt_pullout.setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_lt_pullout);
            cbobx_mdfv_dilemma_zone_time_beg.setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dilemma_zone_time_beg));
            cbobx_mdfv_dilemma_zone_time_end.setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dilemma_zone_time_end));
            cbobx_msiv_hils_sleep_time.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_hitl_sleep_time));
        }
        else {
            cbobx_mdfv_start_time.setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME]));
            cbobx_mdfv_sim_time.setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME]));
            cbobx_mdfv_dt.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT]));
            cbobx_mstv_inter_control.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_INTER_CONTROL]);
            cbobx_mstv_turn_sum.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_TURN_SUM]);
            cbobx_mstv_appro_sum.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_APPRO_SUM]);
            cbobx_mstv_compressed.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_COMPRESSED]);
            cbobx_mstv_pva_data.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_DATA]);
            cbobx_msiv_pva_end.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END]));
            cbobx_mstv_wide.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_WIDE]);
            cbobx_mstv_lt_pullout.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_LT_PULLOUT]);
            cbobx_mdfv_dilemma_zone_time_beg.setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT]));
            cbobx_mdfv_dilemma_zone_time_end.setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT]));
            cbobx_msiv_hils_sleep_time.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME]));
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];

        label_SIM_Par_Opt_2 = new JLabel(lclv_tx_fmt.mstv_name);
        label_SIM_Par_Opt_2.setFont(font1);

        label_msiv_ds_speed = new JLabel(" 1. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED]);
        label_msiv_queue_dist = new JLabel(" 2. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST]);
        label_mdfv_lambda = new JLabel(" 3. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA]);
        label_mdfv_mu = new JLabel(" 4. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU]);
        label_msiv_alpha = new JLabel(" 5. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA]);
        label_mdfv_t_lead = new JLabel(" 6. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD]);
        label_mdfv_t_lag = new JLabel(" 7. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG]);
        label_mdfv_hesitation_factor = new JLabel(" 8. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT]);
        label_mstv_majcol_stop = new JLabel(" 9. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP]);
        label_msiv_majcol_percent_des_speed = new JLabel("10. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD]);
        label_msiv_majcol_evasive_act_mean = new JLabel("11. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN]);
        label_msiv_majcol_evasive_act_stddev = new JLabel("12. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD]);
        label_mstv_enable_ssam_file = new JLabel("13. " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE]);

        cbobx_msiv_ds_speed = new JComboBox(array_msiv_ds_speed);
        cbobx_msiv_queue_dist = new JComboBox(array_msiv_queue_dist);
        cbobx_mdfv_lambda = new JComboBox(array_mdfv_lambda);
        cbobx_mdfv_mu = new JComboBox(array_mdfv_mu);
        cbobx_msiv_alpha = new JComboBox(array_msiv_alpha);
        cbobx_mdfv_t_lead = new JComboBox(array_mdfv_t_lead);
        cbobx_mdfv_t_lag = new JComboBox(array_mdfv_t_lag);
        cbobx_mdfv_hesitation_factor = new JComboBox(array_mdfv_hesitation_factor);
        cbobx_mstv_majcol_stop = new JComboBox(array_mstv_majcol_stop);
        cbobx_msiv_majcol_percent_des_speed = new JComboBox(arry_msiv_majcol_percent_des_speed);
        cbobx_msiv_majcol_evasive_act_mean = new JComboBox(array_msiv_majcol_evasive_act_mean);
        cbobx_msiv_majcol_evasive_act_stddev = new JComboBox(array_msiv_majcol_evasive_act_stddev);
        cbobx_mstv_enable_ssam_file = new JComboBox(array_mstv_enable_ssam_file);

        if (gdvsim.flag_simulationData_ok) {
            cbobx_msiv_ds_speed.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_ds_speed));
            cbobx_msiv_queue_dist.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_queue_dist));
            cbobx_mdfv_lambda.setSelectedItem(threeDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_lambda));
            cbobx_mdfv_mu.setSelectedItem(threeDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_mu));
            cbobx_msiv_alpha.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_alpha));
            cbobx_mdfv_t_lead.setSelectedItem(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_t_lead));
            cbobx_mdfv_t_lag.setSelectedItem(twoDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_t_lag));
            cbobx_mdfv_hesitation_factor.setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_hesitation_factor));
            cbobx_mstv_majcol_stop.setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mstv_majcol_stop);
            cbobx_msiv_majcol_percent_des_speed.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_majcol_percent_des_speed));
            cbobx_msiv_majcol_evasive_act_mean.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_majcol_evasive_act_mean));
            cbobx_msiv_majcol_evasive_act_stddev.setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_majcol_evasive_act_stddev));
            cbobx_mstv_enable_ssam_file.setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mstv_enable_ssam_file);
        }
        else {
            cbobx_msiv_ds_speed.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED]));
            cbobx_msiv_queue_dist.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST]));
            cbobx_mdfv_lambda.setSelectedItem(threeDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA]));
            cbobx_mdfv_mu.setSelectedItem(threeDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU]));
            cbobx_msiv_alpha.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA]));
            cbobx_mdfv_t_lead.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD]));
            cbobx_mdfv_t_lag.setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG]));
            cbobx_mdfv_hesitation_factor.setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT]));
            cbobx_mstv_majcol_stop.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP]);
            cbobx_msiv_majcol_percent_des_speed.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD]));
            cbobx_msiv_majcol_evasive_act_mean.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN]));
            cbobx_msiv_majcol_evasive_act_stddev.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD]));
            cbobx_mstv_enable_ssam_file.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE]);
        }

        acceptDefaultCheckbox = new JCheckBox();
        acceptDefaultCheckbox.setText("Accept All Simulation Default Data Values (Intersection Control UNCONTRL and ALL-STOP only)");
        acceptDefaultCheckbox.setSelected(false);

        acceptDefaultCheckbox.setMnemonic(KeyEvent.VK_D);

        if (cbobx_mstv_inter_control.getSelectedItem().toString().equals("UNCONTRL") || cbobx_mstv_inter_control.getSelectedItem().toString().equals("ALL-STOP")) {
            acceptDefaultCheckbox.setEnabled(true);
            acceptDefaultCheckbox.setVisible(true);
        }
        else {
            acceptDefaultCheckbox.setEnabled(false);
            acceptDefaultCheckbox.setVisible(false);
        }

        if (cbobx_mstv_inter_control.getSelectedItem().toString().equals("UNCONTRL") || cbobx_mstv_inter_control.getSelectedItem().toString().equals("YIELD")
                || cbobx_mstv_inter_control.getSelectedItem().toString().equals("STOP") || cbobx_mstv_inter_control.getSelectedItem().toString().equals("ALL-STOP"))

        {
            cbobx_mdfv_dilemma_zone_time_beg.setVisible(false);
            cbobx_mdfv_dilemma_zone_time_end.setVisible(false);
            cbobx_msiv_hils_sleep_time.setVisible(false);

            label_mdfv_dilemma_zone_time_beg.setVisible(false);
            label_mdfv_dilemma_zone_time_end.setVisible(false);
            label_msiv_hils_sleep_time.setVisible(false);
        }
        else if (cbobx_mstv_inter_control.getSelectedItem().toString().equals("PRETIMED") || cbobx_mstv_inter_control.getSelectedItem().toString().equals("SEMI-ACT")
                || cbobx_mstv_inter_control.getSelectedItem().toString().equals("FULL-ACT") || cbobx_mstv_inter_control.getSelectedItem().toString().equals("TEX-DIA")
                || cbobx_mstv_inter_control.getSelectedItem().toString().equals("NEMA")) {
            cbobx_mdfv_dilemma_zone_time_beg.setVisible(true);
            cbobx_mdfv_dilemma_zone_time_end.setVisible(true);
            cbobx_msiv_hils_sleep_time.setVisible(false);

            label_mdfv_dilemma_zone_time_beg.setVisible(true);
            label_mdfv_dilemma_zone_time_end.setVisible(true);
            label_msiv_hils_sleep_time.setVisible(false);
        }
        else if (cbobx_mstv_inter_control.getSelectedItem().toString().equals("HARDWARE")) {
            cbobx_mdfv_dilemma_zone_time_beg.setVisible(true);
            cbobx_mdfv_dilemma_zone_time_end.setVisible(true);
            cbobx_msiv_hils_sleep_time.setVisible(true);

            label_mdfv_dilemma_zone_time_beg.setVisible(true);
            label_mdfv_dilemma_zone_time_end.setVisible(true);
            label_msiv_hils_sleep_time.setVisible(true);
        }

        okButton = new JButton("  OK  ");
        applyButton = new JButton(" Apply");
        cancelButton = new JButton("Cancel");
        VMSmesgButton = new JButton("VMS Message Data");

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);
        VMSmesgButton.setMnemonic(KeyEvent.VK_V);

        JPanel ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        JPanel panel_accept = new JPanel();
        panel_accept.add(acceptDefaultCheckbox);

        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();

        cbobx_mdfv_start_time.addKeyListener(openComboMenuListener);
        cbobx_mdfv_sim_time.addKeyListener(openComboMenuListener);
        cbobx_mdfv_dt.addKeyListener(openComboMenuListener);
        cbobx_mstv_inter_control.addKeyListener(openComboMenuListener);
        cbobx_mstv_turn_sum.addKeyListener(openComboMenuListener);
        cbobx_mstv_appro_sum.addKeyListener(openComboMenuListener);
        cbobx_mstv_compressed.addKeyListener(openComboMenuListener);
        cbobx_mstv_pva_data.addKeyListener(openComboMenuListener);
        cbobx_msiv_pva_end.addKeyListener(openComboMenuListener);
        cbobx_mstv_wide.addKeyListener(openComboMenuListener);
        cbobx_mstv_lt_pullout.addKeyListener(openComboMenuListener);
        cbobx_mdfv_dilemma_zone_time_beg.addKeyListener(openComboMenuListener);
        cbobx_mdfv_dilemma_zone_time_end.addKeyListener(openComboMenuListener);
        cbobx_msiv_hils_sleep_time.addKeyListener(openComboMenuListener);

        cbobx_mdfv_start_time.addKeyListener(helpListener);
        cbobx_mdfv_sim_time.addKeyListener(helpListener);
        cbobx_mdfv_dt.addKeyListener(helpListener);
        cbobx_mstv_inter_control.addKeyListener(helpListener);
        cbobx_mstv_turn_sum.addKeyListener(helpListener);
        cbobx_mstv_appro_sum.addKeyListener(helpListener);
        cbobx_mstv_compressed.addKeyListener(helpListener);
        cbobx_mstv_pva_data.addKeyListener(helpListener);
        cbobx_msiv_pva_end.addKeyListener(helpListener);
        cbobx_mstv_wide.addKeyListener(helpListener);
        cbobx_mstv_lt_pullout.addKeyListener(helpListener);
        cbobx_mdfv_dilemma_zone_time_beg.addKeyListener(helpListener);
        cbobx_mdfv_dilemma_zone_time_end.addKeyListener(helpListener);
        cbobx_msiv_hils_sleep_time.addKeyListener(helpListener);

        cbobx_msiv_ds_speed.addKeyListener(openComboMenuListener);
        cbobx_msiv_queue_dist.addKeyListener(openComboMenuListener);
        cbobx_mdfv_lambda.addKeyListener(openComboMenuListener);
        cbobx_mdfv_mu.addKeyListener(openComboMenuListener);
        cbobx_msiv_alpha.addKeyListener(openComboMenuListener);
        cbobx_mdfv_t_lead.addKeyListener(openComboMenuListener);
        cbobx_mdfv_t_lag.addKeyListener(openComboMenuListener);
        cbobx_mdfv_hesitation_factor.addKeyListener(openComboMenuListener);
        cbobx_mstv_majcol_stop.addKeyListener(openComboMenuListener);
        cbobx_msiv_majcol_percent_des_speed.addKeyListener(openComboMenuListener);
        cbobx_msiv_majcol_evasive_act_mean.addKeyListener(openComboMenuListener);
        cbobx_msiv_majcol_evasive_act_stddev.addKeyListener(openComboMenuListener);
        cbobx_mstv_enable_ssam_file.addKeyListener(openComboMenuListener);

        cbobx_msiv_ds_speed.addKeyListener(helpListener);
        cbobx_msiv_queue_dist.addKeyListener(helpListener);
        cbobx_mdfv_lambda.addKeyListener(helpListener);
        cbobx_mdfv_mu.addKeyListener(helpListener);
        cbobx_msiv_alpha.addKeyListener(helpListener);
        cbobx_mdfv_t_lead.addKeyListener(helpListener);
        cbobx_mdfv_t_lag.addKeyListener(helpListener);
        cbobx_mdfv_hesitation_factor.addKeyListener(helpListener);
        cbobx_mstv_majcol_stop.addKeyListener(helpListener);
        cbobx_msiv_majcol_percent_des_speed.addKeyListener(helpListener);
        cbobx_msiv_majcol_evasive_act_mean.addKeyListener(helpListener);
        cbobx_msiv_majcol_evasive_act_stddev.addKeyListener(helpListener);
        cbobx_mstv_enable_ssam_file.addKeyListener(helpListener);

        VMSmesgButton.addActionListener(componentActionListener);
        VMSmesgButton.addKeyListener(componentKeyListener);
        VMSmesgButton.addKeyListener(helpListener);

        acceptDefaultCheckbox.addKeyListener(componentKeyListener);
        acceptDefaultCheckbox.addKeyListener(helpListener);

        text_sim_title.addKeyListener(helpListener);

        cbobx_mstv_inter_control.addActionListener(componentActionListener);
        cbobx_mstv_inter_control.addKeyListener(componentKeyListener);

        okButton.addActionListener(componentActionListener);
        applyButton.addActionListener(componentActionListener);
        cancelButton.addActionListener(componentActionListener);

        okButton.addKeyListener(componentKeyListener);
        applyButton.addKeyListener(componentKeyListener);
        cancelButton.addKeyListener(componentKeyListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        setAccessiblity();

        int iRow = 0;

        gbConstr.fill = GridBagConstraints.BOTH;

        gbConstr.insets = new Insets(20, 2, 2, 2);
        addComponent(label_sim_title, iRow, 0, 1, 1);
        addComponent(text_sim_title, iRow++, 1, 4, 1);

        gbConstr.insets = new Insets(2, 2, 2, 2);

        addComponent(label_SIM_Par_Opt, iRow, 0, 3, 1);
        addComponent(label_SIM_Par_Opt_2, iRow++, 3, 2, 1);

        addComponent(cbobx_mdfv_start_time, iRow, 0, 1, 1);
        addComponent(label_mdfv_start_time, iRow, 1, 2, 1);
        addComponent(cbobx_msiv_ds_speed, iRow, 3, 1, 1);
        addComponent(label_msiv_ds_speed, iRow++, 4, 1, 1);

        addComponent(cbobx_mdfv_sim_time, iRow, 0, 1, 1);
        addComponent(label_mdfv_sim_time, iRow, 1, 2, 1);
        addComponent(cbobx_msiv_queue_dist, iRow, 3, 1, 1);
        addComponent(label_msiv_queue_dist, iRow++, 4, 1, 1);

        addComponent(cbobx_mdfv_dt, iRow, 0, 1, 1);
        addComponent(label_mdfv_dt, iRow, 1, 2, 1);
        addComponent(cbobx_mdfv_lambda, iRow, 3, 1, 1);
        addComponent(label_mdfv_lambda, iRow++, 4, 1, 1);

        addComponent(cbobx_mstv_inter_control, iRow, 0, 1, 1);
        addComponent(label_mstv_inter_control, iRow, 1, 2, 1);
        addComponent(cbobx_mdfv_mu, iRow, 3, 1, 1);
        addComponent(label_mdfv_mu, iRow++, 4, 1, 1);

        addComponent(cbobx_mstv_turn_sum, iRow, 0, 1, 1);
        addComponent(label_mstv_turn_sum, iRow, 1, 2, 1);
        addComponent(cbobx_msiv_alpha, iRow, 3, 1, 1);
        addComponent(label_msiv_alpha, iRow++, 4, 1, 1);

        addComponent(cbobx_mstv_appro_sum, iRow, 0, 1, 1);
        addComponent(label_mstv_appro_sum, iRow, 1, 2, 1);
        addComponent(cbobx_mdfv_t_lead, iRow, 3, 1, 1);
        addComponent(label_mdfv_t_lead, iRow++, 4, 1, 1);

        addComponent(cbobx_mstv_compressed, iRow, 0, 1, 1);
        addComponent(label_mstv_compressed, iRow, 1, 2, 1);
        addComponent(cbobx_mdfv_t_lag, iRow, 3, 1, 1);
        addComponent(label_mdfv_t_lag, iRow++, 4, 1, 1);

        addComponent(cbobx_mstv_pva_data, iRow, 0, 1, 1);
        addComponent(label_mstv_pva_data, iRow, 1, 2, 1);
        addComponent(cbobx_mdfv_hesitation_factor, iRow, 3, 1, 1);
        addComponent(label_mdfv_hesitation_factor, iRow++, 4, 1, 1);

        addComponent(cbobx_msiv_pva_end, iRow, 0, 1, 1);
        addComponent(label_msiv_pva_end, iRow, 1, 2, 1);
        addComponent(cbobx_mstv_majcol_stop, iRow, 3, 1, 1);
        addComponent(label_mstv_majcol_stop, iRow++, 4, 1, 1);

        addComponent(cbobx_mstv_wide, iRow, 0, 1, 1);
        addComponent(label_mstv_wide, iRow, 1, 2, 1);
        addComponent(cbobx_msiv_majcol_percent_des_speed, iRow, 3, 1, 1);
        addComponent(label_msiv_majcol_percent_des_speed, iRow++, 4, 1, 1);

        addComponent(cbobx_mstv_lt_pullout, iRow, 0, 1, 1);
        addComponent(label_mstv_lt_pullout, iRow, 1, 2, 1);
        addComponent(cbobx_msiv_majcol_evasive_act_mean, iRow, 3, 1, 1);
        addComponent(label_msiv_majcol_evasive_act_mean, iRow++, 4, 1, 1);

        addComponent(cbobx_mdfv_dilemma_zone_time_beg, iRow, 0, 1, 1);
        addComponent(label_mdfv_dilemma_zone_time_beg, iRow, 1, 2, 1);
        addComponent(cbobx_msiv_majcol_evasive_act_stddev, iRow, 3, 1, 1);
        addComponent(label_msiv_majcol_evasive_act_stddev, iRow++, 4, 1, 1);

        addComponent(cbobx_mdfv_dilemma_zone_time_end, iRow, 0, 1, 1);
        addComponent(label_mdfv_dilemma_zone_time_end, iRow, 1, 2, 1);
        addComponent(cbobx_mstv_enable_ssam_file, iRow, 3, 1, 1);
        addComponent(label_mstv_enable_ssam_file, iRow++, 4, 1, 1);

        addComponent(cbobx_msiv_hils_sleep_time, iRow, 0, 1, 1);
        addComponent(label_msiv_hils_sleep_time, iRow++, 1, 2, 1);

        addComponent(VMSmesgButton, iRow++, 0, 2, 1);

        addComponent(panel_accept, iRow++, 0, 5, 1);
        addComponent(ok_panel, iRow++, 0, 5, 1);

        aFrame.setSize(800, 680);
        aFrame.setVisible(true);
        aFrame.pack();
        aFrame.setLocation(SCREEN_SIZE.width / 2 - aFrame.getWidth() / 2, SCREEN_SIZE.height / 2 - aFrame.getHeight() / 2);
        aFrame.setFocusTraversalPolicy(new focusPolicy());
        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    } // end of method SimulationDataDialog

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstr.gridx = column;
        gbConstr.gridy = row;

        gbConstr.gridwidth = width;
        gbConstr.gridheight = height;

        gbLayout.setConstraints(c, gbConstr);
        container.add(c);
    } // end of method addComponent

    void setAccessiblity() {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];

        cbobx_mdfv_start_time.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME]);
        cbobx_mdfv_sim_time.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME]);
        cbobx_mdfv_dt.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT]);
        cbobx_mstv_inter_control.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_INTER_CONTROL]);
        cbobx_mstv_turn_sum.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_TURN_SUM]);
        cbobx_mstv_appro_sum.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_APPRO_SUM]);
        cbobx_mstv_compressed.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_COMPRESSED]);
        cbobx_mstv_pva_data.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_DATA]);
        cbobx_msiv_pva_end.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END]);
        cbobx_mstv_wide.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_WIDE]);
        cbobx_mstv_lt_pullout.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_LT_PULLOUT]);
        cbobx_mdfv_dilemma_zone_time_beg.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT]);
        cbobx_mdfv_dilemma_zone_time_end.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT]);
        cbobx_msiv_hils_sleep_time.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME]);

        cbobx_mdfv_start_time.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME]);
        cbobx_mdfv_sim_time.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME]);
        cbobx_mdfv_dt.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT]);
        cbobx_mstv_inter_control.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_INTER_CONTROL]);
        cbobx_mstv_turn_sum.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_TURN_SUM]);
        cbobx_mstv_appro_sum.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_APPRO_SUM]);
        cbobx_mstv_compressed.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_COMPRESSED]);
        cbobx_mstv_pva_data.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_DATA]);
        cbobx_msiv_pva_end.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END]);
        cbobx_mstv_wide.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_WIDE]);
        cbobx_mstv_lt_pullout.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_LT_PULLOUT]);
        cbobx_mdfv_dilemma_zone_time_beg.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT]);
        cbobx_mdfv_dilemma_zone_time_end.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT]);
        cbobx_msiv_hils_sleep_time.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME]);

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];

        cbobx_msiv_ds_speed.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED]);
        cbobx_msiv_queue_dist.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST]);
        cbobx_mdfv_lambda.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA]);
        cbobx_mdfv_mu.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU]);
        cbobx_msiv_alpha.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA]);
        cbobx_mdfv_t_lead.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD]);
        cbobx_mdfv_t_lag.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG]);
        cbobx_mdfv_hesitation_factor.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT]);
        cbobx_mstv_majcol_stop.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP]);
        cbobx_msiv_majcol_percent_des_speed.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD]);
        cbobx_msiv_majcol_evasive_act_mean.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN]);
        cbobx_msiv_majcol_evasive_act_stddev.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD]);
        cbobx_mstv_enable_ssam_file.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE]);

        cbobx_msiv_ds_speed.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED]);
        cbobx_msiv_queue_dist.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST]);
        cbobx_mdfv_lambda.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA]);
        cbobx_mdfv_mu.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU]);
        cbobx_msiv_alpha.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA]);
        cbobx_mdfv_t_lead.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD]);
        cbobx_mdfv_t_lag.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG]);
        cbobx_mdfv_hesitation_factor.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT]);
        cbobx_mstv_majcol_stop.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP]);
        cbobx_msiv_majcol_percent_des_speed.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD]);
        cbobx_msiv_majcol_evasive_act_mean.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN]);
        cbobx_msiv_majcol_evasive_act_stddev.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD]);
        cbobx_mstv_enable_ssam_file.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE]);

        text_sim_title.getAccessibleContext().setAccessibleName(label_sim_title.getText());
        acceptDefaultCheckbox.getAccessibleContext().setAccessibleName(acceptDefaultCheckbox.getText());
        VMSmesgButton.getAccessibleContext().setAccessibleName(VMSmesgButton.getText());
        okButton.getAccessibleContext().setAccessibleName("OK");
        applyButton.getAccessibleContext().setAccessibleName("Apply");
        cancelButton.getAccessibleContext().setAccessibleName("Cancel");

        text_sim_title.getAccessibleContext().setAccessibleDescription(label_sim_title.getText());
        acceptDefaultCheckbox.getAccessibleContext().setAccessibleDescription(acceptDefaultCheckbox.getText());
        VMSmesgButton.getAccessibleContext().setAccessibleDescription(VMSmesgButton.getText());
        okButton.getAccessibleContext().setAccessibleDescription("OK");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

    } // end of method setAccessiblity

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
    } // end of class OpenComboMenuListener

    public class focusPolicy extends FocusTraversalPolicy {

        public Component getComponentAfter(Container focusCycleRoot, Component aComponent) {
            if (aComponent.equals(text_sim_title)) {
                return cbobx_mdfv_start_time;
            }
            else if (aComponent.equals(cbobx_mdfv_start_time)) {
                return cbobx_mdfv_sim_time;
            }
            else if (aComponent.equals(cbobx_mdfv_sim_time)) {
                return cbobx_mdfv_dt;
            }
            else if (aComponent.equals(cbobx_mdfv_dt)) {
                return cbobx_mstv_inter_control;
            }
            else if (aComponent.equals(cbobx_mstv_inter_control)) {
                return cbobx_mstv_turn_sum;
            }
            else if (aComponent.equals(cbobx_mstv_turn_sum)) {
                return cbobx_mstv_appro_sum;
            }
            else if (aComponent.equals(cbobx_mstv_appro_sum)) {
                return cbobx_mstv_compressed;
            }
            else if (aComponent.equals(cbobx_mstv_compressed)) {
                return cbobx_mstv_pva_data;
            }
            else if (aComponent.equals(cbobx_mstv_pva_data)) {
                return cbobx_msiv_pva_end;
            }
            else if (aComponent.equals(cbobx_msiv_pva_end)) {
                return cbobx_mstv_wide;
            }
            else if (aComponent.equals(cbobx_mstv_wide)) {
                return cbobx_mstv_lt_pullout;
            }
            else if (aComponent.equals(cbobx_mstv_lt_pullout)) {
                if (cbobx_mdfv_dilemma_zone_time_beg.isVisible()) {
                    return cbobx_mdfv_dilemma_zone_time_beg;
                }
                else if (cbobx_mdfv_dilemma_zone_time_end.isVisible()) {
                    return cbobx_mdfv_dilemma_zone_time_end;
                }
                else if (cbobx_msiv_hils_sleep_time.isVisible()) {
                    return cbobx_msiv_hils_sleep_time;
                }
                else {
                    return VMSmesgButton;
                }
            }
            else if (aComponent.equals(cbobx_mdfv_dilemma_zone_time_beg)) {
                if (cbobx_mdfv_dilemma_zone_time_end.isVisible()) {
                    return cbobx_mdfv_dilemma_zone_time_end;
                }
                else if (cbobx_msiv_hils_sleep_time.isVisible()) {
                    return cbobx_msiv_hils_sleep_time;
                }
                else {
                    return VMSmesgButton;
                }
            }
            else if (aComponent.equals(cbobx_mdfv_dilemma_zone_time_end)) {
                if (cbobx_msiv_hils_sleep_time.isVisible()) {
                    return cbobx_msiv_hils_sleep_time;
                }
                else {
                    return VMSmesgButton;
                }
            }
            else if (aComponent.equals(cbobx_msiv_hils_sleep_time)) {
                return VMSmesgButton;
            }
            else if (aComponent.equals(VMSmesgButton)) {
                return cbobx_msiv_ds_speed;
            }
            else if (aComponent.equals(cbobx_msiv_ds_speed)) {
                return cbobx_msiv_queue_dist;
            }
            else if (aComponent.equals(cbobx_msiv_queue_dist)) {
                return cbobx_mdfv_lambda;
            }
            else if (aComponent.equals(cbobx_mdfv_lambda)) {
                return cbobx_mdfv_mu;
            }
            else if (aComponent.equals(cbobx_mdfv_mu)) {
                return cbobx_msiv_alpha;
            }
            else if (aComponent.equals(cbobx_msiv_alpha)) {
                return cbobx_mdfv_t_lead;
            }
            else if (aComponent.equals(cbobx_mdfv_t_lead)) {
                return cbobx_mdfv_t_lag;
            }
            else if (aComponent.equals(cbobx_mdfv_t_lag)) {
                return cbobx_mdfv_hesitation_factor;
            }
            else if (aComponent.equals(cbobx_mdfv_hesitation_factor)) {
                return cbobx_mstv_majcol_stop;
            }
            else if (aComponent.equals(cbobx_mstv_majcol_stop)) {
                return cbobx_msiv_majcol_percent_des_speed;
            }
            else if (aComponent.equals(cbobx_msiv_majcol_percent_des_speed)) {
                return cbobx_msiv_majcol_evasive_act_mean;
            }
            else if (aComponent.equals(cbobx_msiv_majcol_evasive_act_mean)) {
                return cbobx_msiv_majcol_evasive_act_stddev;
            }
            else if (aComponent.equals(cbobx_msiv_majcol_evasive_act_stddev)) {
                return cbobx_mstv_enable_ssam_file;
            }
            else if (aComponent.equals(cbobx_mstv_enable_ssam_file)) {
                if (acceptDefaultCheckbox.isEnabled()) {
                    return acceptDefaultCheckbox;
                }
                else {
                    return okButton;
                }
            }
            else if (aComponent.equals(acceptDefaultCheckbox)) {
                return okButton;
            }
            else if (aComponent.equals(okButton)) {
                return applyButton;
            }
            else if (aComponent.equals(applyButton)) {
                return cancelButton;
            }
            else if (aComponent.equals(cancelButton)) {
                return text_sim_title;
            }

            return okButton;
        }

        public Component getComponentBefore(Container focusCycleRoot, Component aComponent) {
            if (aComponent.equals(text_sim_title)) {
                return cancelButton;
            }
            else if (aComponent.equals(cbobx_mdfv_start_time)) {
                return text_sim_title;
            }
            else if (aComponent.equals(cbobx_mdfv_sim_time)) {
                return cbobx_mdfv_start_time;
            }
            else if (aComponent.equals(cbobx_mdfv_dt)) {
                return cbobx_mdfv_sim_time;
            }
            else if (aComponent.equals(cbobx_mstv_inter_control)) {
                return cbobx_mdfv_dt;
            }
            else if (aComponent.equals(cbobx_mstv_turn_sum)) {
                return cbobx_mstv_inter_control;
            }
            else if (aComponent.equals(cbobx_mstv_appro_sum)) {
                return cbobx_mstv_turn_sum;
            }
            else if (aComponent.equals(cbobx_mstv_compressed)) {
                return cbobx_mstv_appro_sum;
            }
            else if (aComponent.equals(cbobx_mstv_pva_data)) {
                return cbobx_mstv_compressed;
            }
            else if (aComponent.equals(cbobx_msiv_pva_end)) {
                return cbobx_mstv_pva_data;
            }
            else if (aComponent.equals(cbobx_mstv_wide)) {
                return cbobx_msiv_pva_end;
            }
            else if (aComponent.equals(cbobx_mstv_lt_pullout)) {
                return cbobx_mstv_wide;
            }
            else if (aComponent.equals(cbobx_mdfv_dilemma_zone_time_beg)) {
                return cbobx_mstv_lt_pullout;
            }
            else if (aComponent.equals(cbobx_mdfv_dilemma_zone_time_end)) {
                if (cbobx_mdfv_dilemma_zone_time_beg.isVisible()) {
                    return cbobx_mdfv_dilemma_zone_time_beg;
                }
                else {
                    return cbobx_mstv_lt_pullout;
                }
            }
            else if (aComponent.equals(cbobx_msiv_hils_sleep_time)) {
                if (cbobx_mdfv_dilemma_zone_time_end.isVisible()) {
                    return cbobx_mdfv_dilemma_zone_time_end;
                }
                else if (cbobx_mdfv_dilemma_zone_time_beg.isVisible()) {
                    return cbobx_mdfv_dilemma_zone_time_beg;
                }
                else {
                    return cbobx_mstv_lt_pullout;
                }
            }
            else if (aComponent.equals(VMSmesgButton)) {
                if (cbobx_msiv_hils_sleep_time.isVisible()) {
                    return cbobx_msiv_hils_sleep_time;
                }
                else if (cbobx_mdfv_dilemma_zone_time_end.isVisible()) {
                    return cbobx_mdfv_dilemma_zone_time_end;
                }
                else if (cbobx_mdfv_dilemma_zone_time_beg.isVisible()) {
                    return cbobx_mdfv_dilemma_zone_time_beg;
                }
                else {
                    return cbobx_mstv_lt_pullout;
                }
            }
            else if (aComponent.equals(cbobx_msiv_ds_speed)) {
                return VMSmesgButton;
            }
            else if (aComponent.equals(cbobx_msiv_queue_dist)) {
                return cbobx_msiv_ds_speed;
            }
            else if (aComponent.equals(cbobx_mdfv_lambda)) {
                return cbobx_msiv_queue_dist;
            }
            else if (aComponent.equals(cbobx_mdfv_mu)) {
                return cbobx_mdfv_lambda;
            }
            else if (aComponent.equals(cbobx_msiv_alpha)) {
                return cbobx_mdfv_mu;
            }
            else if (aComponent.equals(cbobx_mdfv_t_lead)) {
                return cbobx_msiv_alpha;
            }
            else if (aComponent.equals(cbobx_mdfv_t_lag)) {
                return cbobx_mdfv_t_lead;
            }
            else if (aComponent.equals(cbobx_mdfv_hesitation_factor)) {
                return cbobx_mdfv_t_lag;
            }
            else if (aComponent.equals(cbobx_mstv_majcol_stop)) {
                return cbobx_mdfv_hesitation_factor;
            }
            else if (aComponent.equals(cbobx_msiv_majcol_percent_des_speed)) {
                return cbobx_mstv_majcol_stop;
            }
            else if (aComponent.equals(cbobx_msiv_majcol_evasive_act_mean)) {
                return cbobx_msiv_majcol_percent_des_speed;
            }
            else if (aComponent.equals(cbobx_msiv_majcol_evasive_act_stddev)) {
                return cbobx_msiv_majcol_evasive_act_mean;
            }
            else if (aComponent.equals(cbobx_mstv_enable_ssam_file)) {
                return cbobx_msiv_majcol_evasive_act_stddev;
            }
            else if (aComponent.equals(acceptDefaultCheckbox)) {
                return cbobx_mstv_enable_ssam_file;
            }
            else if (aComponent.equals(okButton)) {
                if (acceptDefaultCheckbox.isEnabled()) {
                    return acceptDefaultCheckbox;
                }
                else {
                    return cbobx_mstv_enable_ssam_file;
                }
            }
            else if (aComponent.equals(applyButton)) {
                return okButton;
            }
            else if (aComponent.equals(cancelButton)) {
                return applyButton;
            }

            return okButton;
        }

        public Component getDefaultComponent(Container focusCycleRoot) {
            return text_sim_title;
        }

        public Component getLastComponent(Container focusCycleRoot) {
            return cancelButton;
        }

        public Component getFirstComponent(Container focusCycleRoot) {
            return text_sim_title;
        }
    } // end of class focusPolicy

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                if (event.getSource() == text_sim_title) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_TITLE];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_TITLE_TEXT], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_TITLE_TEXT],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_TITLE_TEXT], text_sim_title.getText().trim(), lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_TITLE_TEXT],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_TITLE_TEXT], " ", " ", " ");
                }
                else if (event.getSource() == cbobx_mdfv_start_time) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME], cbobx_mdfv_start_time
                                    .getSelectedItem().toString(),
                            Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME]), " ",
                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME]),
                            Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME]),
                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME]));
                }
                else if (event.getSource() == cbobx_mdfv_sim_time) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME], cbobx_mdfv_sim_time.getSelectedItem().toString(),
                            Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME]), " ",
                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME]), Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME]),
                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME]));
                }
                else if (event.getSource() == cbobx_mdfv_dt) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT], cbobx_mdfv_dt.getSelectedItem().toString(),
                            Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT]), " ", Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT]),
                            Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT]), Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT]));
                }
                else if (event.getSource() == cbobx_mstv_inter_control) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_INTER_CONTROL], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_INTER_CONTROL], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_INTER_CONTROL],
                            cbobx_mstv_inter_control.getSelectedItem().toString(), lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_INTER_CONTROL],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_INTER_CONTROL], " ", " ", " ");
                }
                else if (event.getSource() == cbobx_mstv_turn_sum) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_TURN_SUM], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_TURN_SUM],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_TURN_SUM], cbobx_mstv_turn_sum.getSelectedItem().toString(),
                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_TURN_SUM], lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_TURN_SUM], " ", " ", " ");
                }
                else if (event.getSource() == cbobx_mstv_appro_sum) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_APPRO_SUM], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_APPRO_SUM],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_APPRO_SUM], cbobx_mstv_appro_sum.getSelectedItem().toString(),
                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_APPRO_SUM], lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_APPRO_SUM], " ", " ", " ");
                }
                else if (event.getSource() == cbobx_mstv_compressed) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_COMPRESSED], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_COMPRESSED], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_COMPRESSED], cbobx_mstv_compressed
                                    .getSelectedItem().toString(),
                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_COMPRESSED],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_COMPRESSED], " ", " ", " ");
                }
                else if (event.getSource() == cbobx_mstv_pva_data) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_DATA], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_DATA],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_DATA], cbobx_mstv_pva_data.getSelectedItem().toString(),
                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_DATA], lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_DATA], " ", " ", " ");
                }
                else if (event.getSource() == cbobx_msiv_pva_end) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END], cbobx_msiv_pva_end.getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END]), Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END]));
                }
                else if (event.getSource() == cbobx_mstv_wide) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_WIDE], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_WIDE],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_WIDE], cbobx_mstv_wide.getSelectedItem().toString(),
                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_WIDE], lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_WIDE], " ", " ", " ");
                }
                else if (event.getSource() == cbobx_mstv_lt_pullout) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_LT_PULLOUT], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_LT_PULLOUT], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_LT_PULLOUT], cbobx_mstv_lt_pullout
                                    .getSelectedItem().toString(),
                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_LT_PULLOUT],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_LT_PULLOUT], " ", " ", " ");
                }
                else if (event.getSource() == cbobx_mdfv_dilemma_zone_time_beg) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT],
                            cbobx_mdfv_dilemma_zone_time_beg.getSelectedItem().toString(), Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT]), " ",
                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT]),
                            Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT]),
                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT]));
                }
                else if (event.getSource() == cbobx_mdfv_dilemma_zone_time_end) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT],
                            cbobx_mdfv_dilemma_zone_time_end.getSelectedItem().toString(), Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT]), " ",
                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT]),
                            Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT]),
                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT]));
                }
                else if (event.getSource() == cbobx_msiv_hils_sleep_time) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME],
                            cbobx_msiv_hils_sleep_time.getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME]));
                }
                else if (event.getSource() == cbobx_msiv_ds_speed) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED], cbobx_msiv_ds_speed
                                    .getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED]));
                }
                else if (event.getSource() == cbobx_msiv_queue_dist) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST], cbobx_msiv_queue_dist
                                    .getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST]));
                }
                else if (event.getSource() == cbobx_mdfv_lambda) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA], cbobx_mdfv_lambda.getSelectedItem().toString(),
                            Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA]), " ",
                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA]), Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA]),
                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA]));
                }
                else if (event.getSource() == cbobx_mdfv_mu) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU], cbobx_mdfv_mu.getSelectedItem().toString(),
                            Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU]), " ", Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU]),
                            Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU]), Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU]));
                }
                else if (event.getSource() == cbobx_msiv_alpha) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA], cbobx_msiv_alpha.getSelectedItem().toString(),
                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA]), Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA]));
                }
                else if (event.getSource() == cbobx_mdfv_t_lead) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD], cbobx_mdfv_t_lead.getSelectedItem().toString(),
                            Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD]), " ",
                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD]), Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD]),
                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD]));
                }
                else if (event.getSource() == cbobx_mdfv_t_lag) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG], lclv_tx_fmt.mstv_name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG],
                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG], cbobx_mdfv_t_lag.getSelectedItem().toString(),
                            Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG]), " ",
                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG]), Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG]),
                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG]));
                }
                else if (event.getSource() == cbobx_mdfv_hesitation_factor) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT],
                            cbobx_mdfv_hesitation_factor.getSelectedItem().toString(), Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT]), " ",
                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT]),
                            Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT]),
                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT]));
                }
                else if (event.getSource() == cbobx_mstv_majcol_stop) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP],
                            cbobx_mstv_majcol_stop.getSelectedItem().toString(), lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP], " ", " ", " ");
                }
                else if (event.getSource() == cbobx_msiv_majcol_percent_des_speed) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD],
                            cbobx_msiv_majcol_percent_des_speed.getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD]));
                }
                else if (event.getSource() == cbobx_msiv_majcol_evasive_act_mean) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN],
                            cbobx_msiv_majcol_evasive_act_mean.getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN]));
                }
                else if (event.getSource() == cbobx_msiv_majcol_evasive_act_stddev) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD],
                            cbobx_msiv_majcol_evasive_act_stddev.getSelectedItem().toString(), Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD]), " ",
                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD]),
                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD]),
                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD]));
                }
                else if (event.getSource() == cbobx_mstv_enable_ssam_file) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE],
                            cbobx_mstv_enable_ssam_file.getSelectedItem().toString(), lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE], " ", " ", " ");
                }
                else if (event.getSource() == okButton) {
                    new HelpDialog(true, "OK button", "The OK button saves the data and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == applyButton) {
                    new HelpDialog(true, "Apply button", "The Apply button saves the data but does not close the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == cancelButton) {
                    new HelpDialog(true, "Cancel button", "The Cancel button discards any changes and closes the window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == VMSmesgButton) {
                    new HelpDialog(true, VMSmesgButton.getText(), "The " + VMSmesgButton.getText() + " button open VMS Message window.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == acceptDefaultCheckbox) {
                    if (acceptDefaultCheckbox.isSelected()) {
                        new HelpDialog(true, "Accept Default Checkbox", "The acceptDefaultCheckbox button accepts the default lane control value.",
                                "The Accept All Simulation default data values check box accepts all the Lane Control values when the Type of Intersection Control is UNCONTRL or ALL-STOP.", "True",
                                " ", " ", " ", " ", " ");
                    }
                    else {
                        new HelpDialog(true, "Accept Default Checkbox", "The acceptDefaultCheckbox button accepts the default lane control value.",
                                "The Accept All Simulation default data values check box accepts all the Lane Control values when the Type of Intersection Control is UNCONTRL or ALL-STOP.", "False",
                                " ", " ", " ", " ", " ");
                    }
                } // if(event.getKeyCode() == KeyEvent.VK_F1)
            }
        }
    } // end of class HelpListener

    void acceptLaneControlDefaultValue() {
        int number_of_legs;
        int[] number_of_lanes = new int[8];

        for (int lsiv_lane = 0; lsiv_lane < 8; lsiv_lane++) {
            number_of_lanes[lsiv_lane] = 2;
        }

        number_of_legs = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
            if ((0 < lsiv_leg) && (lsiv_leg <= number_of_legs)) {
                number_of_lanes[lsiv_leg] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            }
            else {
                if (gdvsim.gclv_inter.mbov_is_diamond_interchange)
                    number_of_lanes[lsiv_leg] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
            }
        }
        if (cbobx_mstv_inter_control.getSelectedItem().toString().equals("UNCONTRL")) {
            for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
                if ((0 < lsiv_leg) && (lsiv_leg <= number_of_legs)) {
                    for (int lsiv_lane = 0; lsiv_lane < number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane + 1].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
                    }
                }
                else {
                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        for (int lsiv_lane = 0; lsiv_lane < number_of_lanes[lsiv_leg]; lsiv_lane++) {
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane + 1].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
                        }
                    }
                }
            }

            for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
                if ((0 < lsiv_leg) && (lsiv_leg <= number_of_legs)) {
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_1] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_2] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_3] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_4] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_5] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[6].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_6] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_1] = gdvsim.gclv_inter.TX_FROM_USER;
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_2] = gdvsim.gclv_inter.TX_FROM_USER;
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_3] = gdvsim.gclv_inter.TX_FROM_USER;
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_4] = gdvsim.gclv_inter.TX_FROM_USER;
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_5] = gdvsim.gclv_inter.TX_FROM_USER;
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[6].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_6] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                }
            }
        }

        if (cbobx_mstv_inter_control.getSelectedItem().toString().equals("ALL-STOP")) {
            for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
                if ((0 < lsiv_leg) && (lsiv_leg <= number_of_legs)) {
                    for (int lsiv_lane = 0; lsiv_lane < number_of_lanes[lsiv_leg]; lsiv_lane++) {
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane + 1].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "ST";
                    }
                }
                else {
                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        for (int lsiv_lane = 0; lsiv_lane < number_of_lanes[lsiv_leg]; lsiv_lane++) {
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane + 1].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "ST";
                        }
                    }
                }
            }

            for (int lsiv_leg = 0; lsiv_leg <= (number_of_legs + 1); lsiv_leg++) {
                if ((0 < lsiv_leg) && (lsiv_leg <= number_of_legs)) {
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_1] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_2] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_3] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_4] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_5] = gdvsim.gclv_inter.TX_FROM_USER;
                    gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[6].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_6] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_1] = gdvsim.gclv_inter.TX_FROM_USER;
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_2] = gdvsim.gclv_inter.TX_FROM_USER;
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_3] = gdvsim.gclv_inter.TX_FROM_USER;
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_4] = gdvsim.gclv_inter.TX_FROM_USER;
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_5] = gdvsim.gclv_inter.TX_FROM_USER;
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[6].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_6] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                }
            }
        }

        int lsiv_inb_leg = 3;
        int lsiv_out_leg = 4;
        if (gdvsim.gclv_inter.mbov_is_diamond_interchange && gdvsim.gclv_inter.mbov_free_uturns_defined) {
            // check if free u-turn 1 lane width is valid
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.gclv_inter.mstv_errorMessage = "Error in SDR_Hdr.readFromCards: gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width is invalid.";
                gdvsim.gclv_inter.errorMessage();
                gdvsim.gclv_inter.msiv_returnCode = gdvsim.gclv_inter.RETURN_FATAL_ERROR;
                return;
            }

            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0) {
                // free u-turn 1 connects leg 3 inbound and leg 4 outbound
                lsiv_inb_leg = 3;
                lsiv_out_leg = 4;

                // store movement code "U" for free u-turn in leg 3/4 or 6/1 lane 0
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

                // store lane control "UN" for free u-turn in leg 3/4 or 6/1 lane 0
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            }

            // check if free u-turn 2 lane width is valid
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.gclv_inter.mstv_errorMessage = "Error in SDR_Hdr.readFromCards: gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width is invalid.";
                gdvsim.gclv_inter.errorMessage();
                gdvsim.gclv_inter.msiv_returnCode = gdvsim.gclv_inter.RETURN_FATAL_ERROR;
                return;
            }

            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0) {
                // free u-turn 2 connects leg 6 inbound and leg 1 outbound
                lsiv_inb_leg = 6;
                lsiv_out_leg = 1;

                // store movement code "U" for free u-turn in leg 3/4 or 6/1 lane 0
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            }

            // store lane control "UN" for free u-turn in leg 3/4 or 6/1 lane 0
            gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
            gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
            gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        }
    } // end of method acceptLaneControlDefaultValue

    void setDisplay() {
        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            gdvsim.intersectionTypeButton.setText("Diamond Interchange");
            gdvsim.gdvDataButton.setText("Diamond GDV Data");
        }
        else {
            gdvsim.intersectionTypeButton.setText("Standard Intersection");
            gdvsim.gdvDataButton.setText("GDV Data");
        }

        gdvsim.intersectionTypeButton.setVisible(true);
        gdvsim.intersectionTypeButton.setEnabled(false);
        gdvsim.gdvDataButton.setVisible(true);

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs == 3) {
            gdvsim.leg1Button.setVisible(true);
            gdvsim.leg2Button.setVisible(true);
            gdvsim.leg3Button.setVisible(true);
            gdvsim.leg4Button.setVisible(false);
            gdvsim.leg5Button.setVisible(false);
            gdvsim.leg6Button.setVisible(false);
        }

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs == 4) {
            gdvsim.leg1Button.setVisible(true);
            gdvsim.leg2Button.setVisible(true);
            gdvsim.leg3Button.setVisible(true);
            gdvsim.leg4Button.setVisible(true);
            gdvsim.leg5Button.setVisible(false);
            gdvsim.leg6Button.setVisible(false);
        }

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs == 5) {
            gdvsim.leg1Button.setVisible(true);
            gdvsim.leg2Button.setVisible(true);
            gdvsim.leg3Button.setVisible(true);
            gdvsim.leg4Button.setVisible(true);
            gdvsim.leg5Button.setVisible(true);
            gdvsim.leg6Button.setVisible(false);
        }

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs == 6) {
            gdvsim.leg1Button.setVisible(true);
            gdvsim.leg2Button.setVisible(true);
            gdvsim.leg3Button.setVisible(true);
            gdvsim.leg4Button.setVisible(true);
            gdvsim.leg5Button.setVisible(true);
            gdvsim.leg6Button.setVisible(true);
        }

        gdvsim.simulationButton.setVisible(true);
        gdvsim.laneControlButton.setVisible(true);

        if (gdvsim.gclv_inter.mbov_is_ic_PRETIMED) {
            gdvsim.intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            gdvsim.intersectionControlButton.setVisible(true);
            gdvsim.intersectionControlButton.setMnemonic(KeyEvent.VK_P);
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT) {
            gdvsim.intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            gdvsim.intersectionControlButton.setVisible(true);
            gdvsim.intersectionControlButton.setMnemonic(KeyEvent.VK_E);
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_FULL_ACT) {
            gdvsim.intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            gdvsim.intersectionControlButton.setVisible(true);
            gdvsim.intersectionControlButton.setMnemonic(KeyEvent.VK_F);
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_TEX_DIA) {
            gdvsim.intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            gdvsim.intersectionControlButton.setVisible(true);
            gdvsim.intersectionControlButton.setMnemonic(KeyEvent.VK_T);
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            gdvsim.intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            gdvsim.intersectionControlButton.setVisible(true);
            gdvsim.intersectionControlButton.setMnemonic(KeyEvent.VK_N);
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            gdvsim.intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            gdvsim.intersectionControlButton.setVisible(true);
            gdvsim.intersectionControlButton.setMnemonic(KeyEvent.VK_H);
        }
        else {
            gdvsim.intersectionControlButton.setVisible(false);
        }

        gdvsim.greenIntervalSequenceButton.setVisible(gdvsim.gclv_inter.mbov_is_ic_signal_controlled);

        gdvsim.detectorButton.setVisible(gdvsim.gclv_inter.mbov_is_ic_detector_data);
    } // end of method setDisplay

    void setDefaultNumberOfPhases() {
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] = gdvsim.gclv_inter.TX_FROM_USER;

        if (cbobx_mstv_inter_control.getSelectedItem().toString().equals("TEX-DIA") || cbobx_mstv_inter_control.getSelectedItem().toString().equals("NEMA")) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases = 8;
        }
        else {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases = 2;
        }
    } // end of method setDefaultNumberOfPhases

    void saveData() {
        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_title.mclv_text.mstv_card = text_sim_title.getText().trim();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_title.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_TITLE_TEXT] = gdvsim.gclv_inter.TX_FROM_USER;

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_start_time = Double.valueOf(cbobx_mdfv_start_time.getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_sim_time = Double.valueOf(cbobx_mdfv_sim_time.getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dt = Double.valueOf(cbobx_mdfv_dt.getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = cbobx_mstv_inter_control.getSelectedItem().toString();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_turn_sum = cbobx_mstv_turn_sum.getSelectedItem().toString();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_appro_sum = cbobx_mstv_appro_sum.getSelectedItem().toString();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_compressed = cbobx_mstv_compressed.getSelectedItem().toString();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_pva_data = cbobx_mstv_pva_data.getSelectedItem().toString();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_pva_end = Integer.valueOf(cbobx_msiv_pva_end.getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_wide = cbobx_mstv_wide.getSelectedItem().toString();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_lt_pullout = cbobx_mstv_lt_pullout.getSelectedItem().toString();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dilemma_zone_time_beg = Double.valueOf(cbobx_mdfv_dilemma_zone_time_beg.getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dilemma_zone_time_end = Double.valueOf(cbobx_mdfv_dilemma_zone_time_end.getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_hitl_sleep_time = Integer.valueOf(cbobx_msiv_hils_sleep_time.getSelectedItem().toString()).intValue();

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_START_TIME] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_SIM_TIME] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DT] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_INTER_CONTROL] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_TURN_SUM] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_APPRO_SUM] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_COMPRESSED] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_DATA] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_PVA_END] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_WIDE] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_LT_PULLOUT] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] = gdvsim.gclv_inter.TX_FROM_USER;

        // check and set intersection control gdvsim.gclv_inter.mbov_is_ic_* values using
        // mstv_inter_control
        gdvsim.gclv_inter.check_and_set_intersection_control();
        if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS)
            return;

        // set defaults -------------------- usage ---------------------
        // mclv_sim_header .msiv_preclen_diafig_nemahitlov PRETIMED TEX-DIA NEMA HARDWARE
        // mclv_sim_header .msiv_no_of_phases PRETIMED SEMI-ACT FULL-ACT TEX-DIA NEMA HARDWARE
        // mclv_sim_header .msiv_no_of_det SEMI-ACT FULL-ACT TEX-DIA NEMA HARDWARE
        // mclv_sim_par_opt.mstv_nem2_simul_gapout NEMA
        // mclv_sim_par_opt.mdfv_dilemma_zone_time_beg PRETIMED SEMI-ACT FULL-ACT TEX-DIA NEMA
        // HARDWARE
        // mclv_sim_par_opt.mdfv_dilemma_zone_time_end PRETIMED SEMI-ACT FULL-ACT TEX-DIA NEMA
        // HARDWARE
        // mclv_sim_par_opt.msiv_hitl_sleep_time HARDWARE
        if (gdvsim.gclv_inter.mbov_is_ic_UNCONTRL || gdvsim.gclv_inter.mbov_is_ic_YIELD || gdvsim.gclv_inter.mbov_is_ic_STOP || gdvsim.gclv_inter.mbov_is_ic_ALL_STOP) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov = 0;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases = 0;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det = 0;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_nem2_simul_gapout = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dilemma_zone_time_beg = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dilemma_zone_time_end = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_hitl_sleep_time = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_DET] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] = gdvsim.gclv_inter.TX_DEFAULT;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT] = gdvsim.gclv_inter.TX_DEFAULT;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT] = gdvsim.gclv_inter.TX_DEFAULT;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] = gdvsim.gclv_inter.TX_DEFAULT;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_PRETIMED) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det = 0;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_nem2_simul_gapout = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_hitl_sleep_time = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_DET] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] = gdvsim.gclv_inter.TX_DEFAULT;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] = gdvsim.gclv_inter.TX_DEFAULT;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT || gdvsim.gclv_inter.mbov_is_ic_FULL_ACT) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov = 0;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_nem2_simul_gapout = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_hitl_sleep_time = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] = gdvsim.gclv_inter.TX_DEFAULT;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] = gdvsim.gclv_inter.TX_DEFAULT;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_TEX_DIA) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_nem2_simul_gapout = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_hitl_sleep_time = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] = gdvsim.gclv_inter.TX_DEFAULT;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] = gdvsim.gclv_inter.TX_DEFAULT;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_hitl_sleep_time = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] = gdvsim.gclv_inter.TX_DEFAULT;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_nem2_simul_gapout = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] = gdvsim.gclv_inter.TX_DEFAULT;
        }

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NUM_VMS_MESSAGES] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages = 0;
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NUM_VMS_MESSAGES] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        }

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_ds_speed = Integer.valueOf(cbobx_msiv_ds_speed.getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_queue_dist = Integer.valueOf(cbobx_msiv_queue_dist.getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_lambda = Double.valueOf(cbobx_mdfv_lambda.getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_mu = Double.valueOf(cbobx_mdfv_mu.getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_alpha = Integer.valueOf(cbobx_msiv_alpha.getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_t_lead = Double.valueOf(cbobx_mdfv_t_lead.getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_t_lag = Double.valueOf(cbobx_mdfv_t_lag.getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mdfv_hesitation_factor = Double.valueOf(cbobx_mdfv_hesitation_factor.getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mstv_majcol_stop = cbobx_mstv_majcol_stop.getSelectedItem().toString();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_majcol_percent_des_speed = Integer.valueOf(cbobx_msiv_majcol_percent_des_speed.getSelectedItem().toString())
                .intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_majcol_evasive_act_mean = Integer.valueOf(cbobx_msiv_majcol_evasive_act_mean.getSelectedItem().toString())
                .intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.msiv_majcol_evasive_act_stddev = Integer.valueOf(cbobx_msiv_majcol_evasive_act_stddev.getSelectedItem().toString())
                .intValue();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mstv_enable_ssam_file = cbobx_mstv_enable_ssam_file.getSelectedItem().toString();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_DS_SPEED] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_LAMBDA] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MU] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_ALPHA] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LEAD] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_T_LAG] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD] = gdvsim.gclv_inter.TX_FROM_USER;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt_2.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_2_SSAM_FILE] = gdvsim.gclv_inter.TX_FROM_USER;

        if (acceptDefaultCheckbox.isSelected()) {
            gdvsim.flag_laneControl_ok = true;
            acceptLaneControlDefaultValue();
        }

        int lsiv_inb_leg = 3;
        int lsiv_out_leg = 4;
        if (gdvsim.gclv_inter.mbov_is_diamond_interchange && gdvsim.gclv_inter.mbov_free_uturns_defined) {
            // check if free u-turn 1 lane width is valid
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.gclv_inter.mstv_errorMessage = "Error in SimulationDataDialog: gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width is invalid.";
                gdvsim.gclv_inter.errorMessage();
                gdvsim.gclv_inter.msiv_returnCode = gdvsim.gclv_inter.RETURN_FATAL_ERROR;
                return;
            }

            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0) {
                // free u-turn 1 connects leg 3 inbound and leg 4 outbound
                lsiv_inb_leg = 3;
                lsiv_out_leg = 4;

                // store movement code "U" for free u-turn in leg 3/4 or 6/1 lane 0
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;

                // store lane control "UN" for free u-turn in leg 3/4 or 6/1 lane 0
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            }

            // check if free u-turn 2 lane width is valid
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.gclv_inter.mstv_errorMessage = "Error in SimulationDataDialog: gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width is invalid.";
                gdvsim.gclv_inter.errorMessage();
                gdvsim.gclv_inter.msiv_returnCode = gdvsim.gclv_inter.RETURN_FATAL_ERROR;
                return;
            }

            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0) {
                // free u-turn 2 connects leg 6 inbound and leg 1 outbound
                lsiv_inb_leg = 6;
                lsiv_out_leg = 1;

                // store movement code "U" for free u-turn in leg 3/4 or 6/1 lane 0
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
                gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
                gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_MVMT] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            }

            // store lane control "UN" for free u-turn in leg 3/4 or 6/1 lane 0
            gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
            gdvsim.gclv_inter.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
            gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont = "UN";
            gdvsim.gclv_inter.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0] = gdvsim.gclv_inter.TX_SET_BY_SOFTWARE;
        }

        gdvsim.gclv_inter.calculate_graphics_and_paint();

    } // end of method saveData

    boolean isError() {
        double simDT, simStartTime, simSimulationTime, dz_time_beg, dz_time_end;
        int gdvTotalTime, simPVAEndTime;
        String s;
        TX_Fmt lclv_tx_fmt;
        int lsiv_textLength;
        int lsiv_textMax;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_TITLE];
        lsiv_textLength = text_sim_title.getText().trim().length();
        lsiv_textMax = lclv_tx_fmt.msia_fs[gdvsim.gclv_inter.TX_FMT_SIM_TITLE_TEXT];
        simDT = Double.valueOf(cbobx_mdfv_dt.getSelectedItem().toString()).doubleValue();
        simStartTime = Double.valueOf(cbobx_mdfv_start_time.getSelectedItem().toString()).doubleValue();
        simSimulationTime = Double.valueOf(cbobx_mdfv_sim_time.getSelectedItem().toString()).doubleValue();
        gdvTotalTime = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;
        simPVAEndTime = Integer.valueOf(cbobx_msiv_pva_end.getSelectedItem().toString()).intValue();
        dz_time_beg = Double.valueOf(cbobx_mdfv_dilemma_zone_time_beg.getSelectedItem().toString()).doubleValue();
        dz_time_end = Double.valueOf(cbobx_mdfv_dilemma_zone_time_end.getSelectedItem().toString()).doubleValue();

        if (lsiv_textLength == 0) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_title.mclv_text.mstv_card = null;
            JOptionPane.showMessageDialog(null, "Please enter a title.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }
        if (lsiv_textLength > lsiv_textMax) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_title.mclv_text.mstv_card = null;
            JOptionPane.showMessageDialog(null, "Title is " + (lsiv_textLength - lsiv_textMax) + " character" + ((lsiv_textLength - lsiv_textMax) > 1 ? "s" : "") + " longer than maximum of "
                    + lsiv_textMax + " characters.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (cbobx_mstv_inter_control.getSelectedItem().toString().equals("HARDWARE")) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
            if (simDT < (PARAMS.TEXAS_MODEL_HDTMIN - 0.0001)) {
                JOptionPane.showMessageDialog(null, lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_PAR_OPT_DT] + " = " + simDT + " is less than the minimum for HARDWARE Intersection Control = "
                        + PARAMS.TEXAS_MODEL_HDTMIN + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
            if (simDT > (PARAMS.TEXAS_MODEL_HDTMAX + 0.0001)) {
                JOptionPane.showMessageDialog(null, lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_PAR_OPT_DT] + " = " + simDT + " is greater than the maximum for HARDWARE Intersection Control = "
                        + PARAMS.TEXAS_MODEL_HDTMAX + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }

        if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_TIME] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                && ((simStartTime + simSimulationTime) > gdvTotalTime)) {
            JOptionPane.showMessageDialog(null, "SIM Start Time plus Simulation Time = " + (simStartTime + simSimulationTime) + " should be less than or equal to the GDV Total Time = " + gdvTotalTime
                    + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if ((simStartTime + simSimulationTime) < simPVAEndTime) {
            JOptionPane.showMessageDialog(null, "SIM Start Time plus Simulation Time = " + (simStartTime + simSimulationTime) + " should be greater than the Animation/Pollution end time = "
                    + simPVAEndTime + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
            return true;
        }

        if (dz_time_beg <= dz_time_end) {
            JOptionPane.showMessageDialog(null, "Dilemma Zone Begin Time = " + dz_time_beg + " is less than or equal to Dilemma Zone End Time = " + dz_time_end + ".", "Error Message",
                    JOptionPane.ERROR_MESSAGE);
            return true;
        }

        return false;
    } // end of method isError()

    void InterControlAction() {
        if (cbobx_mstv_inter_control.getSelectedItem().toString().equals("UNCONTRL") || cbobx_mstv_inter_control.getSelectedItem().toString().equals("ALL-STOP")) {
            acceptDefaultCheckbox.setEnabled(true);
            acceptDefaultCheckbox.setVisible(true);
        }
        else {
            acceptDefaultCheckbox.setEnabled(false);
            acceptDefaultCheckbox.setVisible(false);
        }

        if (cbobx_mstv_inter_control.getSelectedItem().toString().equals("UNCONTRL") || cbobx_mstv_inter_control.getSelectedItem().toString().equals("YIELD")
                || cbobx_mstv_inter_control.getSelectedItem().toString().equals("STOP") || cbobx_mstv_inter_control.getSelectedItem().toString().equals("ALL-STOP")) {
            cbobx_mdfv_dilemma_zone_time_beg.setVisible(false);
            cbobx_mdfv_dilemma_zone_time_end.setVisible(false);
            cbobx_msiv_hils_sleep_time.setVisible(false);

            label_mdfv_dilemma_zone_time_beg.setVisible(false);
            label_mdfv_dilemma_zone_time_end.setVisible(false);
            label_msiv_hils_sleep_time.setVisible(false);
        }
        else if (cbobx_mstv_inter_control.getSelectedItem().toString().equals("PRETIMED") || cbobx_mstv_inter_control.getSelectedItem().toString().equals("SEMI-ACT")
                || cbobx_mstv_inter_control.getSelectedItem().toString().equals("FULL-ACT") || cbobx_mstv_inter_control.getSelectedItem().toString().equals("TEX-DIA")
                || cbobx_mstv_inter_control.getSelectedItem().toString().equals("NEMA")) {
            cbobx_mdfv_dilemma_zone_time_beg.setVisible(true);
            cbobx_mdfv_dilemma_zone_time_end.setVisible(true);
            cbobx_msiv_hils_sleep_time.setVisible(false);

            label_mdfv_dilemma_zone_time_beg.setVisible(true);
            label_mdfv_dilemma_zone_time_end.setVisible(true);
            label_msiv_hils_sleep_time.setVisible(false);

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
            cbobx_mdfv_dilemma_zone_time_beg.setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT]));
            cbobx_mdfv_dilemma_zone_time_end.setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT]));
        }
        else if (cbobx_mstv_inter_control.getSelectedItem().toString().equals("HARDWARE")) {
            cbobx_mdfv_dilemma_zone_time_beg.setVisible(true);
            cbobx_mdfv_dilemma_zone_time_end.setVisible(true);
            cbobx_msiv_hils_sleep_time.setVisible(true);

            label_mdfv_dilemma_zone_time_beg.setVisible(true);
            label_mdfv_dilemma_zone_time_end.setVisible(true);
            label_msiv_hils_sleep_time.setVisible(true);

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
            cbobx_mdfv_dilemma_zone_time_beg.setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT]));
            cbobx_mdfv_dilemma_zone_time_end.setSelectedItem(oneDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT]));
            cbobx_msiv_hils_sleep_time.setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME]));
        }
    } // end of method InterControlAction

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    if (gdvsim.flag_simulationData_ok) {
                        if (!gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control.equals(cbobx_mstv_inter_control.getSelectedItem().toString())) {
                            gdvsim.resetSimulationGlobalValue();
                            setDefaultNumberOfPhases();
                        }
                    }
                    else {
                        setDefaultNumberOfPhases();
                    }

                    gdvsim.flag_simulationData_ok = true;
                    saveData();
                    setDisplay();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }
            else if (event.getSource() == VMSmesgButton) {
                if (gdvsim.flag_simulationData_ok) {
                    new VMS_MessageDialog();
                }
                else {
                    JOptionPane.showMessageDialog(null, "Please OK or Apply the SIM Parameter-Option Data before accessing the VMS Message Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                }
            }
            else if (event.getSource() == acceptDefaultCheckbox) {
                if (acceptDefaultCheckbox.isSelected()) {
                    acceptDefaultCheckbox.setSelected(false);
                }
                else {
                    acceptDefaultCheckbox.setSelected(true);
                }
            }
            else if (event.getSource() == cbobx_mstv_inter_control) {
                InterControlAction();
            }
        } // end of method actionPerformed
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    if (!isError()) {
                        if (gdvsim.flag_simulationData_ok) {
                            if (!gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control.equals(cbobx_mstv_inter_control.getSelectedItem().toString())) {
                                gdvsim.resetSimulationGlobalValue();
                                setDefaultNumberOfPhases();
                            }
                        }
                        else {
                            setDefaultNumberOfPhases();
                        }

                        gdvsim.flag_simulationData_ok = true;
                        saveData();
                        setDisplay();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
                else if (event.getSource() == VMSmesgButton) {
                    if (gdvsim.flag_simulationData_ok) {
                        new VMS_MessageDialog();
                    }
                    else {
                        JOptionPane.showMessageDialog(null, "Please OK or Apply the SIM Parameter-Option Data before accessing the VMS Message Data.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    }
                }
                else if (event.getSource() == acceptDefaultCheckbox) {
                    if (acceptDefaultCheckbox.isSelected()) {
                        acceptDefaultCheckbox.setSelected(false);
                    }
                    else {
                        acceptDefaultCheckbox.setSelected(true);
                    }
                }
                else if (event.getSource() == cbobx_mstv_inter_control) {
                    InterControlAction();
                }

            } // end of if(event.getKeyCode() == KeyEvent.VK_ENTER)
        } // end of class keyPressed
    } // end of class ComponentKeyListener

} // end of class SimulationDataDialog
