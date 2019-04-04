package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                         LegVaryTrafDialog.java                             */
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
import java.text.DecimalFormat;
import javax.swing.*;

class LegVaryTrafDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    int NUMOFFIELD = 22;

    static final int VAR_TRAF_PER_PERIOD_DUR = 1;

    static final int VAR_TRAF_PER_PCT_LT = 2;

    static final int VAR_TRAF_PER_PCT_RT = 3;

    static final int VAR_TRAF_PER_PER_US_FUT = 4;

    static final int VAR_TRAF_PER_PCT_1 = 5;

    static final int VAR_TRAF_PER_PCT_2 = 6;

    static final int VAR_TRAF_PER_PCT_3 = 7;

    static final int VAR_TRAF_PER_PCT_4 = 8;

    static final int VAR_TRAF_PER_PCT_5 = 9;

    static final int VAR_TRAF_PER_PCT_6 = 10;

    static final int VAR_TRAF_PER_DIST = 11;

    static final int VAR_TRAF_PER_VOL = 12;

    static final int VAR_TRAF_PER_PAR = 13;

    static final int VAR_TRAF_PER_MEAN = 14;

    static final int VAR_TRAF_PER_85TH = 15;

    static final int VAR_TRAF_PER_DEST_PER_1 = 16;

    static final int VAR_TRAF_PER_DEST_PER_2 = 17;

    static final int VAR_TRAF_PER_DEST_PER_3 = 18;

    static final int VAR_TRAF_PER_DEST_PER_4 = 19;

    static final int VAR_TRAF_PER_DEST_PER_5 = 20;

    static final int VAR_TRAF_PER_DEST_PER_6 = 21;

    static final int VAR_TRAF_PER_TM = 22;

    JFrame aFrame;

    Container container;

    GridBagLayout gbLayout;

    GridBagConstraints gbConstraints;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JComboBox[][] cbo_vtp = new JComboBox[PARAMS.TEXAS_MODEL_NVT + 1][NUMOFFIELD + 1];

    JComboBox[] sum_pct = new JComboBox[PARAMS.TEXAS_MODEL_NVT + 1];

    JComboBox[] sum_dest = new JComboBox[PARAMS.TEXAS_MODEL_NVT + 1];

    JLabel[] label_vtp = new JLabel[PARAMS.TEXAS_MODEL_NVT + 1];

    JButton[] add = new JButton[PARAMS.TEXAS_MODEL_NVT + 1];

    JButton[] del = new JButton[PARAMS.TEXAS_MODEL_NVT + 1];

    JButton[] up = new JButton[PARAMS.TEXAS_MODEL_NVT + 1];

    JButton[] down = new JButton[PARAMS.TEXAS_MODEL_NVT + 1];

    JButton[] edit = new JButton[PARAMS.TEXAS_MODEL_NVT + 1];

    JTextArea label_field_sum_pct, label_field_sum_dest;

    JComboBox cbo_total_vtp, cbo_total_pt;

    JLabel label_title, label_total_vtp, label_total_pt;

    JButton okButton, applyButton, cancelButton;

    Font font, font1;

    TX_Fmt lclv_tx_fmt;

    int numOfVTPs;

    String titleString;

    DecimalFormat oneDigits = new DecimalFormat("0.0");

    DecimalFormat twoDigits = new DecimalFormat("0.00");

    int leg_number;

    int number_of_inbounds;

    int number_of_legs;

    int[] default_msia_dest_per = new int[PARAMS.TEXAS_MODEL_NLGP1];

    int[] default_msia_pct = new int[PARAMS.TEXAS_MODEL_NAL + 1];

    int default_msia_period_duration;

    int default_msia_pct_lt;

    int default_msia_pct_rt;

    int default_msia_percent_using;

    String default_mstv_dist;

    int default_msia_vol;

    double default_mdfv_par;

    double default_mdfv_mean;

    double default_mdfv_85th;

    String default_mstv_tm;

    double[][][] default_mcla_var_traf_period_value = new double[PARAMS.TEXAS_MODEL_NLGP1][PARAMS.TEXAS_MODEL_NVT + 1][PARAMS.TEXAS_MODEL_NVC + 1];

    public LegVaryTrafDialog(int leg_num_param) {
        leg_number = leg_num_param;

        if (gdvsim.gclv_inter.mboa_Leg_Varying_Traffic_Period_OK[leg_number]) {
            numOfVTPs = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods;
        }
        else {
            numOfVTPs = 0;
        }

        number_of_inbounds = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        number_of_legs = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX];

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            for (int lsiv_traf = 1; lsiv_traf <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_traf++) {
                default_mcla_var_traf_period_value[leg_number][lsiv_vtp][lsiv_traf] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf];
            }
        }

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            for (int lsiv_traf = 1; lsiv_traf <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_traf++) {
                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM_PER_01 - 1
                        + lsiv_traf] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp][lsiv_traf] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp][lsiv_traf] = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_MIX_PER_VEH_CLASS_01 - 1 + lsiv_traf];
                }
                else {
                    gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp][lsiv_traf] = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM_PER_01
                            - 1 + lsiv_traf];
                    gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp][lsiv_traf] = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_mix.mdfa_per_veh_class[lsiv_traf];
                }
            }
        }

        // set 0
        default_msia_period_duration = 0;
        default_msia_pct_lt = 0;
        default_msia_pct_rt = 0;
        default_msia_percent_using = 0;
        default_mstv_dist = " ";
        default_msia_vol = 0;
        default_mdfv_par = 0.00;
        default_mdfv_mean = 0.00;
        default_mdfv_85th = 0.00;
        default_mstv_tm = " ";

        for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
            default_msia_pct[lsiv_lane] = 0;
        }

        for (int lsiv_leg = 1; lsiv_leg <= PARAMS.TEXAS_MODEL_NLG; lsiv_leg++) {
            default_msia_dest_per[lsiv_leg] = 0;
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

        // TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR

        default_msia_period_duration = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR];

        // TX_FMT_GDV_VAR_TRAF_PER_PCT_LT

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_LT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_msia_pct_lt = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT];
        }
        else {
            default_msia_pct_lt = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_pct_lt;
        }

        // TX_FMT_GDV_VAR_TRAF_PER_PCT_RT

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_PCT_RT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_msia_pct_rt = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT];
        }
        else {
            default_msia_pct_rt = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_pct_rt;
        }

        // TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            if (leg_number == 3) {
                if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_PERCENT_USING] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                        && (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width != 0)) {
                    default_msia_percent_using = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_percent_using;
                }
                else {
                    default_msia_percent_using = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT];
                }
            }

            if (leg_number == 6) {
                if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_PERCENT_USING] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                        && (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width != 0)) {
                    default_msia_percent_using = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_percent_using;
                }
                else {
                    default_msia_percent_using = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT];
                }
            }
        }

        // TX_FMT_GDV_VAR_TRAF_PER_PCT_1

        if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_PCT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_msia_pct[1] = setDefaultForPctValue(1, number_of_inbounds);
        }
        else {
            default_msia_pct[1] = gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct;
        }

        // TX_FMT_GDV_VAR_TRAF_PER_PCT_2

        if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_PCT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_msia_pct[2] = setDefaultForPctValue(2, number_of_inbounds);
        }
        else {
            default_msia_pct[2] = gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct;
        }

        // TX_FMT_GDV_VAR_TRAF_PER_PCT_3

        if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_PCT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_msia_pct[3] = setDefaultForPctValue(3, number_of_inbounds);
        }
        else {
            default_msia_pct[3] = gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct;
        }

        // TX_FMT_GDV_VAR_TRAF_PER_PCT_4

        if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_PCT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_msia_pct[4] = setDefaultForPctValue(4, number_of_inbounds);
        }
        else {
            default_msia_pct[4] = gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct;
        }

        // TX_FMT_GDV_VAR_TRAF_PER_PCT_5

        if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_PCT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_msia_pct[5] = setDefaultForPctValue(5, number_of_inbounds);
        }
        else {
            default_msia_pct[5] = gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct;
        }

        // TX_FMT_GDV_VAR_TRAF_PER_PCT_6

        if (gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[6].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LANE_DATA_PCT] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_msia_pct[6] = setDefaultForPctValue(6, number_of_inbounds);
        }
        else {
            default_msia_pct[6] = gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[6].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct;
        }

        // TX_FMT_GDV_VAR_TRAF_PER_DIST

        if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_DIST] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_mstv_dist = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DIST];
        }
        else {
            default_mstv_dist = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mstv_dist;
        }

        // TX_FMT_GDV_VAR_TRAF_PER_VOL

        if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_VOL] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_msia_vol = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL];
        }
        else {
            default_msia_vol = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.msiv_vol;
        }

        // TX_FMT_GDV_VAR_TRAF_PER_PAR

        if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_PAR] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_mdfv_par = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PAR];
        }
        else {
            default_mdfv_par = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_par;
        }

        // TX_FMT_GDV_VAR_TRAF_PER_MEAN

        if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_MEAN] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_mdfv_mean = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN];
        }
        else {
            default_mdfv_mean = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_mean;
        }

        // TX_FMT_GDV_VAR_TRAF_PER_85TH

        if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_85TH] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_mdfv_85th = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH];
        }
        else {
            default_mdfv_85th = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_85th;
        }

        for (int lsiv_leg = 1; lsiv_leg <= number_of_legs; lsiv_leg++) {
            if (gdvsim.gclv_inter.mboa_Leg_Outbound_Data_OK[leg_number]
                    && gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1
                            + lsiv_leg] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                default_msia_dest_per[lsiv_leg] = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[lsiv_leg];
            }
            else {
                default_msia_dest_per[lsiv_leg] = setDefaultForDest(lsiv_leg);
            }
        }

        // TX_FMT_GDV_VAR_TRAF_PER_TM

        if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_TRAF_HDWAY_TM] == gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
            default_mstv_tm = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM];
        }
        else {
            default_mstv_tm = gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mstv_tm;
        }

        titleString = lclv_tx_fmt.mstv_name;
        titleString = "Varying Traffic Period Data for Leg " + leg_number;

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
        font1 = new Font("TimesRoman", Font.BOLD, 12);

        label_title = new JLabel(titleString);
        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        label_vtp[0] = new JLabel("Period ");
        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            label_vtp[lsiv_vtp] = new JLabel(Integer.toString(lsiv_vtp));
        }

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            add[lsiv_vtp] = new JButton("Add");
            del[lsiv_vtp] = new JButton("Delete");
            up[lsiv_vtp] = new JButton("Up");
            down[lsiv_vtp] = new JButton("Down");
            edit[lsiv_vtp] = new JButton("Edit");
        }

        label_total_vtp = new JLabel("Total Varying Traffic Period");
        label_total_pt = new JLabel("Total Period Durations (should be " + gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time + ")");

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            label_field[lsiv_field] = new JTextArea();
            label_field[lsiv_field].setBackground(aFrame.getBackground());
            label_field[lsiv_field].setEditable(false);
            label_field[lsiv_field].setFocusable(false);
            label_field[lsiv_field].setWrapStyleWord(true);
            label_field[lsiv_field].setLineWrap(true);
            label_field[lsiv_field].setFont(font1);
        }

        label_field_sum_pct = new JTextArea();
        label_field_sum_pct.setBackground(aFrame.getBackground());
        label_field_sum_pct.setFocusable(false);
        label_field_sum_pct.setEditable(false);
        label_field_sum_pct.setWrapStyleWord(true);
        label_field_sum_pct.setLineWrap(true);
        label_field_sum_pct.setFont(font1);

        label_field_sum_dest = new JTextArea();
        label_field_sum_dest.setBackground(aFrame.getBackground());
        label_field_sum_dest.setFocusable(false);
        label_field_sum_dest.setEditable(false);
        label_field_sum_dest.setWrapStyleWord(true);
        label_field_sum_dest.setLineWrap(true);
        label_field_sum_dest.setFont(font1);

        label_field[VAR_TRAF_PER_PERIOD_DUR].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR]);
        label_field[VAR_TRAF_PER_PCT_LT].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT]);
        label_field[VAR_TRAF_PER_PCT_RT].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT]);
        label_field[VAR_TRAF_PER_PER_US_FUT].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT]);
        label_field[VAR_TRAF_PER_PCT_1].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1]);
        label_field[VAR_TRAF_PER_PCT_2].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2]);
        label_field[VAR_TRAF_PER_PCT_3].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3]);
        label_field[VAR_TRAF_PER_PCT_4].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4]);
        label_field[VAR_TRAF_PER_PCT_5].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5]);
        label_field[VAR_TRAF_PER_PCT_6].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6]);
        label_field[VAR_TRAF_PER_DIST].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DIST]);
        label_field[VAR_TRAF_PER_VOL].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL]);
        label_field[VAR_TRAF_PER_PAR].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PAR]);
        label_field[VAR_TRAF_PER_MEAN].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN]);
        label_field[VAR_TRAF_PER_85TH].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH]);
        label_field[VAR_TRAF_PER_DEST_PER_1].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1]);
        label_field[VAR_TRAF_PER_DEST_PER_2].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2]);
        label_field[VAR_TRAF_PER_DEST_PER_3].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3]);
        label_field[VAR_TRAF_PER_DEST_PER_4].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4]);
        label_field[VAR_TRAF_PER_DEST_PER_5].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5]);
        label_field[VAR_TRAF_PER_DEST_PER_6].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6]);
        label_field[VAR_TRAF_PER_TM].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM]);

        label_field_sum_pct.setText("Sum Of Percent Of Inbound Traffic To Enter Lane");
        label_field_sum_dest.setText("Sum Of Percent Of Inbound Traffic With Destination");

        label_field_sum_pct.setSize(new Dimension(45, 26));
        label_field_sum_dest.setSize(new Dimension(65, 26));

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            double lsiv_min_double;
            double lsiv_max_double;
            double lsiv_inc_double;

            int lsiv_min_int;
            int lsiv_max_int;
            int lsiv_inc_int;

            int arrayIndex, SizeOfArray, intArrayElementValue, seperateIndex;
            int count, index, int_number;
            double double_number, doubleArrayElementValue;

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR];

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_period_duration = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_period_duration[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT];

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_pct_lt = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_pct_lt[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT];

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_pct_rt = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_pct_rt[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT];

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_percent_using = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_percent_using[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1];

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_pct_1 = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_pct_1[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2];

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_pct_2 = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_pct_2[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3];

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_pct_3 = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_pct_3[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4];

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_pct_4 = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_pct_4[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5];

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_pct_5 = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_pct_5[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6];

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_pct_6 = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_pct_6[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            String[] array_mstv_dist = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DIST].substring(1).split("\\|");

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL];

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_vol = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_vol[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PAR];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PAR];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PAR];

            count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            double_number = lsiv_min_double;
            String[] array_mdfv_par = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_mdfv_par[lsiv_i] = twoDigits.format(double_number);
                double_number += lsiv_inc_double;
            }

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN];

            count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            double_number = lsiv_min_double;
            String[] array_mdfv_mean = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_mdfv_mean[lsiv_i] = oneDigits.format(double_number);
                double_number += lsiv_inc_double;
            }

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH];

            count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            double_number = lsiv_min_double;
            String[] array_mdfv_85th = new String[count];
            for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
                array_mdfv_85th[lsiv_i] = oneDigits.format(double_number);
                double_number += lsiv_inc_double;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1];

            if (gdvsim.gclv_inter.mboa_Leg_OK[1] && gdvsim.gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0) {
                lsiv_min_int = lsiv_max_int = 0;
            }

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_dest_per_1 = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_dest_per_1[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2];

            if (gdvsim.gclv_inter.mboa_Leg_OK[2] && gdvsim.gclv_inter.mcla_leg[2].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0) {
                lsiv_min_int = lsiv_max_int = 0;
            }

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_dest_per_2 = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_dest_per_2[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3];

            if (gdvsim.gclv_inter.mboa_Leg_OK[3] && gdvsim.gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0) {
                lsiv_min_int = lsiv_max_int = 0;
            }

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_dest_per_3 = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_dest_per_3[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4];

            if (gdvsim.gclv_inter.mboa_Leg_OK[4] && gdvsim.gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0) {
                lsiv_min_int = lsiv_max_int = 0;
            }

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_dest_per_4 = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_dest_per_4[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5];

            if (gdvsim.gclv_inter.mboa_Leg_OK[5] && gdvsim.gclv_inter.mcla_leg[5].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0) {
                lsiv_min_int = lsiv_max_int = 0;
            }

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_dest_per_5 = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_dest_per_5[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6];
            lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6];
            lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6];

            if (gdvsim.gclv_inter.mboa_Leg_OK[6] && gdvsim.gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0) {
                lsiv_min_int = lsiv_max_int = 0;
            }

            count = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
            int_number = lsiv_min_int;
            String[] array_msia_dest_per_6 = new String[count];
            for (int lsiv_index = 0; lsiv_index < count; lsiv_index++) {
                array_msia_dest_per_6[lsiv_index] = Integer.toString(int_number);
                int_number += lsiv_inc_int;
            }

            String[] array_mstv_tm = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM].substring(1).split("\\|");

            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR] = new JComboBox(array_msia_period_duration);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_LT] = new JComboBox(array_msia_pct_lt);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_RT] = new JComboBox(array_msia_pct_rt);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PER_US_FUT] = new JComboBox(array_msia_percent_using);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1] = new JComboBox(array_msia_pct_1);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2] = new JComboBox(array_msia_pct_2);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3] = new JComboBox(array_msia_pct_3);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4] = new JComboBox(array_msia_pct_4);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5] = new JComboBox(array_msia_pct_5);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6] = new JComboBox(array_msia_pct_6);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DIST] = new JComboBox(array_mstv_dist);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_VOL] = new JComboBox(array_msia_vol);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR] = new JComboBox(array_mdfv_par);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_MEAN] = new JComboBox(array_mdfv_mean);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_85TH] = new JComboBox(array_mdfv_85th);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1] = new JComboBox(array_msia_dest_per_1);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2] = new JComboBox(array_msia_dest_per_2);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3] = new JComboBox(array_msia_dest_per_3);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4] = new JComboBox(array_msia_dest_per_4);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5] = new JComboBox(array_msia_dest_per_5);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6] = new JComboBox(array_msia_dest_per_6);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM] = new JComboBox(array_mstv_tm);
        }

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            label_field[VAR_TRAF_PER_PERIOD_DUR].setSize(new Dimension(45, 26));
            label_field[VAR_TRAF_PER_PCT_LT].setSize(new Dimension(60, 26));
            label_field[VAR_TRAF_PER_PCT_RT].setSize(new Dimension(60, 26));
            label_field[VAR_TRAF_PER_PER_US_FUT].setSize(new Dimension(70, 26));
            label_field[VAR_TRAF_PER_PCT_1].setSize(new Dimension(60, 26));
            label_field[VAR_TRAF_PER_PCT_2].setSize(new Dimension(60, 26));
            label_field[VAR_TRAF_PER_PCT_3].setSize(new Dimension(45, 26));
            label_field[VAR_TRAF_PER_PCT_4].setSize(new Dimension(45, 26));
            label_field[VAR_TRAF_PER_PCT_5].setSize(new Dimension(45, 26));
            label_field[VAR_TRAF_PER_PCT_6].setSize(new Dimension(45, 26));
            label_field[VAR_TRAF_PER_DIST].setSize(new Dimension(45, 26));
            label_field[VAR_TRAF_PER_VOL].setSize(new Dimension(45, 26));
            label_field[VAR_TRAF_PER_PAR].setSize(new Dimension(65, 26));
            label_field[VAR_TRAF_PER_MEAN].setSize(new Dimension(45, 26));
            label_field[VAR_TRAF_PER_85TH].setSize(new Dimension(45, 26));
            label_field[VAR_TRAF_PER_DEST_PER_1].setSize(new Dimension(60, 26));
            label_field[VAR_TRAF_PER_DEST_PER_2].setSize(new Dimension(60, 26));
            label_field[VAR_TRAF_PER_DEST_PER_3].setSize(new Dimension(60, 26));
            label_field[VAR_TRAF_PER_DEST_PER_4].setSize(new Dimension(60, 26));
            label_field[VAR_TRAF_PER_DEST_PER_5].setSize(new Dimension(60, 26));
            label_field[VAR_TRAF_PER_DEST_PER_6].setSize(new Dimension(60, 26));
            label_field[VAR_TRAF_PER_TM].setSize(new Dimension(70, 26));

            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR].setSize(new Dimension(45, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_LT].setSize(new Dimension(60, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_RT].setSize(new Dimension(60, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PER_US_FUT].setSize(new Dimension(70, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].setSize(new Dimension(60, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].setSize(new Dimension(60, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].setSize(new Dimension(45, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].setSize(new Dimension(45, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].setSize(new Dimension(45, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].setSize(new Dimension(45, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DIST].setSize(new Dimension(45, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_VOL].setSize(new Dimension(45, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].setSize(new Dimension(65, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_MEAN].setSize(new Dimension(45, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_85TH].setSize(new Dimension(45, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].setSize(new Dimension(60, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].setSize(new Dimension(60, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].setSize(new Dimension(60, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].setSize(new Dimension(60, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].setSize(new Dimension(60, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].setSize(new Dimension(60, 26));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM].setSize(new Dimension(70, 26));

            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR].setSelectedItem(Integer.toString(default_msia_period_duration));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_LT].setSelectedItem(Integer.toString(default_msia_pct_lt));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_RT].setSelectedItem(Integer.toString(default_msia_pct_rt));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PER_US_FUT].setSelectedItem(Integer.toString(default_msia_percent_using));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].setSelectedItem(Integer.toString(default_msia_pct[1]));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].setSelectedItem(Integer.toString(default_msia_pct[2]));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].setSelectedItem(Integer.toString(default_msia_pct[3]));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].setSelectedItem(Integer.toString(default_msia_pct[4]));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].setSelectedItem(Integer.toString(default_msia_pct[5]));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].setSelectedItem(Integer.toString(default_msia_pct[6]));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DIST].setSelectedItem(default_mstv_dist);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_VOL].setSelectedItem(Integer.toString(default_msia_vol));
            resetDistParameterList(lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].setSelectedItem(twoDigits.format(default_mdfv_par));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_MEAN].setSelectedItem(oneDigits.format(default_mdfv_mean));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_85TH].setSelectedItem(oneDigits.format(default_mdfv_85th));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].setSelectedItem(Integer.toString(default_msia_dest_per[1]));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].setSelectedItem(Integer.toString(default_msia_dest_per[2]));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].setSelectedItem(Integer.toString(default_msia_dest_per[3]));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].setSelectedItem(Integer.toString(default_msia_dest_per[4]));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].setSelectedItem(Integer.toString(default_msia_dest_per[5]));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].setSelectedItem(Integer.toString(default_msia_dest_per[6]));
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM].setSelectedItem(default_mstv_tm);
        }

        if (gdvsim.gclv_inter.mboa_Leg_Varying_Traffic_Period_OK[leg_number]) {
            for (int lsiv_vtp = 1; lsiv_vtp <= numOfVTPs; lsiv_vtp++) {
                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_period_duration));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_LT].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_pct_lt));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_RT].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_pct_rt));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PER_US_FUT].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_percent_using));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct[1]));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct[2]));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct[3]));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct[4]));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct[5]));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct[6]));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DIST] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DIST].setSelectedItem(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_dist);
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_VOL].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_vol));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PAR] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].setSelectedItem(twoDigits.format(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mdfv_par));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_MEAN].setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mdfv_mean));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_85TH].setSelectedItem(oneDigits.format(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mdfv_85th));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per[1]));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per[2]));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per[3]));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per[4]));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per[5]));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per[6]));
                }

                if (gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM].setSelectedItem(gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_tm);
                }
            }
        } // end of if(gdvsim.gclv_inter.mboa_Leg_Varying_Traffic_Period_OK[leg_number])

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            sum_pct[lsiv_vtp] = new JComboBox();
            sum_dest[lsiv_vtp] = new JComboBox();

            sum_pct[lsiv_vtp].addItem(Integer.toString(getSumOfPCT(lsiv_vtp)));
            sum_dest[lsiv_vtp].addItem(Integer.toString(getSumOfDest(lsiv_vtp)));
        }

        setStatus(numOfVTPs);

        cbo_total_vtp = new JComboBox();
        cbo_total_pt = new JComboBox();

        cbo_total_vtp.addItem(Integer.toString(getSumOfVTP()));
        cbo_total_pt.addItem(Integer.toString(getSumOfPeriodTime()));

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

        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();
        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            add[lsiv_vtp].addActionListener(componentActionListener);
            del[lsiv_vtp].addActionListener(componentActionListener);
            up[lsiv_vtp].addActionListener(componentActionListener);
            down[lsiv_vtp].addActionListener(componentActionListener);
            edit[lsiv_vtp].addActionListener(componentActionListener);

            add[lsiv_vtp].addKeyListener(componentKeyListener);
            del[lsiv_vtp].addKeyListener(componentKeyListener);
            up[lsiv_vtp].addKeyListener(componentKeyListener);
            down[lsiv_vtp].addKeyListener(componentKeyListener);
            edit[lsiv_vtp].addKeyListener(componentKeyListener);

            add[lsiv_vtp].addKeyListener(helpListener);
            del[lsiv_vtp].addKeyListener(helpListener);
            up[lsiv_vtp].addKeyListener(helpListener);
            down[lsiv_vtp].addKeyListener(helpListener);
            sum_pct[lsiv_vtp].addKeyListener(helpListener);
            sum_dest[lsiv_vtp].addKeyListener(helpListener);
            edit[lsiv_vtp].addKeyListener(helpListener);

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                cbo_vtp[lsiv_vtp][lsiv_field].addActionListener(componentActionListener);
                cbo_vtp[lsiv_vtp][lsiv_field].addKeyListener(componentKeyListener);
                cbo_vtp[lsiv_vtp][lsiv_field].addKeyListener(openComboMenuListener);
                cbo_vtp[lsiv_vtp][lsiv_field].addKeyListener(helpListener);
            }
        }

        cbo_total_vtp.addKeyListener(helpListener);
        cbo_total_pt.addKeyListener(helpListener);
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

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            if (leg_number == 3 && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width != 0) {
                for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PER_US_FUT].setVisible(true);
                    label_field[VAR_TRAF_PER_PER_US_FUT].setVisible(true);
                }
            }
            else if (leg_number == 6 && gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width != 0) {
                for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PER_US_FUT].setVisible(true);
                    label_field[VAR_TRAF_PER_PER_US_FUT].setVisible(true);
                }
            }
            else {
                for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
                    cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PER_US_FUT].setVisible(false);
                    label_field[VAR_TRAF_PER_PER_US_FUT].setVisible(false);
                }
            }
        }
        else {
            for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PER_US_FUT].setVisible(false);
                label_field[VAR_TRAF_PER_PER_US_FUT].setVisible(false);
            }
        }

        int iRow = 0;
        int sumOfCols = 20;

        int iCol;

        gbConstraints.insets = new Insets(1, 0, 10, 0);
        addComponent(panel_title, iRow++, 0, sumOfCols, 1);

        gbConstraints.insets = new Insets(1, 1, 1, 1);

        iCol = 5;
        for (int lsiv_field = VAR_TRAF_PER_PERIOD_DUR; lsiv_field <= VAR_TRAF_PER_PCT_6; lsiv_field++) {
            addComponent(label_field[lsiv_field], iRow, iCol++, 1, 1);
        }

        addComponent(label_field_sum_pct, iRow, iCol++, 1, 1);

        for (int lsiv_field = VAR_TRAF_PER_DIST; lsiv_field <= VAR_TRAF_PER_DEST_PER_6; lsiv_field++) {
            addComponent(label_field[lsiv_field], iRow, iCol++, 1, 1);
        }

        addComponent(label_field_sum_dest, iRow, iCol++, 1, 1);

        for (int lsiv_field = VAR_TRAF_PER_TM; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            addComponent(label_field[lsiv_field], iRow, iCol++, 1, 1);
        }

        iRow++;

        addComponent(label_vtp[0], iRow++, 0, 4, 1);

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            iCol = 0;
            gbConstraints.insets = new Insets(1, 1, 1, 1);
            addComponent(label_vtp[lsiv_vtp], iRow, iCol++, 1, 1);
            addComponent(add[lsiv_vtp], iRow, iCol++, 1, 1);
            addComponent(del[lsiv_vtp], iRow, iCol++, 1, 1);
            addComponent(up[lsiv_vtp], iRow, iCol++, 1, 1);
            addComponent(down[lsiv_vtp], iRow, iCol++, 1, 1);

            gbConstraints.insets = new Insets(1, 2, 1, 2);

            for (int lsiv_field = VAR_TRAF_PER_PERIOD_DUR; lsiv_field <= VAR_TRAF_PER_PCT_6; lsiv_field++) {
                addComponent(cbo_vtp[lsiv_vtp][lsiv_field], iRow, iCol++, 1, 1);
            }

            addComponent(sum_pct[lsiv_vtp], iRow, iCol++, 1, 1);

            for (int lsiv_field = VAR_TRAF_PER_DIST; lsiv_field <= VAR_TRAF_PER_DEST_PER_6; lsiv_field++) {
                addComponent(cbo_vtp[lsiv_vtp][lsiv_field], iRow, iCol++, 1, 1);
            }

            addComponent(sum_dest[lsiv_vtp], iRow, iCol++, 1, 1);

            for (int lsiv_field = VAR_TRAF_PER_TM; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                addComponent(cbo_vtp[lsiv_vtp][lsiv_field], iRow, iCol++, 1, 1);
            }

            addComponent(edit[lsiv_vtp], iRow, iCol++, 1, 1);

            iRow++;
        }

        addComponent(cbo_total_vtp, iRow, 1, 1, 1);
        addComponent(label_total_vtp, iRow, 2, 3, 1);
        addComponent(cbo_total_pt, iRow, 5, 1, 1);
        addComponent(label_total_pt, iRow++, 6, 8, 1);

        addComponent(ok_panel, iRow++, 0, sumOfCols, 1);

        aFrame.setSize(1000, 700);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        // aFrame.pack();
        // aFrame.setLocation(SCREEN_SIZE.width/2 - aFrame.getWidth()/2,
        // SCREEN_SIZE.height/2 - aFrame.getHeight()/2);

    } // end of method LegVaryTrafDialog

    void addComponent(Component c, int row, int column, int width, int height) {
        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    int getSumOfVTP() {
        int sum = 0;

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            if (cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR].isEnabled())
                sum++;
        }

        return sum;
    } // end of getSumOfVTP()

    int getSumOfPeriodTime() {
        int sum = 0;

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            if (cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR].isEnabled()) {
                sum = sum + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR].getSelectedItem().toString()).intValue();
            }
        }

        return sum;
    } // end of getSumOfVTP()

    int getSumOfPCT(int lsiv_vtp) {
        if (number_of_inbounds == 1) {
            return Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue();
        }

        if (number_of_inbounds == 2) {
            return Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getSelectedItem().toString()).intValue();
        }

        if (number_of_inbounds == 3) {
            return Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].getSelectedItem().toString()).intValue();
        }

        if (number_of_inbounds == 4) {
            return Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].getSelectedItem().toString()).intValue();
        }

        if (number_of_inbounds == 5) {
            return Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].getSelectedItem().toString()).intValue();
        }

        if (number_of_inbounds == 6) {
            return Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].getSelectedItem().toString()).intValue();
        }

        return 0;
    } // end of getSumOfPCT()

    int getSumOfDest(int lsiv_vtp) {
        if (number_of_legs == 1) {
            return Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].getSelectedItem().toString()).intValue();
        }

        if (number_of_legs == 2) {
            return Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].getSelectedItem().toString()).intValue();
        }

        if (number_of_legs == 3) {
            return Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].getSelectedItem().toString()).intValue();
        }

        if (number_of_legs == 4) {
            return Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].getSelectedItem().toString()).intValue();
        }

        if (number_of_legs == 5) {
            return Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].getSelectedItem().toString()).intValue();
        }

        if (number_of_legs == 6) {
            return Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].getSelectedItem().toString()).intValue();
        }

        return 0;
    } // end of getSumOfDest()

    void setAccessiblility() {
        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_LT].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_RT].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PER_US_FUT].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DIST].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DIST] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_VOL].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PAR] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_MEAN].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_85TH].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].getAccessibleContext().setAccessibleName(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM].getAccessibleContext()
                    .setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM] + " for varying traffic period " + lsiv_vtp);

            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_LT].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_RT].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PER_US_FUT].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DIST].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DIST] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_VOL].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PAR] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_MEAN].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_85TH].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6] + " for varying traffic period " + lsiv_vtp);
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM].getAccessibleContext().setAccessibleDescription(
                    lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM] + " for varying traffic period " + lsiv_vtp);

            edit[lsiv_vtp].getAccessibleContext().setAccessibleName("Edit Traffic Mix Data for varying traffic period " + lsiv_vtp);
            edit[lsiv_vtp].getAccessibleContext().setAccessibleDescription("Edit Traffic Mix Data for varying traffic period " + lsiv_vtp);

            add[lsiv_vtp].getAccessibleContext().setAccessibleName("Add                   for varying traffic period " + lsiv_vtp);
            add[lsiv_vtp].getAccessibleContext().setAccessibleDescription("Add                   for varying traffic period " + lsiv_vtp);
            del[lsiv_vtp].getAccessibleContext().setAccessibleName("Delete                for varying traffic period " + lsiv_vtp);
            del[lsiv_vtp].getAccessibleContext().setAccessibleDescription("Delete                for varying traffic period " + lsiv_vtp);
            up[lsiv_vtp].getAccessibleContext().setAccessibleName("Up                    for varying traffic period " + lsiv_vtp);
            up[lsiv_vtp].getAccessibleContext().setAccessibleDescription("Up                    for varying traffic period " + lsiv_vtp);
            down[lsiv_vtp].getAccessibleContext().setAccessibleName("Down                  for varying traffic period " + lsiv_vtp);
            down[lsiv_vtp].getAccessibleContext().setAccessibleDescription("Down                  for varying traffic period " + lsiv_vtp);

            sum_dest[lsiv_vtp].getAccessibleContext().setAccessibleName(label_field_sum_dest.getText() + " for varying traffic period " + lsiv_vtp);
            sum_pct[lsiv_vtp].getAccessibleContext().setAccessibleDescription(label_field_sum_dest.getText() + " for varying traffic period " + lsiv_vtp);
        }

        cbo_total_vtp.getAccessibleContext().setAccessibleName(label_total_vtp.getText());
        cbo_total_vtp.getAccessibleContext().setAccessibleDescription(label_total_vtp.getText());

        cbo_total_pt.getAccessibleContext().setAccessibleName(label_total_pt.getText());
        cbo_total_pt.getAccessibleContext().setAccessibleDescription(label_total_pt.getText());

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

    void setStatus(int sumOfVTP) {
        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            if (lsiv_vtp <= sumOfVTP) {
                label_vtp[lsiv_vtp].setEnabled(true);
                sum_pct[lsiv_vtp].setEnabled(true);
                sum_dest[lsiv_vtp].setEnabled(true);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    cbo_vtp[lsiv_vtp][lsiv_field].setEnabled(true);
                }

                if (cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM].getSelectedItem().toString().equals("YES")) {
                    edit[lsiv_vtp].setEnabled(true);
                }
                else {
                    edit[lsiv_vtp].setEnabled(false);
                }
            }
            else {
                label_vtp[lsiv_vtp].setEnabled(false);
                sum_pct[lsiv_vtp].setEnabled(false);
                sum_dest[lsiv_vtp].setEnabled(false);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    cbo_vtp[lsiv_vtp][lsiv_field].setEnabled(false);
                }
                edit[lsiv_vtp].setEnabled(false);
            }
        }

        if (sumOfVTP == PARAMS.TEXAS_MODEL_NVT) {
            for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
                add[lsiv_vtp].setEnabled(false);
            }
        }
        else {
            for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
                if (lsiv_vtp <= (sumOfVTP + 1)) {
                    add[lsiv_vtp].setEnabled(true);
                }
                else {
                    add[lsiv_vtp].setEnabled(false);
                }
            }
        }

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            if (lsiv_vtp <= sumOfVTP) {
                del[lsiv_vtp].setEnabled(true);
            }
            else {
                del[lsiv_vtp].setEnabled(false);
            }
        }

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            if (lsiv_vtp <= sumOfVTP) {
                up[lsiv_vtp].setEnabled(true);
            }
            else {
                up[lsiv_vtp].setEnabled(false);
            }

            up[1].setEnabled(false);
        }

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            if (lsiv_vtp < sumOfVTP) {
                down[lsiv_vtp].setEnabled(true);
            }
            else {
                down[lsiv_vtp].setEnabled(false);
            }
        }

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            if (number_of_inbounds == 1) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].setVisible(false);

                label_field[VAR_TRAF_PER_PCT_1].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_2].setVisible(false);
                label_field[VAR_TRAF_PER_PCT_3].setVisible(false);
                label_field[VAR_TRAF_PER_PCT_4].setVisible(false);
                label_field[VAR_TRAF_PER_PCT_5].setVisible(false);
                label_field[VAR_TRAF_PER_PCT_6].setVisible(false);
            }
            else if (number_of_inbounds == 2) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].setVisible(false);

                label_field[VAR_TRAF_PER_PCT_1].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_2].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_3].setVisible(false);
                label_field[VAR_TRAF_PER_PCT_4].setVisible(false);
                label_field[VAR_TRAF_PER_PCT_5].setVisible(false);
                label_field[VAR_TRAF_PER_PCT_6].setVisible(false);
            }
            else if (number_of_inbounds == 3) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].setVisible(false);

                label_field[VAR_TRAF_PER_PCT_1].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_2].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_3].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_4].setVisible(false);
                label_field[VAR_TRAF_PER_PCT_5].setVisible(false);
                label_field[VAR_TRAF_PER_PCT_6].setVisible(false);
            }
            else if (number_of_inbounds == 4) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].setVisible(false);

                label_field[VAR_TRAF_PER_PCT_1].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_2].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_3].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_4].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_5].setVisible(false);
                label_field[VAR_TRAF_PER_PCT_6].setVisible(false);
            }
            else if (number_of_inbounds == 5) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].setVisible(false);

                label_field[VAR_TRAF_PER_PCT_1].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_2].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_3].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_4].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_5].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_6].setVisible(false);
            }
            else if (number_of_inbounds == 6) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].setVisible(true);

                label_field[VAR_TRAF_PER_PCT_1].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_2].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_3].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_4].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_5].setVisible(true);
                label_field[VAR_TRAF_PER_PCT_6].setVisible(true);
            }
        }

        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            if (number_of_legs == 1) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].setVisible(false);

                label_field[VAR_TRAF_PER_DEST_PER_1].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_2].setVisible(false);
                label_field[VAR_TRAF_PER_DEST_PER_3].setVisible(false);
                label_field[VAR_TRAF_PER_DEST_PER_4].setVisible(false);
                label_field[VAR_TRAF_PER_DEST_PER_5].setVisible(false);
                label_field[VAR_TRAF_PER_DEST_PER_6].setVisible(false);
            }

            if (number_of_legs == 2) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].setVisible(false);

                label_field[VAR_TRAF_PER_DEST_PER_1].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_2].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_3].setVisible(false);
                label_field[VAR_TRAF_PER_DEST_PER_4].setVisible(false);
                label_field[VAR_TRAF_PER_DEST_PER_5].setVisible(false);
                label_field[VAR_TRAF_PER_DEST_PER_6].setVisible(false);
            }

            if (number_of_legs == 3) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].setVisible(false);

                label_field[VAR_TRAF_PER_DEST_PER_1].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_2].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_3].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_4].setVisible(false);
                label_field[VAR_TRAF_PER_DEST_PER_5].setVisible(false);
                label_field[VAR_TRAF_PER_DEST_PER_6].setVisible(false);
            }

            if (number_of_legs == 4) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].setVisible(false);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].setVisible(false);

                label_field[VAR_TRAF_PER_DEST_PER_1].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_2].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_3].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_4].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_5].setVisible(false);
                label_field[VAR_TRAF_PER_DEST_PER_6].setVisible(false);
            }

            if (number_of_legs == 5) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].setVisible(false);

                label_field[VAR_TRAF_PER_DEST_PER_1].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_2].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_3].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_4].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_5].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_6].setVisible(false);
            }

            if (number_of_legs == 6) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].setVisible(true);
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].setVisible(true);

                label_field[VAR_TRAF_PER_DEST_PER_1].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_2].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_3].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_4].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_5].setVisible(true);
                label_field[VAR_TRAF_PER_DEST_PER_6].setVisible(true);
            }
        }
    } // end of setStatus

    int setDefaultForPctValue(int lane, int number_of_bounds) {
        int retValue = 0;

        if (number_of_bounds == 3) {
            if (lane == 3) {
                retValue = 34;
            }
            else {
                retValue = 33;
            }
        }
        else if (number_of_bounds == 6) {
            if (lane == 1 || lane == 2) {
                retValue = 16;
            }
            else {
                retValue = 17;
            }
        }
        else {
            if (number_of_bounds != 0) {
                retValue = 100 / number_of_bounds;
            }
        }

        return retValue;

    } // end of setDefaultForPctValue

    int setDefaultForDest(int lsiv_leg) {
        int retValue = 0;
        int countNoOutbound = 0;
        int lastLegWithOutbound;

        if (leg_number == number_of_legs) {
            lastLegWithOutbound = number_of_legs - 1;
        }
        else {
            lastLegWithOutbound = number_of_legs;
        }

        for (int lsiv_j = 1; lsiv_j <= number_of_legs; lsiv_j++) {
            if (lsiv_j == leg_number)
                continue;

            if (gdvsim.gclv_inter.mboa_Leg_OK[lsiv_j]
                    && (gdvsim.gclv_inter.mcla_leg[lsiv_j].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                    && (gdvsim.gclv_inter.mcla_leg[lsiv_j].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0)) {
                countNoOutbound++;
            }
            else {
                lastLegWithOutbound = lsiv_j;
            }
        }

        int eachValue = 0;
        int lastValue = 0;
        boolean allOutboundZero = false;

        if (countNoOutbound == (number_of_legs - 1)) {
            allOutboundZero = true;
        }

        if (!allOutboundZero) {
            int divided = number_of_legs - 1 - countNoOutbound;

            if (divided == 3) {
                eachValue = 33;
                lastValue = 34;
            }
            else {
                eachValue = 100 / divided;
                lastValue = 100 / divided;
            }
        }

        if (allOutboundZero) {
            retValue = 0;
        }
        else {
            if (lsiv_leg == leg_number) {
                retValue = 0;
            }
            else if (gdvsim.gclv_inter.mboa_Leg_OK[lsiv_leg]
                    && (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_NO_OUT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                    && (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0)) {
                retValue = 0;
            }
            else {
                if (lsiv_leg == lastLegWithOutbound) {
                    retValue = lastValue;
                }
                else {
                    retValue = eachValue;
                }
            }
        }

        return retValue;

    } // end of method setDefaultForDest

    void removeAllAction() {
        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                cbo_vtp[lsiv_vtp][lsiv_field].removeActionListener(componentActionListener);
                cbo_vtp[lsiv_vtp][lsiv_field].removeKeyListener(componentKeyListener);
            }
        }
    } // end of removeAllAction()

    void addAllAction() {
        for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                cbo_vtp[lsiv_vtp][lsiv_field].addActionListener(componentActionListener);
                cbo_vtp[lsiv_vtp][lsiv_field].addKeyListener(componentKeyListener);
            }
        }
    } // end of addAllAction()

    void resetDistParameterList(int lsiv_vtp) {
        for (int lsiv_vtp_i = 1; lsiv_vtp_i <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp_i++) {
            cbo_vtp[lsiv_vtp_i][VAR_TRAF_PER_DIST].removeActionListener(componentActionListener);
            cbo_vtp[lsiv_vtp_i][VAR_TRAF_PER_DIST].removeKeyListener(componentKeyListener);
        }

        double lsiv_min_double;
        double lsiv_max_double;
        double lsiv_inc_double;

        int arrayIndex, SizeOfArray, intArrayElementValue, seperateIndex;

        double double_number, doubleArrayElementValue;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

        // check and set headway distribution gdvsim.gclv_inter..mbov_is_hd_* values
        gdvsim.gclv_inter.check_and_set_headway_distribution(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DIST].getSelectedItem().toString());
        if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS)
            return;

        if (gdvsim.gclv_inter.mbov_is_hd_CONSTAN) {
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_ERLANG) {
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_GAMMA) {
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_LOGNRML) {
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_NEGEXP) {
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_SNEGEXP) {
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_UNIFORM) {
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]));
        }

        for (int lsiv_vtp_i = 1; lsiv_vtp_i <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp_i++) {
            cbo_vtp[lsiv_vtp_i][VAR_TRAF_PER_DIST].addActionListener(componentActionListener);
            cbo_vtp[lsiv_vtp_i][VAR_TRAF_PER_DIST].addKeyListener(componentKeyListener);
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

    } // resetDistParameterList

    void setValueAfterAdd(int sum, int index) {
        for (int lsiv_vtp = sum; lsiv_vtp >= index; lsiv_vtp--) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                String temp;
                cbo_vtp[lsiv_vtp + 1][lsiv_field].setSelectedItem(cbo_vtp[lsiv_vtp][lsiv_field].getSelectedItem().toString());
            }

            for (int lsiv_traf = 1; lsiv_traf <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_traf++) {
                gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp + 1][lsiv_traf] = gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp][lsiv_traf];
                gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp + 1][lsiv_traf] = gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp][lsiv_traf];
            }

        }
    } // end of setValueAfterAdd()

    void setValueAfterDel(int sum, int index) {
        for (int lsiv_vtp = index; lsiv_vtp < sum; lsiv_vtp++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                cbo_vtp[lsiv_vtp][lsiv_field].setSelectedItem(cbo_vtp[lsiv_vtp + 1][lsiv_field].getSelectedItem().toString());
            }

            for (int lsiv_traf = 1; lsiv_traf <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_traf++) {
                gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp][lsiv_traf] = gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp + 1][lsiv_traf];
                gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp][lsiv_traf] = gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp + 1][lsiv_traf];
            }
        }

        cbo_vtp[sum][VAR_TRAF_PER_PERIOD_DUR].setSelectedItem(Integer.toString(default_msia_period_duration));
        cbo_vtp[sum][VAR_TRAF_PER_PCT_LT].setSelectedItem(Integer.toString(default_msia_pct_lt));
        cbo_vtp[sum][VAR_TRAF_PER_PCT_RT].setSelectedItem(Integer.toString(default_msia_pct_rt));
        cbo_vtp[sum][VAR_TRAF_PER_PER_US_FUT].setSelectedItem(Integer.toString(default_msia_percent_using));
        cbo_vtp[sum][VAR_TRAF_PER_PCT_1].setSelectedItem(Integer.toString(default_msia_pct[1]));
        cbo_vtp[sum][VAR_TRAF_PER_PCT_2].setSelectedItem(Integer.toString(default_msia_pct[2]));
        cbo_vtp[sum][VAR_TRAF_PER_PCT_3].setSelectedItem(Integer.toString(default_msia_pct[3]));
        cbo_vtp[sum][VAR_TRAF_PER_PCT_4].setSelectedItem(Integer.toString(default_msia_pct[4]));
        cbo_vtp[sum][VAR_TRAF_PER_PCT_5].setSelectedItem(Integer.toString(default_msia_pct[5]));
        cbo_vtp[sum][VAR_TRAF_PER_PCT_6].setSelectedItem(Integer.toString(default_msia_pct[6]));
        cbo_vtp[sum][VAR_TRAF_PER_DIST].setSelectedItem(default_mstv_dist);
        cbo_vtp[sum][VAR_TRAF_PER_VOL].setSelectedItem(Integer.toString(default_msia_vol));
        resetDistParameterList(sum);
        cbo_vtp[sum][VAR_TRAF_PER_PAR].setSelectedItem(twoDigits.format(default_mdfv_par));
        cbo_vtp[sum][VAR_TRAF_PER_MEAN].setSelectedItem(oneDigits.format(default_mdfv_mean));
        cbo_vtp[sum][VAR_TRAF_PER_85TH].setSelectedItem(oneDigits.format(default_mdfv_85th));
        cbo_vtp[sum][VAR_TRAF_PER_DEST_PER_1].setSelectedItem(Integer.toString(default_msia_dest_per[1]));
        cbo_vtp[sum][VAR_TRAF_PER_DEST_PER_2].setSelectedItem(Integer.toString(default_msia_dest_per[2]));
        cbo_vtp[sum][VAR_TRAF_PER_DEST_PER_3].setSelectedItem(Integer.toString(default_msia_dest_per[3]));
        cbo_vtp[sum][VAR_TRAF_PER_DEST_PER_4].setSelectedItem(Integer.toString(default_msia_dest_per[4]));
        cbo_vtp[sum][VAR_TRAF_PER_DEST_PER_5].setSelectedItem(Integer.toString(default_msia_dest_per[5]));
        cbo_vtp[sum][VAR_TRAF_PER_DEST_PER_6].setSelectedItem(Integer.toString(default_msia_dest_per[6]));
        cbo_vtp[sum][VAR_TRAF_PER_TM].setSelectedItem(default_mstv_tm);

        for (int lsiv_traf = 1; lsiv_traf <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_traf++) {
            gdvsim.mcla_var_traf_period_stat[leg_number][sum][lsiv_traf] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            gdvsim.mcla_var_traf_period_value[leg_number][sum][lsiv_traf] = default_mcla_var_traf_period_value[leg_number][sum][lsiv_traf];
        }

    } // end of setValueAfterDel()

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
                else if (event.getSource() == cbo_total_vtp) {
                    new HelpDialog(true, label_total_vtp.getText(), label_total_vtp.getText(), "The user cannot specify this item.  This item is the " + label_total_vtp.getText()
                            + " and is determined by the program.", cbo_total_vtp.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NVT), "1");

                }
                else if (event.getSource() == cbo_total_pt) {
                    new HelpDialog(true, label_total_pt.getText(), label_total_pt.getText(), "The user cannot specify this item.  This item is the " + label_total_pt.getText()
                            + " and is determined by the program.", cbo_total_pt.getSelectedItem().toString(), "0", " ", "0",
                            Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time), "1");
                }

                for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
                    if (event.getSource() == sum_pct[lsiv_vtp]) {
                        new HelpDialog(true, label_field_sum_pct.getText(), label_field_sum_pct.getText(), "The user cannot specify this item.  This item is the " + label_field_sum_pct.getText()
                                + " and is determined by the program.", sum_pct[lsiv_vtp].getSelectedItem().toString(), "0", " ", "0", "100", "1");
                    }
                    else if (event.getSource() == sum_dest[lsiv_vtp]) {
                        new HelpDialog(true, label_field_sum_dest.getText(), label_field_sum_dest.getText(), "The user cannot specify this item.  This item is the " + label_field_sum_dest.getText()
                                + " and is determined by the program.", sum_dest[lsiv_vtp].getSelectedItem().toString(), "0", " ", "0", "100", "1");
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst(
                                "#", Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR].getSelectedItem().toString(),
                                Integer.toString(default_msia_period_duration), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_LT]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#",
                                Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_LT].getSelectedItem().toString(),
                                Integer.toString(default_msia_pct_lt), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_RT]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#",
                                Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_RT].getSelectedItem().toString(),
                                Integer.toString(default_msia_pct_rt), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PER_US_FUT]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst(
                                "#", Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PER_US_FUT].getSelectedItem().toString(),
                                Integer.toString(default_msia_percent_using), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#",
                                Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString(),
                                Integer.toString(default_msia_pct[1]), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#",
                                Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getSelectedItem().toString(),
                                Integer.toString(default_msia_pct[2]), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#",
                                Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].getSelectedItem().toString(),
                                Integer.toString(default_msia_pct[3]), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#",
                                Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].getSelectedItem().toString(),
                                Integer.toString(default_msia_pct[4]), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#",
                                Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].getSelectedItem().toString(),
                                Integer.toString(default_msia_pct[5]), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#",
                                Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].getSelectedItem().toString(),
                                Integer.toString(default_msia_pct[6]), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DIST]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DIST], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#",
                                Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DIST],
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DIST], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DIST].getSelectedItem().toString(), default_mstv_dist,
                                lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DIST], " ", " ", " ");
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_VOL]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#",
                                Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_VOL].getSelectedItem().toString(),
                                Integer.toString(default_msia_vol), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];
                        String name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#", Integer.toString(leg_number));

                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

                        // check and set headway distribution gdvsim.gclv_inter..mbov_is_hd_* values
                        gdvsim.gclv_inter.check_and_set_headway_distribution(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DIST].getSelectedItem().toString());
                        if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS)
                            return;

                        if (gdvsim.gclv_inter.mbov_is_hd_CONSTAN) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN], name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN],
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].getSelectedItem().toString(),
                                    twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]), " ",
                                    twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]));
                        }
                        else if (gdvsim.gclv_inter.mbov_is_hd_ERLANG) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG], name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG],
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].getSelectedItem().toString(),
                                    twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]), " ",
                                    twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]));
                        }
                        else if (gdvsim.gclv_inter.mbov_is_hd_GAMMA) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA], name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA],
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].getSelectedItem().toString(),
                                    twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]), " ",
                                    twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]));
                        }
                        else if (gdvsim.gclv_inter.mbov_is_hd_LOGNRML) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML], name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML],
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].getSelectedItem().toString(),
                                    twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]), " ",
                                    twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]));
                        }
                        else if (gdvsim.gclv_inter.mbov_is_hd_NEGEXP) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP], name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP],
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].getSelectedItem().toString(),
                                    twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]), " ",
                                    twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]));
                        }
                        else if (gdvsim.gclv_inter.mbov_is_hd_SNEGEXP) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP], name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP],
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].getSelectedItem().toString(),
                                    twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]), " ",
                                    twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]));
                        }
                        else if (gdvsim.gclv_inter.mbov_is_hd_UNIFORM) {
                            new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM], name, lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM],
                                    lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].getSelectedItem().toString(),
                                    twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]), " ",
                                    twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]),
                                    twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]));
                        }
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_MEAN]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#",
                                Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_MEAN].getSelectedItem().toString(),
                                oneDigits.format(default_mdfv_mean), " ", oneDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN]),
                                oneDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN]),
                                oneDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_85TH]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#",
                                Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_85TH].getSelectedItem().toString(),
                                oneDigits.format(default_mdfv_85th), " ", oneDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH]),
                                oneDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH]),
                                oneDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst(
                                "#", Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].getSelectedItem().toString(),
                                Integer.toString(default_msia_dest_per[1]), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst(
                                "#", Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].getSelectedItem().toString(),
                                Integer.toString(default_msia_dest_per[2]), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst(
                                "#", Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].getSelectedItem().toString(),
                                Integer.toString(default_msia_dest_per[3]), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst(
                                "#", Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].getSelectedItem().toString(),
                                Integer.toString(default_msia_dest_per[4]), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst(
                                "#", Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].getSelectedItem().toString(),
                                Integer.toString(default_msia_dest_per[5]), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst(
                                "#", Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6] + " for varying traffic period " + lsiv_vtp,
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].getSelectedItem().toString(),
                                Integer.toString(default_msia_dest_per[6]), " ", Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6]),
                                Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6]),
                                Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6]));
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM]) {
                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER];

                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM], lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(lsiv_vtp)).replaceFirst("#",
                                Integer.toString(leg_number)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM],
                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM], cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM].getSelectedItem().toString(), default_mstv_tm,
                                lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM], " ", " ", " ");
                    }
                    else if (event.getSource() == edit[lsiv_vtp]) {
                        new HelpDialog(true, "Edit Button", "The Edit button edits the traffic mix data for the varying traffic period" + lsiv_vtp + ".", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == up[lsiv_vtp]) {
                        new HelpDialog(true, "Up Button", "The Up button swaps the data for the previous varying traffic period and the current varying traffic period.", " ", " ", " ", " ", " ", " ",
                                " ");
                    }
                    else if (event.getSource() == down[lsiv_vtp]) {
                        new HelpDialog(true, "Down Button", "The Down button swaps the data for the next varying traffic period and the current varying traffic period.", " ", " ", " ", " ", " ", " ",
                                " ");
                    }
                    else if (event.getSource() == del[lsiv_vtp]) {
                        new HelpDialog(
                                true,
                                "Delete Button",
                                "The Delete button moves the data up 1 varying traffic period for all VMS messages below this varying traffic period and decreases the Total Varying Traffic Periods by 1.",
                                " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == add[lsiv_vtp]) {
                        new HelpDialog(
                                true,
                                "Add Button",
                                "The Add button moves the data down 1 varying traffic period for all VMS messages below this varying traffic period, inserts a new varying traffic period at the current position, copies the values of all parameters from the previous varying traffic period to the new varying traffic period, and increases the Total VMS Messages by 1.",
                                " ", " ", " ", " ", " ", " ", " ");
                    }
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_F1)
        }
    } // end of HelpListener

    void saveData() {
        numOfVTPs = getSumOfVTP();

        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods = numOfVTPs;
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_LEG_GEO_TRAFPER] = gdvsim.gclv_inter.TX_FROM_USER;

        if (numOfVTPs > 0) {
            gdvsim.gclv_inter.mboa_Leg_Varying_Traffic_Period_OK[leg_number] = true;

            for (int lsiv_vtp = 1; lsiv_vtp <= numOfVTPs; lsiv_vtp++) {
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_period_duration = Integer.valueOf(
                        cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_pct_lt = Integer.valueOf(
                        cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_LT].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_pct_rt = Integer.valueOf(
                        cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_RT].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_percent_using = Integer.valueOf(
                        cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PER_US_FUT].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct[1] = Integer
                        .valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct[2] = Integer
                        .valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct[3] = Integer
                        .valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct[4] = Integer
                        .valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct[5] = Integer
                        .valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_pct[6] = Integer
                        .valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_dist = cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DIST].getSelectedItem().toString();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msiv_vol = Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_VOL].getSelectedItem().toString())
                        .intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mdfv_par = Double.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].getSelectedItem().toString())
                        .doubleValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mdfv_mean = Double.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_MEAN].getSelectedItem().toString())
                        .doubleValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mdfv_85th = Double.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_85TH].getSelectedItem().toString())
                        .doubleValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per[1] = Integer.valueOf(
                        cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per[2] = Integer.valueOf(
                        cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per[3] = Integer.valueOf(
                        cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per[4] = Integer.valueOf(
                        cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per[5] = Integer.valueOf(
                        cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].msia_dest_per[6] = Integer.valueOf(
                        cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].getSelectedItem().toString()).intValue();
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_tm = cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM].getSelectedItem().toString();

                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DIST] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PAR] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM] = gdvsim.gclv_inter.TX_FROM_USER;
            }

            for (int lsiv_vtp = numOfVTPs + 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PERIOD_DUR] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_LT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_RT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PER_US_FUT] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_1] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_2] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_3] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_4] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_5] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PCT_6] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DIST] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_VOL] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_PAR] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_MEAN] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_85TH] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_1] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_2] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_3] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_4] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_5] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_DEST_PER_6] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        if (gdvsim.gclv_inter.mbov_is_diamond_interchange) {
            if (leg_number == 3) {
                if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_PERCENT_USING] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                        && (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width != 0)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_percent_using = Integer.valueOf(
                            cbo_vtp[1][VAR_TRAF_PER_PER_US_FUT].getSelectedItem().toString()).intValue();
                }
            }

            if (leg_number == 6) {
                if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_FREE_UTURN_PERCENT_USING] != gdvsim.gclv_inter.TX_DATA_IS_INVALID)
                        && (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width != 0)) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gdvsim.gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_percent_using = Integer.valueOf(
                            cbo_vtp[1][VAR_TRAF_PER_PER_US_FUT].getSelectedItem().toString()).intValue();
                }
            }
        }

        gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = Integer.valueOf(cbo_vtp[1][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = Integer.valueOf(cbo_vtp[1][VAR_TRAF_PER_PCT_2].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = Integer.valueOf(cbo_vtp[1][VAR_TRAF_PER_PCT_3].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = Integer.valueOf(cbo_vtp[1][VAR_TRAF_PER_PCT_4].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = Integer.valueOf(cbo_vtp[1][VAR_TRAF_PER_PCT_5].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mcla_inb_lane[6].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct = Integer.valueOf(cbo_vtp[1][VAR_TRAF_PER_PCT_6].getSelectedItem().toString()).intValue();

        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mstv_dist = cbo_vtp[1][VAR_TRAF_PER_DIST].getSelectedItem().toString();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.msiv_vol = Integer.valueOf(cbo_vtp[1][VAR_TRAF_PER_VOL].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_par = Double.valueOf(cbo_vtp[1][VAR_TRAF_PER_PAR].getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_mean = Double.valueOf(cbo_vtp[1][VAR_TRAF_PER_MEAN].getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mdfv_85th = Double.valueOf(cbo_vtp[1][VAR_TRAF_PER_85TH].getSelectedItem().toString()).doubleValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1] = Integer.valueOf(cbo_vtp[1][VAR_TRAF_PER_DEST_PER_1].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2] = Integer.valueOf(cbo_vtp[1][VAR_TRAF_PER_DEST_PER_2].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3] = Integer.valueOf(cbo_vtp[1][VAR_TRAF_PER_DEST_PER_3].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4] = Integer.valueOf(cbo_vtp[1][VAR_TRAF_PER_DEST_PER_4].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5] = Integer.valueOf(cbo_vtp[1][VAR_TRAF_PER_DEST_PER_5].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6] = Integer.valueOf(cbo_vtp[1][VAR_TRAF_PER_DEST_PER_6].getSelectedItem().toString()).intValue();
        gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mclv_hdway.mstv_tm = cbo_vtp[1][VAR_TRAF_PER_TM].getSelectedItem().toString();

        for (int lsiv_vtp = 1; lsiv_vtp <= numOfVTPs; lsiv_vtp++) {
            for (int lsiv_traf = 1; lsiv_traf <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_traf++) {
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_VAR_TRAF_PER_TM_PER_01 - 1
                        + lsiv_traf] = gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp][lsiv_traf];
                gdvsim.gclv_inter.mcla_leg[leg_number].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_mix.mdfa_per_veh_class[lsiv_traf] = gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp][lsiv_traf];
            }
        }

        gdvsim.gclv_inter.calculate_graphics_and_paint();

    } // end of saveData()

    boolean isError() {
        int total_msiv_period_duration = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;
        int current_msiv_period_duration = 0;

        numOfVTPs = getSumOfVTP();

        for (int lsiv_vtp = 1; lsiv_vtp <= numOfVTPs; lsiv_vtp++) {
            current_msiv_period_duration = current_msiv_period_duration + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR].getSelectedItem().toString()).intValue();
        }

        if (current_msiv_period_duration != total_msiv_period_duration) {
            JOptionPane.showMessageDialog(null, "Total Period Duration = " + current_msiv_period_duration + " minutes should be equal to " + total_msiv_period_duration + " minutes.", "Error Message",
                    JOptionPane.ERROR_MESSAGE);
            return true;
        }

        for (int lsiv_vtp = 1; lsiv_vtp <= numOfVTPs; lsiv_vtp++) {
            int sum_percent = 0;
            if (number_of_inbounds == 1) {
                sum_percent = sum_percent + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue();

                if (sum_percent != 100) {
                    JOptionPane.showMessageDialog(null, "Sum percent = " + sum_percent + " should be equal to 100 for varying traffic period " + lsiv_vtp, "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
            else if (number_of_inbounds == 2) {
                sum_percent = sum_percent + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getSelectedItem().toString()).intValue();

                if (sum_percent != 100) {
                    JOptionPane.showMessageDialog(null, "Sum percent = " + sum_percent + " should be equal to 100 for varying traffic period " + lsiv_vtp, "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
            else if (number_of_inbounds == 3) {
                sum_percent = sum_percent + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].getSelectedItem().toString()).intValue();

                if (sum_percent != 100) {
                    JOptionPane.showMessageDialog(null, "Sum percent = " + sum_percent + " should be equal to 100 for varying traffic period " + lsiv_vtp, "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
            else if (number_of_inbounds == 4) {
                sum_percent = sum_percent + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].getSelectedItem().toString()).intValue();

                if (sum_percent != 100) {
                    JOptionPane.showMessageDialog(null, "Sum percent = " + sum_percent + " should be equal to 100 for varying traffic period " + lsiv_vtp, "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
            else if (number_of_inbounds == 5) {
                sum_percent = sum_percent + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].getSelectedItem().toString()).intValue();

                if (sum_percent != 100) {
                    JOptionPane.showMessageDialog(null, "Sum percent = " + sum_percent + " should be equal to 100 for varying traffic period " + lsiv_vtp, "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
            else if (number_of_inbounds == 6) {
                sum_percent = sum_percent + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5].getSelectedItem().toString()).intValue()
                        + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6].getSelectedItem().toString()).intValue();

                if (sum_percent != 100) {
                    JOptionPane.showMessageDialog(null, "Sum percent = " + sum_percent + " should be equal to 100 for varying traffic period " + lsiv_vtp, "Error Message", JOptionPane.ERROR_MESSAGE);
                    return true;
                }
            }
        }

        for (int lsiv_vtp = 1; lsiv_vtp <= numOfVTPs; lsiv_vtp++) {
            int sum_destination = 0;

            sum_destination = sum_destination + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5].getSelectedItem().toString()).intValue()
                    + Integer.valueOf(cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6].getSelectedItem().toString()).intValue();

            if (sum_destination != 100) {
                JOptionPane.showMessageDialog(null, "Sum of Destination Percentages = " + sum_destination + " should be equal to 100 for Varying Traffic Period " + lsiv_vtp, "Error Message",
                        JOptionPane.ERROR_MESSAGE);
                return true;
            }
        }
        return false;
    } // end of isError

    void DownButtonAction(int lsiv_vtp) {
        removeAllAction();

        for (int lsiv_vtp_1 = 1; lsiv_vtp_1 <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp_1++) {
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_PCT_1].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_PCT_2].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_PCT_3].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_PCT_4].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_PCT_5].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_PCT_6].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_DEST_PER_1].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_DEST_PER_2].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_DEST_PER_3].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_DEST_PER_4].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_DEST_PER_5].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_DEST_PER_6].setVisible(true);
        }

        String DistParam1 = cbo_vtp[lsiv_vtp + 1][VAR_TRAF_PER_PAR].getSelectedItem().toString();
        String DistParam2 = cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].getSelectedItem().toString();

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == VAR_TRAF_PER_PAR) {
                resetDistParameterList(lsiv_vtp + 1);
                resetDistParameterList(lsiv_vtp);
                cbo_vtp[lsiv_vtp + 1][lsiv_field].setSelectedItem(DistParam2);
                cbo_vtp[lsiv_vtp][lsiv_field].setSelectedItem(DistParam1);
            }
            else {
                String temp = cbo_vtp[lsiv_vtp][lsiv_field].getSelectedItem().toString();
                cbo_vtp[lsiv_vtp][lsiv_field].setSelectedItem(cbo_vtp[lsiv_vtp + 1][lsiv_field].getSelectedItem().toString());
                cbo_vtp[lsiv_vtp + 1][lsiv_field].setSelectedItem(temp);
            }
        }

        for (int lsiv_traf = 1; lsiv_traf <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_traf++) {
            int intTemp;
            double doubleTemp;

            intTemp = gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp][lsiv_traf];
            gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp][lsiv_traf] = gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp + 1][lsiv_traf];
            gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp + 1][lsiv_traf] = intTemp;

            doubleTemp = gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp][lsiv_traf];
            gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp][lsiv_traf] = gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp + 1][lsiv_traf];
            gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp + 1][lsiv_traf] = doubleTemp;
        }

        numOfVTPs = getSumOfVTP();
        setStatus(numOfVTPs);

        cbo_total_pt.removeAllItems();
        cbo_total_pt.addItem(Integer.toString(getSumOfPeriodTime()));

        for (int lsiv_vtp_1 = 1; lsiv_vtp_1 <= numOfVTPs; lsiv_vtp_1++) {
            sum_pct[lsiv_vtp_1].removeAllItems();
            sum_pct[lsiv_vtp_1].addItem(Integer.toString(getSumOfPCT(lsiv_vtp_1)));
            sum_dest[lsiv_vtp_1].removeAllItems();
            sum_dest[lsiv_vtp_1].addItem(Integer.toString(getSumOfDest(lsiv_vtp_1)));
        }

        addAllAction();

    } // end of method DownButtonAction

    void UpButtonAction(int lsiv_vtp) {
        removeAllAction();

        for (int lsiv_vtp_1 = 1; lsiv_vtp_1 <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp_1++) {
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_PCT_1].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_PCT_2].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_PCT_3].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_PCT_4].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_PCT_5].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_PCT_6].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_DEST_PER_1].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_DEST_PER_2].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_DEST_PER_3].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_DEST_PER_4].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_DEST_PER_5].setVisible(true);
            cbo_vtp[lsiv_vtp_1][VAR_TRAF_PER_DEST_PER_6].setVisible(true);
        }

        String DistParam1 = cbo_vtp[lsiv_vtp - 1][VAR_TRAF_PER_PAR].getSelectedItem().toString();
        String DistParam2 = cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PAR].getSelectedItem().toString();

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == VAR_TRAF_PER_PAR) {
                resetDistParameterList(lsiv_vtp - 1);
                resetDistParameterList(lsiv_vtp);

                cbo_vtp[lsiv_vtp - 1][lsiv_field].setSelectedItem(DistParam2);
                cbo_vtp[lsiv_vtp][lsiv_field].setSelectedItem(DistParam1);
            }
            else {
                String temp = cbo_vtp[lsiv_vtp][lsiv_field].getSelectedItem().toString();
                cbo_vtp[lsiv_vtp][lsiv_field].setSelectedItem(cbo_vtp[lsiv_vtp - 1][lsiv_field].getSelectedItem().toString());
                cbo_vtp[lsiv_vtp - 1][lsiv_field].setSelectedItem(temp);
            }
        }

        for (int lsiv_traf = 1; lsiv_traf <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_traf++) {
            int intTemp;
            double doubleTemp;

            intTemp = gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp][lsiv_traf];
            gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp][lsiv_traf] = gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp - 1][lsiv_traf];
            gdvsim.mcla_var_traf_period_stat[leg_number][lsiv_vtp - 1][lsiv_traf] = intTemp;

            doubleTemp = gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp][lsiv_traf];
            gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp][lsiv_traf] = gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp - 1][lsiv_traf];
            gdvsim.mcla_var_traf_period_value[leg_number][lsiv_vtp - 1][lsiv_traf] = doubleTemp;
        }

        numOfVTPs = getSumOfVTP();
        setStatus(numOfVTPs);

        cbo_total_pt.removeAllItems();
        cbo_total_pt.addItem(Integer.toString(getSumOfPeriodTime()));

        for (int lsiv_vtp_1 = 1; lsiv_vtp_1 <= numOfVTPs; lsiv_vtp_1++) {
            sum_pct[lsiv_vtp_1].removeAllItems();
            sum_pct[lsiv_vtp_1].addItem(Integer.toString(getSumOfPCT(lsiv_vtp_1)));
            sum_dest[lsiv_vtp_1].removeAllItems();
            sum_dest[lsiv_vtp_1].addItem(Integer.toString(getSumOfDest(lsiv_vtp_1)));
        }

        addAllAction();

    } // end of method UpButtonAction

    void DeleteButtonAction(int lsiv_vtp) {
        removeAllAction();

        int numOfVTPsBeforeClick;
        numOfVTPsBeforeClick = getSumOfVTP();
        setValueAfterDel(numOfVTPsBeforeClick, lsiv_vtp);
        setStatus(numOfVTPsBeforeClick - 1);

        cbo_total_vtp.removeAllItems();
        cbo_total_vtp.addItem(Integer.toString(getSumOfVTP()));
        cbo_total_pt.removeAllItems();
        cbo_total_pt.addItem(Integer.toString(getSumOfPeriodTime()));

        for (int lsiv_vtp_1 = 1; lsiv_vtp_1 <= (numOfVTPsBeforeClick - 1); lsiv_vtp_1++) {
            sum_pct[lsiv_vtp_1].removeAllItems();
            sum_pct[lsiv_vtp_1].addItem(Integer.toString(getSumOfPCT(lsiv_vtp_1)));
            sum_dest[lsiv_vtp_1].removeAllItems();
            sum_dest[lsiv_vtp_1].addItem(Integer.toString(getSumOfDest(lsiv_vtp_1)));
        }

        if (!del[lsiv_vtp].isEnabled()) {
            okButton.requestFocus();
        }

        addAllAction();

    } // end of method DeleteButtonAction()

    void AddButtonAction(int lsiv_vtp) {
        removeAllAction();

        int numOfVTPsBeforeClick;
        numOfVTPsBeforeClick = getSumOfVTP();
        setStatus(numOfVTPsBeforeClick + 1);
        setValueAfterAdd(numOfVTPsBeforeClick, lsiv_vtp);
        setStatus(numOfVTPsBeforeClick + 1);

        cbo_total_vtp.removeAllItems();
        cbo_total_vtp.addItem(Integer.toString(getSumOfVTP()));
        cbo_total_pt.removeAllItems();
        cbo_total_pt.addItem(Integer.toString(getSumOfPeriodTime()));

        for (int lsiv_vtp_1 = 1; lsiv_vtp_1 <= (numOfVTPsBeforeClick + 1); lsiv_vtp_1++) {
            sum_pct[lsiv_vtp_1].removeAllItems();
            sum_pct[lsiv_vtp_1].addItem(Integer.toString(getSumOfPCT(lsiv_vtp_1)));
            sum_dest[lsiv_vtp_1].removeAllItems();
            sum_dest[lsiv_vtp_1].addItem(Integer.toString(getSumOfDest(lsiv_vtp_1)));
        }

        addAllAction();

    } // end of method AddButtonAction

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }
            else if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }

            for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
                if (event.getSource() == add[lsiv_vtp]) {
                    AddButtonAction(lsiv_vtp);
                    break;
                }
                else if (event.getSource() == del[lsiv_vtp]) {
                    DeleteButtonAction(lsiv_vtp);
                    break;
                }
                else if (event.getSource() == down[lsiv_vtp]) {
                    DownButtonAction(lsiv_vtp);
                    break;
                }

                if (lsiv_vtp != 1) {
                    if (event.getSource() == up[lsiv_vtp]) {
                        UpButtonAction(lsiv_vtp);
                        break;
                    }
                }
            }

            for (int lsiv_vtp = 1; lsiv_vtp <= getSumOfVTP(); lsiv_vtp++) {
                if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DIST]) {
                    resetDistParameterList(lsiv_vtp);
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1]) {
                    sum_pct[lsiv_vtp].removeAllItems();
                    sum_pct[lsiv_vtp].addItem(Integer.toString(getSumOfPCT(lsiv_vtp)));
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2]) {
                    sum_pct[lsiv_vtp].removeAllItems();
                    sum_pct[lsiv_vtp].addItem(Integer.toString(getSumOfPCT(lsiv_vtp)));
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3]) {
                    sum_pct[lsiv_vtp].removeAllItems();
                    sum_pct[lsiv_vtp].addItem(Integer.toString(getSumOfPCT(lsiv_vtp)));
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4]) {
                    sum_pct[lsiv_vtp].removeAllItems();
                    sum_pct[lsiv_vtp].addItem(Integer.toString(getSumOfPCT(lsiv_vtp)));
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5]) {
                    sum_pct[lsiv_vtp].removeAllItems();
                    sum_pct[lsiv_vtp].addItem(Integer.toString(getSumOfPCT(lsiv_vtp)));
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6]) {
                    sum_pct[lsiv_vtp].removeAllItems();
                    sum_pct[lsiv_vtp].addItem(Integer.toString(getSumOfPCT(lsiv_vtp)));
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1]) {
                    sum_dest[lsiv_vtp].removeAllItems();
                    sum_dest[lsiv_vtp].addItem(Integer.toString(getSumOfDest(lsiv_vtp)));
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2]) {
                    sum_dest[lsiv_vtp].removeAllItems();
                    sum_dest[lsiv_vtp].addItem(Integer.toString(getSumOfDest(lsiv_vtp)));
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3]) {
                    sum_dest[lsiv_vtp].removeAllItems();
                    sum_dest[lsiv_vtp].addItem(Integer.toString(getSumOfDest(lsiv_vtp)));
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4]) {
                    sum_dest[lsiv_vtp].removeAllItems();
                    sum_dest[lsiv_vtp].addItem(Integer.toString(getSumOfDest(lsiv_vtp)));
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5]) {
                    sum_dest[lsiv_vtp].removeAllItems();
                    sum_dest[lsiv_vtp].addItem(Integer.toString(getSumOfDest(lsiv_vtp)));
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6]) {
                    sum_dest[lsiv_vtp].removeAllItems();
                    sum_dest[lsiv_vtp].addItem(Integer.toString(getSumOfDest(lsiv_vtp)));
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR]) {
                    cbo_total_pt.removeAllItems();
                    cbo_total_pt.addItem(Integer.toString(getSumOfPeriodTime()));
                    break;
                }
                else if (event.getSource() == edit[lsiv_vtp]) {
                    new TrafficMixDialog(leg_number, lsiv_vtp);
                    break;
                }
                else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM]) {
                    if (cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM].getSelectedItem().toString().equals("YES")) {
                        edit[lsiv_vtp].setEnabled(true);
                        new TrafficMixDialog(leg_number, lsiv_vtp);
                    }
                    else {
                        edit[lsiv_vtp].setEnabled(false);
                    }
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
                        saveData();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }
                else if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }

                for (int lsiv_vtp = 1; lsiv_vtp <= PARAMS.TEXAS_MODEL_NVT; lsiv_vtp++) {
                    if (event.getSource() == add[lsiv_vtp]) {
                        AddButtonAction(lsiv_vtp);
                        break;
                    }
                    else if (event.getSource() == del[lsiv_vtp]) {
                        DeleteButtonAction(lsiv_vtp);
                        break;
                    }
                    else if (event.getSource() == down[lsiv_vtp]) {
                        DownButtonAction(lsiv_vtp);
                        break;
                    }

                    if (lsiv_vtp != 1) {
                        if (event.getSource() == up[lsiv_vtp]) {
                            UpButtonAction(lsiv_vtp);
                            break;
                        }
                    }
                }

                for (int lsiv_vtp = 1; lsiv_vtp <= getSumOfVTP(); lsiv_vtp++) {
                    if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DIST]) {
                        resetDistParameterList(lsiv_vtp);
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_1]) {
                        sum_pct[lsiv_vtp].removeAllItems();
                        sum_pct[lsiv_vtp].addItem(Integer.toString(getSumOfPCT(lsiv_vtp)));
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_2]) {
                        sum_pct[lsiv_vtp].removeAllItems();
                        sum_pct[lsiv_vtp].addItem(Integer.toString(getSumOfPCT(lsiv_vtp)));
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_3]) {
                        sum_pct[lsiv_vtp].removeAllItems();
                        sum_pct[lsiv_vtp].addItem(Integer.toString(getSumOfPCT(lsiv_vtp)));
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_4]) {
                        sum_pct[lsiv_vtp].removeAllItems();
                        sum_pct[lsiv_vtp].addItem(Integer.toString(getSumOfPCT(lsiv_vtp)));
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_5]) {
                        sum_pct[lsiv_vtp].removeAllItems();
                        sum_pct[lsiv_vtp].addItem(Integer.toString(getSumOfPCT(lsiv_vtp)));
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PCT_6]) {
                        sum_pct[lsiv_vtp].removeAllItems();
                        sum_pct[lsiv_vtp].addItem(Integer.toString(getSumOfPCT(lsiv_vtp)));
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_1]) {
                        sum_dest[lsiv_vtp].removeAllItems();
                        sum_dest[lsiv_vtp].addItem(Integer.toString(getSumOfDest(lsiv_vtp)));
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_2]) {
                        sum_dest[lsiv_vtp].removeAllItems();
                        sum_dest[lsiv_vtp].addItem(Integer.toString(getSumOfDest(lsiv_vtp)));
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_3]) {
                        sum_dest[lsiv_vtp].removeAllItems();
                        sum_dest[lsiv_vtp].addItem(Integer.toString(getSumOfDest(lsiv_vtp)));
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_4]) {
                        sum_dest[lsiv_vtp].removeAllItems();
                        sum_dest[lsiv_vtp].addItem(Integer.toString(getSumOfDest(lsiv_vtp)));
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_5]) {
                        sum_dest[lsiv_vtp].removeAllItems();
                        sum_dest[lsiv_vtp].addItem(Integer.toString(getSumOfDest(lsiv_vtp)));
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_DEST_PER_6]) {
                        sum_dest[lsiv_vtp].removeAllItems();
                        sum_dest[lsiv_vtp].addItem(Integer.toString(getSumOfDest(lsiv_vtp)));
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_PERIOD_DUR]) {
                        cbo_total_pt.removeAllItems();
                        cbo_total_pt.addItem(Integer.toString(getSumOfPeriodTime()));
                        break;
                    }
                    else if (event.getSource() == edit[lsiv_vtp]) {
                        new TrafficMixDialog(leg_number, lsiv_vtp);
                        break;
                    }
                    else if (event.getSource() == cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM]) {
                        if (cbo_vtp[lsiv_vtp][VAR_TRAF_PER_TM].getSelectedItem().toString().equals("YES")) {
                            edit[lsiv_vtp].setEnabled(true);
                            new TrafficMixDialog(leg_number, lsiv_vtp);
                        }
                        else {
                            edit[lsiv_vtp].setEnabled(false);
                        }
                        break;
                    }
                }
            } // end of if(event.getKeyCode() == KeyEvent.VK_ENTER)
        } // end of method actionPerformed
    } // end of class ComponentKeyListener

} // end of class LegVaryTrafDialog.java
