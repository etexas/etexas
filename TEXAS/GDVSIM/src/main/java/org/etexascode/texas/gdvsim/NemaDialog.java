package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              NemaDialog.java                               */
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
import java.awt.event.MouseEvent;
import javax.accessibility.*;

class NemaDialog extends JDialog {

    static final Dimension SCREEN_SIZE = Toolkit.getDefaultToolkit().getScreenSize();

    int NUMOFFIELD = 28;

    static final int MIN_GRN = 1;

    static final int PAS_TIM = 2;

    static final int MAX_1 = 3;

    static final int MAX_2 = 4;

    static final int MAX_1TO2_TIME = 5;

    static final int YEL_CHG = 6;

    static final int RED_CLR = 7;

    static final int RED_REVERT = 8;

    static final int WALK = 9;

    static final int PED_CLR = 10;

    static final int DUAL_ENT_PH = 11;

    static final int STORE_DEMAND = 12;

    static final int MAX_RECALL = 13;

    static final int MIN_RECALL = 14;

    static final int PED_RECALL = 15;

    static final int PED_RECYCLE = 16;

    static final int CALLATPHTERM = 17;

    static final int COND_SERVICE = 18;

    static final int USE_VOLDENOPT = 19;

    static final int VOLDEN_ADDINI = 20;

    static final int VOLDEN_MAXINI = 21;

    static final int VOLDEN_T2RED = 22;

    static final int VOLDEN_TB4RED = 23;

    static final int VOLDEN_MINGAP = 24;

    static final int PED_MVMT = 25;

    static final int PED_MVMT_DIST = 26;

    static final int PED_MVMT_VOL = 27;

    static final int PED_MVMT_PAR = 28;

    JFrame aFrame;

    Container main_container;

    GridBagLayout layout_mainpanel;

    GridBagLayout layout_mainset01;

    GridBagLayout layout_mainset02;

    GridBagLayout layout_volumeden;

    GridBagLayout layout_pedestrmv;

    GridBagConstraints cnstr_mainpanel;

    GridBagConstraints cnstr_mainset01;

    GridBagConstraints cnstr_mainset02;

    GridBagConstraints cnstr_volumeden;

    GridBagConstraints cnstr_pedestrmv;

    JTabbedPane tabbedPane;

    JPanel panel_mainset01;

    JPanel panel_mainset02;

    JPanel panel_volumeden;

    JPanel panel_pedestrmv;

    JLabel[] label_mainset01 = new JLabel[PARAMS.TEXAS_MODEL_NPN + 1];

    JLabel[] label_mainset02 = new JLabel[PARAMS.TEXAS_MODEL_NPN + 1];

    JLabel[] label_volumeden = new JLabel[PARAMS.TEXAS_MODEL_NPN + 1];

    JLabel[] label_pedestrmv = new JLabel[PARAMS.TEXAS_MODEL_NPN + 1];

    JLabel label_title, label_total;

    JComboBox cbo_total;

    JComboBox cbo_gapout;

    JComboBox[][] comboBox_nema = new JComboBox[PARAMS.TEXAS_MODEL_NPN + 1][NUMOFFIELD + 1];

    JButton okButton, applyButton, cancelButton;

    JTextArea[] label_field = new JTextArea[NUMOFFIELD + 1];

    JButton[] setAllButton = new JButton[NUMOFFIELD + 1];

    JComboBox[] setAllComboBox = new JComboBox[NUMOFFIELD + 1];

    JButton[] add = new JButton[PARAMS.TEXAS_MODEL_NPN + 1];

    JButton[] del = new JButton[PARAMS.TEXAS_MODEL_NPN + 1];

    JButton[] up = new JButton[PARAMS.TEXAS_MODEL_NPN + 1];

    JButton[] down = new JButton[PARAMS.TEXAS_MODEL_NPN + 1];

    JButton[] btngrp = new JButton[4];

    Font font, font1, font2;

    TX_Fmt lclv_tx_fmt;

    String titleString;

    int initial_number_of_phases;

    SetAllActionListener setAllActionListener;

    SetAllKeyListener setAllKeyListener;

    OpenComboMenuListener openComboMenuListener;

    HelpListener helpListener;

    OkApplyActionListener okApplyActionListener;

    OkApplyKeyListener okApplyKeyListener;

    AddActionListener addActionListener;

    DelActionListener delActionListener;

    UpActionListener upActionListener;

    DownActionListener downActionListener;

    AddKeyListener addKeyListener;

    DelKeyListener delKeyListener;

    UpKeyListener upKeyListener;

    DownKeyListener downKeyListener;

    BtngrpActionListener btngrpActionListener;

    BtngrpKeyListener btngrpKeyListener;

    VoldenoptActionListener voldenoptActionListener;

    VoldenoptKeyListener voldenoptKeyListener;

    PedMvmtActionListener pedMvmtActionListener;

    PedMvmtKeyListener pedMvmtKeyListener;

    PedMvmtDistActionListener pedMvmtDistActionListener;

    PedMvmtDistKeyListener pedMvmtDistKeyListener;

    DecimalFormat oneDigits = new DecimalFormat("0.0");

    DecimalFormat twoDigits = new DecimalFormat("0.00");

    String[][][] greenSeqDefault = new String[PARAMS.TEXAS_MODEL_NGI + 1][PARAMS.TEXAS_MODEL_NIA][PARAMS.TEXAS_MODEL_NAL + 1];

    int[][] detConnNemaDefConn = new int[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NPL + 1];

    String[][] detConnNemaDefType = new String[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NPL + 1];

    int[][] movementDefault = new int[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NMP + 1];

    int[] numOfMovementDefault = new int[PARAMS.TEXAS_MODEL_NPN + 1];

    int[][] overlapDefault = new int[PARAMS.TEXAS_MODEL_NON + 1][PARAMS.TEXAS_MODEL_NPN + 1];

    int[][][] ringGroupDefault = new int[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRGP + 1];

    int[][] ringGroupNumOfPhasesDefault = new int[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1];

    int ringGroupNumOfRingDefault;

    int ringGroupNumOfGroupDefault;

    double simStartTime, simSimulationTime;

    public NemaDialog() {
        if (gdvsim.flag_greenSequence_ok) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        String defaultValue = " ";

                        if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0 - 1
                                + lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            String lc = " ";
                            lc = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont;

                            if (lc.equals("BL") || lc.equals("UN") || lc.equals("YI") || lc.equals("ST")) {
                                defaultValue = "UN";
                            }
                        }

                        greenSeqDefault[lsiv_phase][lsiv_leg][lsiv_lane] = defaultValue;
                    }
                }
            }

            for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
                for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        String defaultValue = " ";

                        if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0 - 1
                                + lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            String lc = " ";
                            lc = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont;

                            if (lc.equals("BL") || lc.equals("UN") || lc.equals("YI") || lc.equals("ST")) {
                                defaultValue = "UN";
                            }
                        }

                        greenSeqDefault[PARAMS.TEXAS_MODEL_NPN + lsiv_overlap][lsiv_leg][lsiv_lane] = defaultValue;
                    }
                }
            }

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        gdvsim.greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01
                                - 1 + lsiv_phase];

                        if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01
                                - 1 + lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            gdvsim.greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01
                                    - 1 + lsiv_phase];
                        }
                        else {
                            gdvsim.greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane] = greenSeqDefault[lsiv_phase][lsiv_leg][lsiv_lane];
                        }
                    }
                }
            }

            for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
                for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        gdvsim.greenSeqStat[PARAMS.TEXAS_MODEL_NPN
                                + lsiv_overlap][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A
                                        - 1 + lsiv_overlap];

                        if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A
                                - 1 + lsiv_overlap] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            gdvsim.greenSeqValue[PARAMS.TEXAS_MODEL_NPN
                                    + lsiv_overlap][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A
                                            - 1 + lsiv_overlap];
                        }
                        else {
                            gdvsim.greenSeqValue[PARAMS.TEXAS_MODEL_NPN + lsiv_overlap][lsiv_leg][lsiv_lane] = greenSeqDefault[PARAMS.TEXAS_MODEL_NPN + lsiv_overlap][lsiv_leg][lsiv_lane];
                        }
                    }
                }
            }
        }

        if (gdvsim.flag_detConnForNema_ok) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H];

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                    detConnNemaDefConn[lsiv_phase][lsiv_det] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_DET_01 - 1 + lsiv_det];
                    gdvsim.detConnNemaVStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_DET_01
                            - 1 + lsiv_det];
                    detConnNemaDefType[lsiv_phase][lsiv_det] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01 - 1 + lsiv_det];
                    gdvsim.detConnNemaTStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01
                            - 1 + lsiv_det];
                }
            }

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_DET_01 - 1
                            + lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.detConnNemaValue[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det];
                    }
                    else {
                        gdvsim.detConnNemaValue[lsiv_phase][lsiv_det] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_DET_01 - 1 + lsiv_det];
                    }

                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01 - 1
                            + lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.detConnNemaType[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[lsiv_det];
                    }
                    else {
                        gdvsim.detConnNemaType[lsiv_phase][lsiv_det] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01 - 1 + lsiv_det];
                    }
                }
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT];

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            numOfMovementDefault[lsiv_phase] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1 + lsiv_phase];
            gdvsim.numOfMovementStat[lsiv_phase] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01
                    - 1 + lsiv_phase];
            gdvsim.numOfMovementValue[lsiv_phase] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num[lsiv_phase];

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                movementDefault[lsiv_phase][lsiv_mvt] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP];
                gdvsim.movementStat[lsiv_phase][lsiv_mvt] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1
                        - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP];
                gdvsim.movementValue[lsiv_phase][lsiv_mvt] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list[lsiv_phase][lsiv_mvt];
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_HEADER];

        gdvsim.numOfOverlap = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov;
        gdvsim.numOfOverlapStat = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV];

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF];

        for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                overlapDefault[lsiv_olp][lsiv_phase] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1 + lsiv_phase];
                gdvsim.overlapStat[lsiv_olp][lsiv_phase] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_olp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01
                        - 1 + lsiv_phase];
            }
        }

        for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_olp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1
                        + lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.overlapValue[lsiv_olp][lsiv_phase] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_olp].msia_phase[lsiv_phase];
                }
                else {
                    gdvsim.overlapValue[lsiv_olp][lsiv_phase] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1 + lsiv_phase];
                }
            }
        }

        if (gdvsim.flag_nemaRingGroup_ok) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP];

            for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                        ringGroupDefault[lsiv_ring][lsiv_group][lsiv_phase] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1 + lsiv_phase + (lsiv_group - 1)
                                * PARAMS.TEXAS_MODEL_NRGP + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP];
                        gdvsim.ringGroupStat[lsiv_ring][lsiv_group][lsiv_phase] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01
                                - 1 + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP];
                    }
                }
            }

            for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1
                                + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP
                                + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[lsiv_ring][lsiv_group][lsiv_phase];
                        }
                        else {
                            gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1 + lsiv_phase + (lsiv_group - 1)
                                    * PARAMS.TEXAS_MODEL_NRGP + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP];
                        }
                    }
                }
            }

            gdvsim.ringGroupNumOfRingStat = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS];

            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                ringGroupNumOfRingDefault = 1;
                gdvsim.ringGroupNumOfRingValue = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_rings;
            }
            else {
                ringGroupNumOfRingDefault = 0;
                gdvsim.ringGroupNumOfRingValue = 0;
            }

            gdvsim.ringGroupNumOfGroupStat = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS];

            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                ringGroupNumOfGroupDefault = 1;
                gdvsim.ringGroupNumOfGroupValue = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msiv_num_groups;
            }
            else {
                ringGroupNumOfGroupDefault = 0;
                gdvsim.ringGroupNumOfGroupValue = 0;
            }

            for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                    gdvsim.ringGroupNumOfPhasesStat[lsiv_ring][lsiv_group] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1
                            - 1 + lsiv_group + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG];
                }
            }

            for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1 - 1
                            + lsiv_group + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.ringGroupNumOfPhasesValue[lsiv_ring][lsiv_group] = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[lsiv_ring][lsiv_group];
                        ringGroupNumOfPhasesDefault[lsiv_ring][lsiv_group] = 0;
                    }
                    else {
                        ringGroupNumOfPhasesDefault[lsiv_ring][lsiv_group] = 1;
                        gdvsim.ringGroupNumOfPhasesValue[lsiv_ring][lsiv_group] = 1;
                    }
                }
            }
        } // end of if (gdvsim.flag_nemaRingGroup_ok)

        initial_number_of_phases = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases;

        simStartTime = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_start_time;
        simSimulationTime = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_sim_time;

        if (initial_number_of_phases < 2)
            initial_number_of_phases = 2;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];

        titleString = lclv_tx_fmt.mstv_name.substring(8);

        aFrame = new JFrame(titleString);

        main_container = aFrame.getContentPane();

        JPanel wholePanel = new JPanel();
        main_container = wholePanel;

        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        aFrame.getContentPane().add(scrollpane);

        layout_mainpanel = new GridBagLayout();
        main_container.setLayout(layout_mainpanel);
        cnstr_mainpanel = new GridBagConstraints();

        cnstr_mainpanel.fill = GridBagConstraints.BOTH;

        font = new Font("TimesRoman", Font.BOLD, 18);
        font1 = new Font("TimesRoman", Font.BOLD, 14);
        font2 = new Font("TimesRoman", Font.BOLD, 10);

        label_title = new JLabel(titleString);
        label_total = new JLabel("Total Phases");

        label_title.setFont(font);

        JPanel panel_title = new JPanel();
        panel_title.add(label_title);

        panel_mainset01 = new JPanel();
        panel_mainset02 = new JPanel();
        panel_volumeden = new JPanel();
        panel_pedestrmv = new JPanel();

        tabbedPane = new JTabbedPane();
        tabbedPane.addTab("Main Settings 1", panel_mainset01);
        tabbedPane.addTab("Main Settings 2", panel_mainset02);
        tabbedPane.addTab("Volume Density Settings", panel_volumeden);
        tabbedPane.addTab("Pedestrian Settings", panel_pedestrmv);

        layout_mainset01 = new GridBagLayout();
        layout_mainset02 = new GridBagLayout();
        layout_volumeden = new GridBagLayout();
        layout_pedestrmv = new GridBagLayout();

        cnstr_mainset01 = new GridBagConstraints();
        cnstr_mainset02 = new GridBagConstraints();
        cnstr_volumeden = new GridBagConstraints();
        cnstr_pedestrmv = new GridBagConstraints();

        panel_mainset01.setLayout(layout_mainset01);
        panel_mainset02.setLayout(layout_mainset02);
        panel_volumeden.setLayout(layout_volumeden);
        panel_pedestrmv.setLayout(layout_pedestrmv);

        cnstr_mainset01.fill = GridBagConstraints.BOTH;
        cnstr_mainset02.fill = GridBagConstraints.BOTH;
        cnstr_volumeden.fill = GridBagConstraints.BOTH;
        cnstr_pedestrmv.fill = GridBagConstraints.BOTH;

        double lsiv_min_double;
        double lsiv_max_double;
        double lsiv_inc_double;

        int lsiv_min_int;
        int lsiv_max_int;
        int lsiv_inc_int;

        int arrayIndex, SizeOfArray, intArrayElementValue, seperateIndex;

        int count, index;
        double double_number, doubleArrayElementValue;

        lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN];
        lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN];
        lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN];

        SizeOfArray = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
        intArrayElementValue = lsiv_min_int;
        String[] array_min_grn = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_min_grn[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc_int;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PAS_TIM];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PAS_TIM];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PAS_TIM];

        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        double_number = lsiv_min_double;
        String[] array_pas_tim = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_pas_tim[lsiv_i] = oneDigits.format(double_number);
            double_number += lsiv_inc_double;
        }

        lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1];
        lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1];
        lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1];

        SizeOfArray = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
        intArrayElementValue = lsiv_min_int;
        String[] array_max_1 = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_max_1[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc_int;
        }

        lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_2];
        lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_2];
        lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_2];

        SizeOfArray = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
        intArrayElementValue = lsiv_min_int;
        String[] array_max_2 = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_max_2[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc_int;
        }

        lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME];
        lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME];
        lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME];

        lsiv_max_int = (int)(simStartTime + simSimulationTime);

        SizeOfArray = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
        intArrayElementValue = lsiv_min_int;
        String[] array_max_1to2_time = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_max_1to2_time[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc_int;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_YEL_CHG];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_YEL_CHG];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_YEL_CHG];

        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        double_number = lsiv_min_double;
        String[] array_yel_chg = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_yel_chg[lsiv_i] = oneDigits.format(double_number);
            double_number += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_CLR];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_CLR];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_CLR];

        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        double_number = lsiv_min_double;
        String[] array_red_clr = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_red_clr[lsiv_i] = oneDigits.format(double_number);
            double_number += lsiv_inc_double;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT];

        count = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        double_number = lsiv_min_double;
        String[] array_red_revert = new String[count];
        for (int lsiv_i = 0; lsiv_i < count; lsiv_i++) {
            array_red_revert[lsiv_i] = oneDigits.format(double_number);
            double_number += lsiv_inc_double;
        }

        lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_WALK];
        lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_WALK];
        lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_WALK];

        SizeOfArray = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
        intArrayElementValue = lsiv_min_int;
        String[] array_walk = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_walk[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc_int;
        }

        lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_CLR];
        lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_CLR];
        lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_CLR];

        SizeOfArray = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
        intArrayElementValue = lsiv_min_int;
        String[] array_ped_clr = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_ped_clr[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc_int;
        }

        lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_DUAL_ENT_PH];
        lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_DUAL_ENT_PH];
        lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_DUAL_ENT_PH];

        SizeOfArray = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
        intArrayElementValue = lsiv_min_int;
        String[] array_dual_ent_ph = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_dual_ent_ph[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc_int;
        }

        String[] array_store_demand = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_STORE_DEMAND].substring(1).split("\\|");
        String[] array_max_recall = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_RECALL].substring(1).split("\\|");
        String[] array_min_recall = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_RECALL].substring(1).split("\\|");
        String[] array_ped_recall = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_RECALL].substring(1).split("\\|");
        String[] array_ped_recycle = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_RECYCLE].substring(1).split("\\|");
        String[] array_call_on_maxout = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_CALLATPHTERM].substring(1).split("\\|");
        String[] array_conditional_service = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_COND_SERVICE].substring(1).split("\\|");
        String[] array_use_volden_options = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_USE_VOLDENOPT].substring(1).split("\\|");

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_volden_add_ini = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_volden_add_ini[arrayIndex] = oneDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI];
        lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI];
        lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI];

        SizeOfArray = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
        intArrayElementValue = lsiv_min_int;
        String[] array_volden_max_ini = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_volden_max_ini[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc_int;
        }

        lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED];
        lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED];
        lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED];

        SizeOfArray = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
        intArrayElementValue = lsiv_min_int;
        String[] array_volden_t2r_veh = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_volden_t2r_veh[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc_int;
        }

        lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED];
        lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED];
        lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED];

        SizeOfArray = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
        intArrayElementValue = lsiv_min_int;
        String[] array_volden_tb4r_veh = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_volden_tb4r_veh[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc_int;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_volden_min_gap = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_volden_min_gap[arrayIndex] = oneDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        String[] array_ped_mvmt = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT].substring(1).split("\\|");
        String[] array_ped_mvmt_dist = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST].substring(1).split("\\|");

        lsiv_min_int = lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL];
        lsiv_max_int = lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL];
        lsiv_inc_int = lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL];

        SizeOfArray = (lsiv_max_int - lsiv_min_int) / lsiv_inc_int + 1;
        intArrayElementValue = lsiv_min_int;
        String[] array_ped_mvmt_vol = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_ped_mvmt_vol[arrayIndex] = Integer.toString(intArrayElementValue);
            intArrayElementValue += lsiv_inc_int;
        }

        lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR];
        lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR];
        lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR];

        SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
        doubleArrayElementValue = lsiv_min_double;
        String[] array_ped_mvmt_par = new String[SizeOfArray];
        for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
            array_ped_mvmt_par[arrayIndex] = twoDigits.format(doubleArrayElementValue);
            doubleArrayElementValue += lsiv_inc_double;
        }

        setAllComboBox[1] = new JComboBox(array_min_grn);
        setAllComboBox[2] = new JComboBox(array_pas_tim);
        setAllComboBox[3] = new JComboBox(array_max_1);
        setAllComboBox[4] = new JComboBox(array_max_2);
        setAllComboBox[5] = new JComboBox(array_max_1to2_time);
        setAllComboBox[6] = new JComboBox(array_yel_chg);
        setAllComboBox[7] = new JComboBox(array_red_clr);
        setAllComboBox[8] = new JComboBox(array_red_revert);
        setAllComboBox[9] = new JComboBox(array_walk);
        setAllComboBox[10] = new JComboBox(array_ped_clr);
        setAllComboBox[11] = new JComboBox(array_dual_ent_ph);
        setAllComboBox[12] = new JComboBox(array_store_demand);
        setAllComboBox[13] = new JComboBox(array_max_recall);
        setAllComboBox[14] = new JComboBox(array_min_recall);
        setAllComboBox[15] = new JComboBox(array_ped_recall);
        setAllComboBox[16] = new JComboBox(array_ped_recycle);
        setAllComboBox[17] = new JComboBox(array_call_on_maxout);
        setAllComboBox[18] = new JComboBox(array_conditional_service);
        setAllComboBox[19] = new JComboBox(array_use_volden_options);
        setAllComboBox[20] = new JComboBox(array_volden_add_ini);
        setAllComboBox[21] = new JComboBox(array_volden_max_ini);
        setAllComboBox[22] = new JComboBox(array_volden_t2r_veh);
        setAllComboBox[23] = new JComboBox(array_volden_tb4r_veh);
        setAllComboBox[24] = new JComboBox(array_volden_min_gap);
        setAllComboBox[25] = new JComboBox(array_ped_mvmt);
        setAllComboBox[26] = new JComboBox(array_ped_mvmt_dist);
        setAllComboBox[27] = new JComboBox(array_ped_mvmt_vol);
        setAllComboBox[28] = new JComboBox(array_ped_mvmt_par);

        setAllComboBox[1].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN]));
        setAllComboBox[2].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PAS_TIM]));
        setAllComboBox[3].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1]));
        setAllComboBox[4].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_2]));
        setAllComboBox[5].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME]));
        setAllComboBox[6].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_YEL_CHG]));
        setAllComboBox[7].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_CLR]));
        setAllComboBox[8].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT]));
        setAllComboBox[9].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_WALK]));
        setAllComboBox[10].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_CLR]));
        setAllComboBox[11].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_DUAL_ENT_PH]));
        setAllComboBox[12].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_STORE_DEMAND]);
        setAllComboBox[13].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_RECALL]);
        setAllComboBox[14].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_RECALL]);
        setAllComboBox[15].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_RECALL]);
        setAllComboBox[16].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_RECYCLE]);
        setAllComboBox[17].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_CALLATPHTERM]);
        setAllComboBox[18].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_COND_SERVICE]);
        setAllComboBox[19].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_USE_VOLDENOPT]);
        setAllComboBox[20].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI]));
        setAllComboBox[21].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI]));
        setAllComboBox[22].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED]));
        setAllComboBox[23].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED]));
        setAllComboBox[24].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP]));
        setAllComboBox[25].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT]);
        setAllComboBox[26].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST]);
        setAllComboBox[27].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL]));
        setAllComboBox[28].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR]));

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllButton[lsiv_field] = new JButton("Set All");
            label_field[lsiv_field] = new JTextArea();
            label_field[lsiv_field].setFocusable(false);
            label_field[lsiv_field].setBackground(aFrame.getBackground());
            label_field[lsiv_field].setEditable(false);
            label_field[lsiv_field].setWrapStyleWord(true);
            label_field[lsiv_field].setLineWrap(true);
            label_field[lsiv_field].setFont(font1);
            setAllButton[lsiv_field].setSize(new Dimension(80, 10));
            label_field[lsiv_field].setSize(new Dimension(80, 10));
        }

        label_field[1].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN]);
        label_field[2].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PAS_TIM]);
        label_field[3].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1]);
        label_field[4].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_2]);
        label_field[5].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME]);
        label_field[6].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_YEL_CHG]);
        label_field[7].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_CLR]);
        label_field[8].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT]);
        label_field[9].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_WALK]);
        label_field[10].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_CLR]);
        label_field[11].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_DUAL_ENT_PH]);
        label_field[12].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_STORE_DEMAND]);
        label_field[13].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_RECALL]);
        label_field[14].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_RECALL]);
        label_field[15].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_RECALL]);
        label_field[16].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_RECYCLE]);
        label_field[17].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_CALLATPHTERM]);
        label_field[18].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_COND_SERVICE]);
        label_field[19].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_USE_VOLDENOPT]);
        label_field[20].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI]);
        label_field[21].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI]);
        label_field[22].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED]);
        label_field[23].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED]);
        label_field[24].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP]);
        label_field[25].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT]);
        label_field[26].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST]);
        label_field[27].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL]);
        label_field[28].setText(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR]);

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            comboBox_nema[lsiv_phase][1] = new JComboBox(array_min_grn);
            comboBox_nema[lsiv_phase][2] = new JComboBox(array_pas_tim);
            comboBox_nema[lsiv_phase][3] = new JComboBox(array_max_1);
            comboBox_nema[lsiv_phase][4] = new JComboBox(array_max_2);
            comboBox_nema[lsiv_phase][5] = new JComboBox(array_max_1to2_time);
            comboBox_nema[lsiv_phase][6] = new JComboBox(array_yel_chg);
            comboBox_nema[lsiv_phase][7] = new JComboBox(array_red_clr);
            comboBox_nema[lsiv_phase][8] = new JComboBox(array_red_revert);
            comboBox_nema[lsiv_phase][9] = new JComboBox(array_walk);
            comboBox_nema[lsiv_phase][10] = new JComboBox(array_ped_clr);
            comboBox_nema[lsiv_phase][11] = new JComboBox(array_dual_ent_ph);
            comboBox_nema[lsiv_phase][12] = new JComboBox(array_store_demand);
            comboBox_nema[lsiv_phase][13] = new JComboBox(array_max_recall);
            comboBox_nema[lsiv_phase][14] = new JComboBox(array_min_recall);
            comboBox_nema[lsiv_phase][15] = new JComboBox(array_ped_recall);
            comboBox_nema[lsiv_phase][16] = new JComboBox(array_ped_recycle);
            comboBox_nema[lsiv_phase][17] = new JComboBox(array_call_on_maxout);
            comboBox_nema[lsiv_phase][18] = new JComboBox(array_conditional_service);
            comboBox_nema[lsiv_phase][19] = new JComboBox(array_use_volden_options);
            comboBox_nema[lsiv_phase][20] = new JComboBox(array_volden_add_ini);
            comboBox_nema[lsiv_phase][21] = new JComboBox(array_volden_max_ini);
            comboBox_nema[lsiv_phase][22] = new JComboBox(array_volden_t2r_veh);
            comboBox_nema[lsiv_phase][23] = new JComboBox(array_volden_tb4r_veh);
            comboBox_nema[lsiv_phase][24] = new JComboBox(array_volden_min_gap);
            comboBox_nema[lsiv_phase][25] = new JComboBox(array_ped_mvmt);
            comboBox_nema[lsiv_phase][26] = new JComboBox(array_ped_mvmt_dist);
            comboBox_nema[lsiv_phase][27] = new JComboBox(array_ped_mvmt_vol);
            comboBox_nema[lsiv_phase][28] = new JComboBox(array_ped_mvmt_par);

            comboBox_nema[lsiv_phase][1].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN]));
            comboBox_nema[lsiv_phase][2].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PAS_TIM]));
            comboBox_nema[lsiv_phase][3].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1]));
            comboBox_nema[lsiv_phase][4].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_2]));
            comboBox_nema[lsiv_phase][5].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME]));
            comboBox_nema[lsiv_phase][6].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_YEL_CHG]));
            comboBox_nema[lsiv_phase][7].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_CLR]));
            comboBox_nema[lsiv_phase][8].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT]));
            comboBox_nema[lsiv_phase][9].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_WALK]));
            comboBox_nema[lsiv_phase][10].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_CLR]));
            comboBox_nema[lsiv_phase][11].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_DUAL_ENT_PH]));
            comboBox_nema[lsiv_phase][12].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_STORE_DEMAND]);
            comboBox_nema[lsiv_phase][13].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_RECALL]);
            comboBox_nema[lsiv_phase][14].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_RECALL]);
            comboBox_nema[lsiv_phase][15].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_RECALL]);
            comboBox_nema[lsiv_phase][16].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_RECYCLE]);
            comboBox_nema[lsiv_phase][17].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_CALLATPHTERM]);
            comboBox_nema[lsiv_phase][18].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_COND_SERVICE]);
            comboBox_nema[lsiv_phase][19].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_USE_VOLDENOPT]);
            comboBox_nema[lsiv_phase][20].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI]));
            comboBox_nema[lsiv_phase][21].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI]));
            comboBox_nema[lsiv_phase][22].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED]));
            comboBox_nema[lsiv_phase][23].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED]));
            comboBox_nema[lsiv_phase][24].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP]));
            comboBox_nema[lsiv_phase][25].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT]);
            comboBox_nema[lsiv_phase][26].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST]);
            comboBox_nema[lsiv_phase][27].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL]));
            comboBox_nema[lsiv_phase][28].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR]));
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];

        String[] array_gapout = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT].substring(1).split("\\|");

        cbo_gapout = new JComboBox(array_gapout);
        JLabel label_gapout = new JLabel(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT]);

        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] == gdvsim.gclv_inter.TX_FROM_USER) {
            cbo_gapout.setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_nem2_simul_gapout);
        }
        else {
            cbo_gapout.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT]);
        }

        if (gdvsim.flag_nema_ok) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][1].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_min_grn));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PAS_TIM] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][2].setSelectedItem(Double.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_pas_tim));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][3].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_max_1));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_2] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][4].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_max_2));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][5].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_max_1to2_time));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_YEL_CHG] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][6].setSelectedItem(Double.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_yel_chg));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_CLR] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][7].setSelectedItem(Double.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_red_clr));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][8].setSelectedItem(Double.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_red_revert));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_WALK] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][9].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_walk));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_CLR] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][10].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_ped_clr));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_DUAL_ENT_PH] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][11].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_dual_ent_ph));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_STORE_DEMAND] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][12].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_store_demand);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_RECALL] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][13].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_max_recall);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_RECALL] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][14].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_min_recall);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_RECALL] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][15].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_recall);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_RECYCLE] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][16].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_recycle);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_CALLATPHTERM] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][17].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_call_at_phase_term);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_COND_SERVICE] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][18].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_conditional_service);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_USE_VOLDENOPT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][19].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_use_volden_options);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][20]
                            .setSelectedItem(Double.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_volden_add_ini));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][21].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_volden_max_ini));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][22].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_volden_t2r_veh));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][23].setSelectedItem(Integer
                            .toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_volden_tb4r_veh));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][24]
                            .setSelectedItem(Double.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_volden_min_gap));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][25].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][26].setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt_dist);
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][27].setSelectedItem(Integer.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_ped_mvmt_vol));
                }

                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    comboBox_nema[lsiv_phase][28].setSelectedItem(Double.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_ped_mvmt_par));
                }
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            resetMovementParameter(lsiv_phase);

            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                comboBox_nema[lsiv_phase][28].setSelectedItem(Double.toString(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_ped_mvmt_par));
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            label_mainset01[lsiv_phase] = new JLabel("Phase " + Integer.toString(lsiv_phase));
            label_mainset02[lsiv_phase] = new JLabel("Phase " + Integer.toString(lsiv_phase));
            label_volumeden[lsiv_phase] = new JLabel("Phase " + Integer.toString(lsiv_phase));
            label_pedestrmv[lsiv_phase] = new JLabel("Phase " + Integer.toString(lsiv_phase));

            add[lsiv_phase] = new JButton("Add");
            del[lsiv_phase] = new JButton("Delete");
            up[lsiv_phase] = new JButton("Up");
            down[lsiv_phase] = new JButton("Down");

            add[lsiv_phase].setSize(new Dimension(5, 10));
            del[lsiv_phase].setSize(new Dimension(5, 10));
            up[lsiv_phase].setSize(new Dimension(5, 10));
            down[lsiv_phase].setSize(new Dimension(5, 10));
        }

        cbo_total = new JComboBox();

        setStatus(initial_number_of_phases);

        cbo_total.addItem(Integer.toString(initial_number_of_phases));

        btngrp[1] = new JButton("NEMA Movement Data");
        btngrp[2] = new JButton("NEMA Ring Group Data");
        btngrp[3] = new JButton("NEMA Overlap Data");

        okButton = new JButton("  OK  ");
        applyButton = new JButton(" Apply");
        cancelButton = new JButton("Cancel");

        btngrp[1].setMnemonic(KeyEvent.VK_M);
        btngrp[2].setMnemonic(KeyEvent.VK_R);
        btngrp[3].setMnemonic(KeyEvent.VK_L);
        btngrp[1].setDisplayedMnemonicIndex(5);

        okButton.setMnemonic(KeyEvent.VK_O);
        applyButton.setMnemonic(KeyEvent.VK_A);
        cancelButton.setMnemonic(KeyEvent.VK_C);

        tabbedPane.setDisplayedMnemonicIndexAt(0, 14);
        tabbedPane.setDisplayedMnemonicIndexAt(1, 14);
        tabbedPane.setDisplayedMnemonicIndexAt(2, 0);
        tabbedPane.setDisplayedMnemonicIndexAt(3, 0);

        JPanel ok_panel = new JPanel();
        ok_panel.add(okButton);
        ok_panel.add(applyButton);
        ok_panel.add(cancelButton);

        setAllActionListener = new SetAllActionListener();
        setAllKeyListener = new SetAllKeyListener();
        openComboMenuListener = new OpenComboMenuListener();
        helpListener = new HelpListener();
        okApplyActionListener = new OkApplyActionListener();
        okApplyKeyListener = new OkApplyKeyListener();
        addActionListener = new AddActionListener();
        delActionListener = new DelActionListener();
        upActionListener = new UpActionListener();
        downActionListener = new DownActionListener();
        addKeyListener = new AddKeyListener();
        delKeyListener = new DelKeyListener();
        upKeyListener = new UpKeyListener();
        downKeyListener = new DownKeyListener();
        btngrpActionListener = new BtngrpActionListener();
        btngrpKeyListener = new BtngrpKeyListener();
        voldenoptActionListener = new VoldenoptActionListener();
        voldenoptKeyListener = new VoldenoptKeyListener();
        pedMvmtActionListener = new PedMvmtActionListener();
        pedMvmtKeyListener = new PedMvmtKeyListener();
        pedMvmtDistActionListener = new PedMvmtDistActionListener();
        pedMvmtDistKeyListener = new PedMvmtDistKeyListener();

        for (int lsiv_i = 1; lsiv_i <= 3; lsiv_i++) {
            btngrp[lsiv_i].addActionListener(btngrpActionListener);
            btngrp[lsiv_i].addKeyListener(btngrpKeyListener);
            btngrp[lsiv_i].addKeyListener(helpListener);
        }

        cbo_gapout.addKeyListener(helpListener);
        cbo_gapout.addKeyListener(openComboMenuListener);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllButton[lsiv_field].addActionListener(setAllActionListener);
            setAllButton[lsiv_field].addKeyListener(setAllKeyListener);
            setAllButton[lsiv_field].addKeyListener(helpListener);
        }

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            setAllComboBox[lsiv_field].addKeyListener(openComboMenuListener);
            setAllComboBox[lsiv_field].addKeyListener(helpListener);
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                comboBox_nema[lsiv_phase][lsiv_field].addKeyListener(openComboMenuListener);
                comboBox_nema[lsiv_phase][lsiv_field].addKeyListener(helpListener);
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            comboBox_nema[lsiv_phase][USE_VOLDENOPT].addKeyListener(voldenoptKeyListener);
            comboBox_nema[lsiv_phase][USE_VOLDENOPT].addActionListener(voldenoptActionListener);

            comboBox_nema[lsiv_phase][PED_MVMT].addKeyListener(pedMvmtKeyListener);
            comboBox_nema[lsiv_phase][PED_MVMT].addActionListener(pedMvmtActionListener);

            comboBox_nema[lsiv_phase][PED_MVMT_DIST].addKeyListener(pedMvmtDistKeyListener);
            comboBox_nema[lsiv_phase][PED_MVMT_DIST].addActionListener(pedMvmtDistActionListener);
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            add[lsiv_phase].addActionListener(addActionListener);
            del[lsiv_phase].addActionListener(delActionListener);
            up[lsiv_phase].addActionListener(upActionListener);
            down[lsiv_phase].addActionListener(downActionListener);

            add[lsiv_phase].addKeyListener(addKeyListener);
            del[lsiv_phase].addKeyListener(delKeyListener);
            up[lsiv_phase].addKeyListener(upKeyListener);
            down[lsiv_phase].addKeyListener(downKeyListener);

            add[lsiv_phase].addKeyListener(helpListener);
            del[lsiv_phase].addKeyListener(helpListener);
            up[lsiv_phase].addKeyListener(helpListener);
            down[lsiv_phase].addKeyListener(helpListener);
        }

        cbo_total.addKeyListener(helpListener);
        cbo_total.addKeyListener(openComboMenuListener);

        okButton.addKeyListener(helpListener);
        applyButton.addKeyListener(helpListener);
        cancelButton.addKeyListener(helpListener);

        okButton.addKeyListener(okApplyKeyListener);
        applyButton.addKeyListener(okApplyKeyListener);
        cancelButton.addKeyListener(okApplyKeyListener);

        okButton.addActionListener(okApplyActionListener);
        applyButton.addActionListener(okApplyActionListener);
        cancelButton.addActionListener(okApplyActionListener);

        setAccessibility();

        cnstr_mainpanel.insets = new Insets(1, 1, 1, 1);

        int mainpanel_row = 0;
        int numOfColumns = 6;
        int mainset01_row = 0;
        int mainset02_row = 0;
        int volumeden_row = 0;
        int pedestrmv_row = 0;

        cnstr_mainpanel.insets = new Insets(1, 1, 1, 1);
        cnstr_mainset01.insets = new Insets(1, 2, 2, 1);
        cnstr_mainset02.insets = new Insets(1, 2, 2, 1);
        cnstr_volumeden.insets = new Insets(1, 2, 2, 1);
        cnstr_pedestrmv.insets = new Insets(1, 2, 2, 1);

        addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, btngrp[1], mainset01_row, 1, 3, 1, 0);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            switch (lsiv_field) {
                case MIN_GRN:
                case PAS_TIM:
                case MAX_1:
                case MAX_2:
                case MAX_1TO2_TIME:
                case YEL_CHG:
                case RED_CLR:

                    addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, label_field[lsiv_field], mainset01_row, lsiv_field + 4, 1, 4, 0);
                    break;

                case RED_REVERT:
                case DUAL_ENT_PH:
                case STORE_DEMAND:
                case MAX_RECALL:
                case MIN_RECALL:
                case CALLATPHTERM:
                case COND_SERVICE:

                    addComponent(panel_mainset02, layout_mainset02, cnstr_mainset02, label_field[lsiv_field], mainset02_row, lsiv_field, 1, 4, 0);
                    break;

                case USE_VOLDENOPT:
                case VOLDEN_ADDINI:
                case VOLDEN_MAXINI:
                case VOLDEN_T2RED:
                case VOLDEN_TB4RED:
                case VOLDEN_MINGAP:

                    addComponent(panel_volumeden, layout_volumeden, cnstr_volumeden, label_field[lsiv_field], volumeden_row, lsiv_field, 1, 4, 0);
                    break;

                case WALK:
                case PED_CLR:
                case PED_RECALL:
                case PED_RECYCLE:
                case PED_MVMT:
                case PED_MVMT_DIST:
                case PED_MVMT_VOL:
                case PED_MVMT_PAR:

                    addComponent(panel_pedestrmv, layout_pedestrmv, cnstr_pedestrmv, label_field[lsiv_field], pedestrmv_row, lsiv_field, 1, 4, 0);
                    break;
            }
        }

        mainset01_row++;
        mainset02_row++;
        volumeden_row++;
        pedestrmv_row++;

        addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, btngrp[2], mainset01_row, 1, 3, 1, 0);

        mainset01_row++;
        mainset02_row++;
        volumeden_row++;
        pedestrmv_row++;

        addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, btngrp[3], mainset01_row, 1, 3, 1, 0);

        mainset01_row++;
        mainset02_row++;
        volumeden_row++;
        pedestrmv_row++;

        mainset01_row++;
        mainset02_row++;
        volumeden_row++;
        pedestrmv_row++;

        addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, cbo_gapout, mainset01_row, 1, 1, 1, 0);
        addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, label_gapout, mainset01_row, 2, 3, 1, 0);

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            switch (lsiv_field) {
                case MIN_GRN:
                case PAS_TIM:
                case MAX_1:
                case MAX_2:
                case MAX_1TO2_TIME:
                case YEL_CHG:
                case RED_CLR:

                    addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, setAllComboBox[lsiv_field], mainset01_row, lsiv_field + 4, 1, 1, 0);
                    break;

                case RED_REVERT:
                case DUAL_ENT_PH:
                case STORE_DEMAND:
                case MAX_RECALL:
                case MIN_RECALL:
                case CALLATPHTERM:
                case COND_SERVICE:

                    addComponent(panel_mainset02, layout_mainset02, cnstr_mainset02, setAllComboBox[lsiv_field], mainset02_row, lsiv_field, 1, 1, 0);
                    break;

                case USE_VOLDENOPT:
                case VOLDEN_ADDINI:
                case VOLDEN_MAXINI:
                case VOLDEN_T2RED:
                case VOLDEN_TB4RED:
                case VOLDEN_MINGAP:

                    addComponent(panel_volumeden, layout_volumeden, cnstr_volumeden, setAllComboBox[lsiv_field], volumeden_row, lsiv_field, 1, 1, 0);
                    break;

                case WALK:
                case PED_CLR:
                case PED_RECALL:
                case PED_RECYCLE:
                case PED_MVMT:
                case PED_MVMT_DIST:
                case PED_MVMT_VOL:
                case PED_MVMT_PAR:

                    addComponent(panel_pedestrmv, layout_pedestrmv, cnstr_pedestrmv, setAllComboBox[lsiv_field], pedestrmv_row, lsiv_field, 1, 1, 0);
                    break;
            }
        }

        mainset01_row++;
        mainset02_row++;
        volumeden_row++;
        pedestrmv_row++;

        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            switch (lsiv_field) {
                case MIN_GRN:
                case PAS_TIM:
                case MAX_1:
                case MAX_2:
                case MAX_1TO2_TIME:
                case YEL_CHG:
                case RED_CLR:

                    addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, setAllButton[lsiv_field], mainset01_row, lsiv_field + 4, 1, 1, 0);
                    break;

                case RED_REVERT:
                case DUAL_ENT_PH:
                case STORE_DEMAND:
                case MAX_RECALL:
                case MIN_RECALL:
                case CALLATPHTERM:
                case COND_SERVICE:

                    addComponent(panel_mainset02, layout_mainset02, cnstr_mainset02, setAllButton[lsiv_field], mainset02_row, lsiv_field, 1, 1, 0);
                    break;

                case USE_VOLDENOPT:
                case VOLDEN_ADDINI:
                case VOLDEN_MAXINI:
                case VOLDEN_T2RED:
                case VOLDEN_TB4RED:
                case VOLDEN_MINGAP:

                    addComponent(panel_volumeden, layout_volumeden, cnstr_volumeden, setAllButton[lsiv_field], volumeden_row, lsiv_field, 1, 1, 0);
                    break;

                case WALK:
                case PED_CLR:
                case PED_RECALL:
                case PED_RECYCLE:
                case PED_MVMT:
                case PED_MVMT_DIST:
                case PED_MVMT_VOL:
                case PED_MVMT_PAR:

                    addComponent(panel_pedestrmv, layout_pedestrmv, cnstr_pedestrmv, setAllButton[lsiv_field], pedestrmv_row, lsiv_field, 1, 1, 0);
                    break;
            }
        }

        mainset01_row++;
        mainset02_row++;
        volumeden_row++;
        pedestrmv_row++;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, label_mainset01[lsiv_phase], mainset01_row, 0, 1, 1, 0);
            addComponent(panel_mainset02, layout_mainset02, cnstr_mainset02, label_mainset02[lsiv_phase], mainset02_row, 0, 1, 1, 0);
            addComponent(panel_volumeden, layout_volumeden, cnstr_volumeden, label_volumeden[lsiv_phase], volumeden_row, 0, 1, 1, 0);
            addComponent(panel_pedestrmv, layout_pedestrmv, cnstr_pedestrmv, label_pedestrmv[lsiv_phase], pedestrmv_row, 0, 1, 1, 0);

            addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, add[lsiv_phase], mainset01_row, 1, 1, 1, 0);
            addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, del[lsiv_phase], mainset01_row, 2, 1, 1, 0);
            addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, up[lsiv_phase], mainset01_row, 3, 1, 1, 0);
            addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, down[lsiv_phase], mainset01_row, 4, 1, 1, 0);

            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                switch (lsiv_field) {
                    case MIN_GRN:
                    case PAS_TIM:
                    case MAX_1:
                    case MAX_2:
                    case MAX_1TO2_TIME:
                    case YEL_CHG:
                    case RED_CLR:

                        addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, comboBox_nema[lsiv_phase][lsiv_field], mainset01_row, lsiv_field + 4, 1, 1, 0);
                        break;

                    case RED_REVERT:
                    case DUAL_ENT_PH:
                    case STORE_DEMAND:
                    case MAX_RECALL:
                    case MIN_RECALL:
                    case CALLATPHTERM:
                    case COND_SERVICE:

                        addComponent(panel_mainset02, layout_mainset02, cnstr_mainset02, comboBox_nema[lsiv_phase][lsiv_field], mainset02_row, lsiv_field, 1, 1, 0);
                        break;

                    case USE_VOLDENOPT:
                    case VOLDEN_ADDINI:
                    case VOLDEN_MAXINI:
                    case VOLDEN_T2RED:
                    case VOLDEN_TB4RED:
                    case VOLDEN_MINGAP:

                        addComponent(panel_volumeden, layout_volumeden, cnstr_volumeden, comboBox_nema[lsiv_phase][lsiv_field], volumeden_row, lsiv_field, 1, 1, 0);
                        break;

                    case WALK:
                    case PED_CLR:
                    case PED_RECALL:
                    case PED_RECYCLE:
                    case PED_MVMT:
                    case PED_MVMT_DIST:
                    case PED_MVMT_VOL:
                    case PED_MVMT_PAR:

                        addComponent(panel_pedestrmv, layout_pedestrmv, cnstr_pedestrmv, comboBox_nema[lsiv_phase][lsiv_field], pedestrmv_row, lsiv_field, 1, 1, 0);
                        break;
                }
            }

            mainset01_row++;
            mainset02_row++;
            volumeden_row++;
            pedestrmv_row++;
        }

        addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, cbo_total, mainset01_row, 1, 1, 1, 0);
        addComponent(panel_mainset01, layout_mainset01, cnstr_mainset01, label_total, mainset01_row++, 2, 3, 1, 0);

        addComponent(main_container, layout_mainpanel, cnstr_mainpanel, panel_title, mainpanel_row++, 0, numOfColumns, 1, 0);
        addComponent(main_container, layout_mainpanel, cnstr_mainpanel, tabbedPane, mainpanel_row++, 0, numOfColumns, 1, 0);
        addComponent(main_container, layout_mainpanel, cnstr_mainpanel, ok_panel, mainpanel_row, 0, numOfColumns, 1, 0);

        aFrame.setSize(950, 700);
        aFrame.setVisible(true);

        aFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        // aFrame.setFocusTraversalPolicy(new focusPolicy());
        // aFrame.pack();
        // aFrame.setLocation(SCREEN_SIZE.width/2 - aFrame.getWidth()/2, SCREEN_SIZE.height/2 -
        // aFrame.getHeight()/2);

    } // end of method NemaDialog

    void addComponent(Container container, GridBagLayout gbLayout, GridBagConstraints gbConstraints, Component c, int row, int column, int width, int height, int ipadx) {
        gbConstraints.ipadx = ipadx;

        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        container.add(c);
    } // end of method addComponent

    void addComponent(JPanel JP, GridBagLayout gbLayout, GridBagConstraints gbConstraints, Component c, int row, int column, int width, int height, int ipadx) {
        gbConstraints.ipadx = ipadx;

        gbConstraints.gridx = column;
        gbConstraints.gridy = row;

        gbConstraints.gridwidth = width;
        gbConstraints.gridheight = height;

        gbLayout.setConstraints(c, gbConstraints);
        JP.add(c);
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

    public class focusPolicy extends FocusTraversalPolicy {

        public Component getComponentAfter(Container focusCycleRoot, Component aComponent) {
            return okButton;
        }

        public Component getComponentBefore(Container focusCycleRoot, Component aComponent) {
            return okButton;
        }

        public Component getDefaultComponent(Container focusCycleRoot) {
            return tabbedPane.getComponentAt(1);
        }

        public Component getLastComponent(Container focusCycleRoot) {
            return cancelButton;
        }

        public Component getFirstComponent(Container focusCycleRoot) {
            return tabbedPane.getComponentAt(1);
        }

    } // end of class focusPolicy

    int getNumberOfPhases() {
        int numOfPh = 0;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            if (comboBox_nema[lsiv_phase][1].isEnabled()) {
                numOfPh++;
            }
        }

        return numOfPh;
    }

    void setAccessibility() {
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];

            setAllComboBox[lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field] + " for set all");
            setAllComboBox[lsiv_field].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field] + " for set all");

            setAllButton[lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field] + " for set all");
            setAllButton[lsiv_field].getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field] + " for set all");
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == NUMOFFIELD) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

                    // check and set headway distribution gdvsim.gclv_inter..mbov_is_hd_* values
                    gdvsim.gclv_inter.check_and_set_headway_distribution(comboBox_nema[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString());
                    if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS)
                        return;

                    if (gdvsim.gclv_inter.mbov_is_hd_CONSTAN) {
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM] + " for phase " + lsiv_phase);
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM] + " for phase " + lsiv_phase);
                    }
                    else if (gdvsim.gclv_inter.mbov_is_hd_ERLANG) {
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + " for phase " + lsiv_phase);
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG] + " for phase " + lsiv_phase);
                    }
                    else if (gdvsim.gclv_inter.mbov_is_hd_GAMMA) {
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + " for phase " + lsiv_phase);
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA] + " for phase " + lsiv_phase);
                    }
                    else if (gdvsim.gclv_inter.mbov_is_hd_LOGNRML) {
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + " for phase " + lsiv_phase);
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML] + " for phase " + lsiv_phase);
                    }
                    else if (gdvsim.gclv_inter.mbov_is_hd_NEGEXP) {
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP] + " for phase " + lsiv_phase);
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP] + " for phase " + lsiv_phase);
                    }
                    else if (gdvsim.gclv_inter.mbov_is_hd_SNEGEXP) {
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + " for phase " + lsiv_phase);
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP] + " for phase " + lsiv_phase);
                    }
                    else if (gdvsim.gclv_inter.mbov_is_hd_UNIFORM) {
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + " for phase " + lsiv_phase);
                        comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM] + " for phase " + lsiv_phase);
                    }
                }
                else {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];
                    comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleName(
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field] + " for phase " + lsiv_phase);
                    comboBox_nema[lsiv_phase][lsiv_field].getAccessibleContext().setAccessibleDescription(
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field] + " for phase " + lsiv_phase);
                }
            }

            add[lsiv_phase].getAccessibleContext().setAccessibleName("Add    for phase " + lsiv_phase);
            add[lsiv_phase].getAccessibleContext().setAccessibleDescription("Add    for phase " + lsiv_phase);

            del[lsiv_phase].getAccessibleContext().setAccessibleName("Delete for phase " + lsiv_phase);
            del[lsiv_phase].getAccessibleContext().setAccessibleDescription("Delete for phase " + lsiv_phase);

            up[lsiv_phase].getAccessibleContext().setAccessibleName("Up     for phase " + lsiv_phase);
            up[lsiv_phase].getAccessibleContext().setAccessibleDescription("Up     for phase " + lsiv_phase);

            down[lsiv_phase].getAccessibleContext().setAccessibleName("Down   for phase " + lsiv_phase);
            down[lsiv_phase].getAccessibleContext().setAccessibleDescription("Down   for phase " + lsiv_phase);
        }

        btngrp[1].getAccessibleContext().setAccessibleName("NEMA Movement Data");
        btngrp[1].getAccessibleContext().setAccessibleDescription("NEMA Movement Data");

        btngrp[2].getAccessibleContext().setAccessibleName("NEMA Ring Group Data");
        btngrp[2].getAccessibleContext().setAccessibleDescription("NEMA Ring Group Data");

        btngrp[3].getAccessibleContext().setAccessibleName("NEMA Overlap Data");
        btngrp[3].getAccessibleContext().setAccessibleDescription("NEMA Overlap Data");

        cbo_total.getAccessibleContext().setAccessibleName(label_total.getText());
        cbo_total.getAccessibleContext().setAccessibleDescription(label_total.getText());

        okButton.getAccessibleContext().setAccessibleName("OK");
        okButton.getAccessibleContext().setAccessibleDescription("OK");

        applyButton.getAccessibleContext().setAccessibleName("Apply");
        applyButton.getAccessibleContext().setAccessibleDescription("Apply");

        cancelButton.getAccessibleContext().setAccessibleName("Cancel");
        cancelButton.getAccessibleContext().setAccessibleDescription("Cancel");

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];

        cbo_gapout.getAccessibleContext().setAccessibleName(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]);
        cbo_gapout.getAccessibleContext().setAccessibleDescription(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]);

    } // end of setAccessibility()

    void resetMovementParameter(int lsiv_phase) {
        double lsiv_min_double;
        double lsiv_max_double;
        double lsiv_inc_double;

        int arrayIndex, SizeOfArray, intArrayElementValue, seperateIndex;

        double double_number, doubleArrayElementValue;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

        // check and set headway distribution gdvsim.gclv_inter..mbov_is_hd_* values
        gdvsim.gclv_inter.check_and_set_headway_distribution(comboBox_nema[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString());
        if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS)
            return;

        if (gdvsim.gclv_inter.mbov_is_hd_CONSTAN) {
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_nema[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_ERLANG) {
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_nema[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_GAMMA) {
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_nema[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_LOGNRML) {
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_nema[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_NEGEXP) {
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_nema[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_SNEGEXP) {
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_nema[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]));
        }
        else if (gdvsim.gclv_inter.mbov_is_hd_UNIFORM) {
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].removeAllItems();

            lsiv_min_double = lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];
            lsiv_max_double = lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];
            lsiv_inc_double = lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM];

            SizeOfArray = (int)((lsiv_max_double - lsiv_min_double) / lsiv_inc_double) + 1;
            doubleArrayElementValue = lsiv_min_double;
            String[] array_par = new String[SizeOfArray];
            for (arrayIndex = 0; arrayIndex < SizeOfArray; arrayIndex++) {
                comboBox_nema[lsiv_phase][PED_MVMT_PAR].addItem(twoDigits.format(doubleArrayElementValue));
                doubleArrayElementValue += lsiv_inc_double;
            }
            comboBox_nema[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]));
        }
    } // resetMovementParameter

    class BtngrpActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == btngrp[1]) {
                if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases != getNumberOfPhases())
                        || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] == gdvsim.gclv_inter.TX_DATA_IS_INVALID)) {
                    JOptionPane.showMessageDialog(null, "Please OK or Apply NEMA Signal Controller Timing Data before accessing the NEMA Movement Data.", "Warning Message",
                            JOptionPane.WARNING_MESSAGE);
                }
                else {
                    new NemaMovementDialog();
                }
            }

            if (event.getSource() == btngrp[2]) {
                if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases != getNumberOfPhases())
                        || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] == gdvsim.gclv_inter.TX_DATA_IS_INVALID)) {
                    JOptionPane.showMessageDialog(null, "Please OK or Apply NEMA Signal Controller Timing Data before accessing the NEMA RingGroup Data.", "Warning Message",
                            JOptionPane.WARNING_MESSAGE);
                }
                else {
                    new NemaRingGroupControllerDialog();
                }
            }

            if (event.getSource() == btngrp[3]) {
                if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases != getNumberOfPhases())
                        || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] == gdvsim.gclv_inter.TX_DATA_IS_INVALID)) {
                    JOptionPane
                            .showMessageDialog(null, "Please OK or Apply NEMA Signal Controller Timing Data before accessing the NEMA Overlap Data.", "Warning Message", JOptionPane.WARNING_MESSAGE);
                }
                else {
                    new NemaOverlapDialog();
                }
            }

        }
    } // end of BtngrpActionListener()

    class BtngrpKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == btngrp[1]) {
                    if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases != getNumberOfPhases())
                            || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] == gdvsim.gclv_inter.TX_DATA_IS_INVALID)) {
                        JOptionPane.showMessageDialog(null, "Please OK or Apply NEMA Signal Controller Timing Data before accessing the NEMA Movement Data.", "Warning Message",
                                JOptionPane.WARNING_MESSAGE);
                    }
                    else {
                        new NemaMovementDialog();
                    }
                }

                if (event.getSource() == btngrp[2]) {
                    if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases != getNumberOfPhases())
                            || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] == gdvsim.gclv_inter.TX_DATA_IS_INVALID)) {
                        JOptionPane.showMessageDialog(null, "Please OK or Apply NEMA Signal Controller Timing Data before accessing the NEMA RingGroup Data.", "Warning Message",
                                JOptionPane.WARNING_MESSAGE);
                    }
                    else {
                        new NemaRingGroupControllerDialog();
                    }
                }

                if (event.getSource() == btngrp[3]) {
                    if ((gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases != getNumberOfPhases())
                            || (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] == gdvsim.gclv_inter.TX_DATA_IS_INVALID)) {
                        JOptionPane.showMessageDialog(null, "Please OK or Apply NEMA Signal Controller Timing Data before accessing the NEMA Overlap Data.", "Warning Message",
                                JOptionPane.WARNING_MESSAGE);
                    }
                    else {
                        new NemaOverlapDialog();
                    }
                }
            }
        }
    } // end of BtngrpKeyListener()

    class VoldenoptActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            int sumOfPhase = getNumberOfPhases();
            for (int lsiv_phase = 1; lsiv_phase <= sumOfPhase; lsiv_phase++) {
                if (event.getSource() == comboBox_nema[lsiv_phase][USE_VOLDENOPT]) {
                    if (comboBox_nema[lsiv_phase][USE_VOLDENOPT].getSelectedItem().toString().equals("YES")) {
                        comboBox_nema[lsiv_phase][VOLDEN_ADDINI].setEnabled(true);
                        comboBox_nema[lsiv_phase][VOLDEN_MAXINI].setEnabled(true);
                        comboBox_nema[lsiv_phase][VOLDEN_T2RED].setEnabled(true);
                        comboBox_nema[lsiv_phase][VOLDEN_TB4RED].setEnabled(true);
                        comboBox_nema[lsiv_phase][VOLDEN_MINGAP].setEnabled(true);
                    }
                    else {
                        comboBox_nema[lsiv_phase][VOLDEN_ADDINI].setEnabled(false);
                        comboBox_nema[lsiv_phase][VOLDEN_MAXINI].setEnabled(false);
                        comboBox_nema[lsiv_phase][VOLDEN_T2RED].setEnabled(false);
                        comboBox_nema[lsiv_phase][VOLDEN_TB4RED].setEnabled(false);
                        comboBox_nema[lsiv_phase][VOLDEN_MINGAP].setEnabled(false);

                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];

                        comboBox_nema[lsiv_phase][VOLDEN_ADDINI].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI]));
                        comboBox_nema[lsiv_phase][VOLDEN_MAXINI].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI]));
                        comboBox_nema[lsiv_phase][VOLDEN_T2RED].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED]));
                        comboBox_nema[lsiv_phase][VOLDEN_TB4RED].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED]));
                        comboBox_nema[lsiv_phase][VOLDEN_MINGAP].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP]));
                    }
                }
            }
        }
    } // end of VoldenoptActionListener()

    class VoldenoptKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                int sumOfPhase = getNumberOfPhases();
                for (int lsiv_phase = 1; lsiv_phase <= sumOfPhase; lsiv_phase++) {
                    if (event.getSource() == comboBox_nema[lsiv_phase][USE_VOLDENOPT]) {
                        if (comboBox_nema[lsiv_phase][USE_VOLDENOPT].getSelectedItem().toString().equals("YES")) {
                            comboBox_nema[lsiv_phase][VOLDEN_ADDINI].setEnabled(true);
                            comboBox_nema[lsiv_phase][VOLDEN_MAXINI].setEnabled(true);
                            comboBox_nema[lsiv_phase][VOLDEN_T2RED].setEnabled(true);
                            comboBox_nema[lsiv_phase][VOLDEN_TB4RED].setEnabled(true);
                            comboBox_nema[lsiv_phase][VOLDEN_MINGAP].setEnabled(true);
                        }
                        else {
                            comboBox_nema[lsiv_phase][VOLDEN_ADDINI].setEnabled(false);
                            comboBox_nema[lsiv_phase][VOLDEN_MAXINI].setEnabled(false);
                            comboBox_nema[lsiv_phase][VOLDEN_T2RED].setEnabled(false);
                            comboBox_nema[lsiv_phase][VOLDEN_TB4RED].setEnabled(false);
                            comboBox_nema[lsiv_phase][VOLDEN_MINGAP].setEnabled(false);

                            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];

                            comboBox_nema[lsiv_phase][VOLDEN_ADDINI].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI]));
                            comboBox_nema[lsiv_phase][VOLDEN_MAXINI].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI]));
                            comboBox_nema[lsiv_phase][VOLDEN_T2RED].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED]));
                            comboBox_nema[lsiv_phase][VOLDEN_TB4RED].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED]));
                            comboBox_nema[lsiv_phase][VOLDEN_MINGAP].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP]));
                        }
                    }
                }
            }
        }
    } // end of VoldenoptKeyListener()

    class PedMvmtDistActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            int sumOfPhase = getNumberOfPhases();
            for (int lsiv_phase = 1; lsiv_phase <= sumOfPhase; lsiv_phase++) {
                if (event.getSource() == comboBox_nema[lsiv_phase][PED_MVMT_DIST]) {
                    resetMovementParameter(lsiv_phase);
                }
            }
        }
    } // end of PedMvmtDistActionListener()

    class PedMvmtDistKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                int sumOfPhase = getNumberOfPhases();
                for (int lsiv_phase = 1; lsiv_phase <= sumOfPhase; lsiv_phase++) {
                    if (event.getSource() == comboBox_nema[lsiv_phase][PED_MVMT_DIST]) {
                        resetMovementParameter(lsiv_phase);
                    }
                }
            }
        }
    } // end of PedMvmtDistKeyListener()

    class PedMvmtActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            int sumOfPhase = getNumberOfPhases();
            for (int lsiv_phase = 1; lsiv_phase <= sumOfPhase; lsiv_phase++) {
                if (event.getSource() == comboBox_nema[lsiv_phase][PED_MVMT]) {
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeActionListener(pedMvmtDistActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeKeyListener(pedMvmtDistKeyListener);

                    if (comboBox_nema[lsiv_phase][PED_MVMT].getSelectedItem().toString().equals("YES")) {
                        comboBox_nema[lsiv_phase][PED_MVMT_DIST].setEnabled(true);
                        comboBox_nema[lsiv_phase][PED_MVMT_VOL].setEnabled(true);
                        comboBox_nema[lsiv_phase][PED_MVMT_PAR].setEnabled(true);
                    }
                    else {
                        comboBox_nema[lsiv_phase][PED_MVMT_DIST].setEnabled(false);
                        comboBox_nema[lsiv_phase][PED_MVMT_VOL].setEnabled(false);
                        comboBox_nema[lsiv_phase][PED_MVMT_PAR].setEnabled(false);

                        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];

                        comboBox_nema[lsiv_phase][PED_MVMT_DIST].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST]);
                        comboBox_nema[lsiv_phase][PED_MVMT_VOL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL]));
                        comboBox_nema[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR]));

                        resetMovementParameter(lsiv_phase);
                    }

                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].addActionListener(pedMvmtDistActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].addKeyListener(pedMvmtDistKeyListener);

                }
            }
        }
    } // end of PedMvmtActionListener()

    class PedMvmtKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                int sumOfPhase = getNumberOfPhases();
                for (int lsiv_phase = 1; lsiv_phase <= sumOfPhase; lsiv_phase++) {
                    if (event.getSource() == comboBox_nema[lsiv_phase][PED_MVMT]) {
                        comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeActionListener(pedMvmtDistActionListener);
                        comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeKeyListener(pedMvmtDistKeyListener);

                        if (comboBox_nema[lsiv_phase][PED_MVMT].getSelectedItem().toString().equals("YES")) {
                            comboBox_nema[lsiv_phase][PED_MVMT_DIST].setEnabled(true);
                            comboBox_nema[lsiv_phase][PED_MVMT_VOL].setEnabled(true);
                            comboBox_nema[lsiv_phase][PED_MVMT_PAR].setEnabled(true);
                        }
                        else {
                            comboBox_nema[lsiv_phase][PED_MVMT_DIST].setEnabled(false);
                            comboBox_nema[lsiv_phase][PED_MVMT_VOL].setEnabled(false);
                            comboBox_nema[lsiv_phase][PED_MVMT_PAR].setEnabled(false);

                            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];

                            comboBox_nema[lsiv_phase][PED_MVMT_DIST].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST]);
                            comboBox_nema[lsiv_phase][PED_MVMT_VOL].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL]));
                            comboBox_nema[lsiv_phase][PED_MVMT_PAR].setSelectedItem(twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR]));

                            resetMovementParameter(lsiv_phase);
                        }

                        comboBox_nema[lsiv_phase][PED_MVMT_DIST].addActionListener(pedMvmtDistActionListener);
                        comboBox_nema[lsiv_phase][PED_MVMT_DIST].addKeyListener(pedMvmtDistKeyListener);
                    }
                }
            }
        }
    } // end of PedMvmtKeyListener()

    class SetAllActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (event.getSource() == setAllButton[lsiv_field]) {
                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                        if (comboBox_nema[lsiv_phase][lsiv_field].isEnabled()) {
                            comboBox_nema[lsiv_phase][lsiv_field].setSelectedItem(setAllComboBox[lsiv_field].getSelectedItem().toString());
                        }
                    }
                }
            }
        }
    } // end of SetAllActionListener

    class SetAllKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    if (event.getSource() == setAllButton[lsiv_field]) {
                        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                            if (comboBox_nema[lsiv_phase][lsiv_field].isEnabled()) {
                                comboBox_nema[lsiv_phase][lsiv_field].setSelectedItem(setAllComboBox[lsiv_field].getSelectedItem().toString());
                            }
                        }
                    }
                }
            }
        }
    } // end of SetAllKeyListener

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
                            + " and is determined by the program.", cbo_total.getSelectedItem().toString(), "0", " ", "0", Integer.toString(PARAMS.TEXAS_MODEL_NPN), "1");
                }
                else if (event.getSource() == btngrp[1]) {
                    new HelpDialog(true, "NEMA Movement Button", "The NEMA Movement button opens NEMA Movement window", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == btngrp[2]) {
                    new HelpDialog(true, "NEMA RingGroup Button", "The NEMA RingGroup button opens NEMA RingGroup window", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == btngrp[3]) {
                    new HelpDialog(true, "NEMA Overlap Button", "The NEMA Overlap button opens NEMA Overlap window", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == cbo_gapout) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT];
                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT], lclv_tx_fmt.mstv_name,
                            lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT], cbo_gapout
                                    .getSelectedItem().toString(),
                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT],
                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT], " ", " ", " ");
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (event.getSource() == add[lsiv_phase]) {
                        new HelpDialog(
                                true,
                                "Add Button",
                                "The Add button moves the data down 1 phase for all phases below this phase, inserts a new phase at the current position, copies the values of all parameters from the previous phase to the new phase, and increases the Total Phases by 1.",
                                " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == del[lsiv_phase]) {
                        new HelpDialog(true, "Delete Button", "The Delete button moves the data up 1 phase for all phases below this phase and decreases the Total Phases by 1.", " ", " ", " ", " ",
                                " ", " ", " ");
                    }
                    else if (event.getSource() == up[lsiv_phase]) {
                        new HelpDialog(true, "Up Button", "The Up button swaps the data for the previous phase and the current phase.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (event.getSource() == down[lsiv_phase]) {
                        new HelpDialog(true, "Down Button", "The Down button swaps the data for the next phase and the current phase.", " ", " ", " ", " ", " ", " ", " ");
                    }
                }

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];

                    if (event.getSource() == setAllButton[lsiv_field]) {
                        new HelpDialog(true, "Set All Button", "The Set All button sets the value of " + lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]
                                + " for all phases to the value selected above.", " ", " ", " ", " ", " ", " ", " ");
                    }

                    if (event.getSource() == setAllComboBox[lsiv_field]) {
                        switch (lsiv_field) {
                            case 2:
                            case 6:
                            case 7:
                            case 8:
                            case 20:
                            case 24:
                            case 28:
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field] + " for all phases",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field], setAllComboBox[lsiv_field].getSelectedItem().toString(),
                                        Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]), " ",
                                        Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]),
                                        Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]),
                                        Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]));
                                break;

                            case 1:
                            case 3:
                            case 4:
                            case 5:
                            case 9:
                            case 10:
                            case 11:
                            case 21:
                            case 22:
                            case 23:
                            case 27:
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field] + " for all phases",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field], setAllComboBox[lsiv_field].getSelectedItem().toString(),
                                        Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]), " ",
                                        Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]),
                                        Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]));
                                break;

                            case 12:
                            case 13:
                            case 14:
                            case 15:
                            case 16:
                            case 17:
                            case 18:
                            case 19:
                            case 25:
                            case 26:
                                new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field], lclv_tx_fmt.mstv_name.substring(8),
                                        lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field] + " for all phases",
                                        lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field], setAllComboBox[lsiv_field].getSelectedItem().toString(),
                                        lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                        lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1
                                                + lsiv_field],
                                        " ", " ", " ");
                                break;
                        }
                    }
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                        if (event.getSource() == comboBox_nema[lsiv_phase][lsiv_field]) {
                            switch (lsiv_field) {
                                case 2:
                                case 6:
                                case 7:
                                case 8:
                                case 20:
                                case 24:
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_phase)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            comboBox_nema[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                            Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]), " ",
                                            Double.toString(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]),
                                            Double.toString(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]),
                                            Double.toString(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]));
                                    break;

                                case 28:
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];
                                    String name = lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_phase));

                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM];

                                    // check and set headway distribution
                                    // gdvsim.gclv_inter..mbov_is_hd_* values
                                    gdvsim.gclv_inter.check_and_set_headway_distribution(comboBox_nema[lsiv_phase][PED_MVMT_DIST].getSelectedItem().toString());
                                    if (gdvsim.gclv_inter.msiv_returnCode != gdvsim.gclv_inter.RETURN_SUCCESS)
                                        return;

                                    if (gdvsim.gclv_inter.mbov_is_hd_CONSTAN) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN],
                                                comboBox_nema[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_CONSTAN]));
                                    }
                                    else if (gdvsim.gclv_inter.mbov_is_hd_ERLANG) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG],
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG], comboBox_nema[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_ERLANG]));
                                    }
                                    else if (gdvsim.gclv_inter.mbov_is_hd_GAMMA) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA],
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA], comboBox_nema[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_GAMMA]));
                                    }
                                    else if (gdvsim.gclv_inter.mbov_is_hd_LOGNRML) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML],
                                                comboBox_nema[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_LOGNRML]));
                                    }
                                    else if (gdvsim.gclv_inter.mbov_is_hd_NEGEXP) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP],
                                                lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP], comboBox_nema[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_NEGEXP]));
                                    }
                                    else if (gdvsim.gclv_inter.mbov_is_hd_SNEGEXP) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP],
                                                comboBox_nema[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_SNEGEXP]));
                                    }
                                    else if (gdvsim.gclv_inter.mbov_is_hd_UNIFORM) {
                                        new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM], name,
                                                lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM], lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM],
                                                comboBox_nema[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                                twoDigits.format(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]), " ",
                                                twoDigits.format(lclv_tx_fmt.mdfa_min[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_max[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]),
                                                twoDigits.format(lclv_tx_fmt.mdfa_inc[gdvsim.gclv_inter.TX_FMT_GDV_HDWAY_PARAM_UNIFORM]));
                                    }

                                    break;

                                case 1:
                                case 3:
                                case 4:
                                case 9:
                                case 10:
                                case 21:
                                case 22:
                                case 23:
                                case 27:
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_phase)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            comboBox_nema[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]), " ",
                                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]),
                                            Integer.toString(lclv_tx_fmt.msia_max[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]),
                                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]));
                                    break;

                                case 5:
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_phase)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            comboBox_nema[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]), " ",
                                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]),
                                            Integer.toString((int)(simStartTime + simSimulationTime)), Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1
                                                    + lsiv_field]));
                                    break;

                                case 11:
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_phase)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            comboBox_nema[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                            Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]), " ",
                                            Integer.toString(lclv_tx_fmt.msia_min[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]), Integer.toString(getNumberOfPhases()),
                                            Integer.toString(lclv_tx_fmt.msia_inc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field]));
                                    break;

                                case 12:
                                case 13:
                                case 14:
                                case 15:
                                case 16:
                                case 17:
                                case 18:
                                case 19:
                                case 25:
                                case 26:
                                    lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];
                                    new HelpDialog(lclv_tx_fmt.mboa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            lclv_tx_fmt.mstv_name.replace("#", Integer.toString(lsiv_phase)), lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_help[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            comboBox_nema[lsiv_phase][lsiv_field].getSelectedItem().toString(),
                                            lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN - 1 + lsiv_field],
                                            lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN
                                                    - 1 + lsiv_field],
                                            " ", " ", " ");
                                    break;
                            }
                        }
                    }
                }
            }
        }
    } // end of help

    void setStatus(int sumOfPhases) {
        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            if (lsiv_phase <= sumOfPhases) {
                label_mainset01[lsiv_phase].setEnabled(true);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_nema[lsiv_phase][lsiv_field].setEnabled(true);
                }
            }
            else {
                label_mainset01[lsiv_phase].setEnabled(false);

                for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                    comboBox_nema[lsiv_phase][lsiv_field].setEnabled(false);
                }
            }
        }

        if (sumOfPhases == PARAMS.TEXAS_MODEL_NPN) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                add[lsiv_phase].setEnabled(false);
            }
        }
        else {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                if (lsiv_phase <= (sumOfPhases + 1)) {
                    add[lsiv_phase].setEnabled(true);
                }
                else {
                    add[lsiv_phase].setEnabled(false);
                }
            }
        }

        if (sumOfPhases == PARAMS.TEXAS_MODEL_NPN) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                del[lsiv_phase].setEnabled(true);
            }
        }
        else if (sumOfPhases <= 2) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                del[lsiv_phase].setEnabled(false);
            }
        }
        else {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                if (lsiv_phase <= sumOfPhases) {
                    del[lsiv_phase].setEnabled(true);
                }
                else {
                    del[lsiv_phase].setEnabled(false);
                }
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            if (lsiv_phase <= sumOfPhases) {
                up[lsiv_phase].setEnabled(true);
            }
            else {
                up[lsiv_phase].setEnabled(false);
            }

            up[1].setEnabled(false);
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            if (lsiv_phase < sumOfPhases) {
                down[lsiv_phase].setEnabled(true);
            }
            else {
                down[lsiv_phase].setEnabled(false);
            }
        }
        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            if (comboBox_nema[lsiv_phase][USE_VOLDENOPT].getSelectedItem().toString().equals("YES")) {
                comboBox_nema[lsiv_phase][VOLDEN_ADDINI].setEnabled(true);
                comboBox_nema[lsiv_phase][VOLDEN_MAXINI].setEnabled(true);
                comboBox_nema[lsiv_phase][VOLDEN_T2RED].setEnabled(true);
                comboBox_nema[lsiv_phase][VOLDEN_TB4RED].setEnabled(true);
                comboBox_nema[lsiv_phase][VOLDEN_MINGAP].setEnabled(true);
            }
            else {
                comboBox_nema[lsiv_phase][VOLDEN_ADDINI].setEnabled(false);
                comboBox_nema[lsiv_phase][VOLDEN_MAXINI].setEnabled(false);
                comboBox_nema[lsiv_phase][VOLDEN_T2RED].setEnabled(false);
                comboBox_nema[lsiv_phase][VOLDEN_TB4RED].setEnabled(false);
                comboBox_nema[lsiv_phase][VOLDEN_MINGAP].setEnabled(false);
            }

            if (comboBox_nema[lsiv_phase][PED_MVMT].getSelectedItem().toString().equals("YES")) {
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].setEnabled(true);
                comboBox_nema[lsiv_phase][PED_MVMT_VOL].setEnabled(true);
                comboBox_nema[lsiv_phase][PED_MVMT_PAR].setEnabled(true);
            }
            else {
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].setEnabled(false);
                comboBox_nema[lsiv_phase][PED_MVMT_VOL].setEnabled(false);
                comboBox_nema[lsiv_phase][PED_MVMT_PAR].setEnabled(false);
            }
        }

    } // end of setStatus

    void setValueAfterAdd(int sum, int index) {
        for (int lsiv_phase = sum; lsiv_phase >= index; lsiv_phase--) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == PED_MVMT_PAR) {
                    resetMovementParameter(lsiv_phase + 1);
                    comboBox_nema[lsiv_phase + 1][PED_MVMT_PAR].setSelectedItem(comboBox_nema[lsiv_phase][PED_MVMT_PAR].getSelectedItem().toString());
                }
                else {
                    comboBox_nema[lsiv_phase + 1][lsiv_field].setSelectedItem(comboBox_nema[lsiv_phase][lsiv_field].getSelectedItem().toString());
                }
            }
        }

        if (gdvsim.flag_greenSequence_ok) {
            for (int lsiv_phase = sum; lsiv_phase >= index; lsiv_phase--) {
                for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        if (gdvsim.greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            gdvsim.greenSeqValue[lsiv_phase + 1][lsiv_leg][lsiv_lane] = gdvsim.greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane];
                            gdvsim.greenSeqStat[lsiv_phase + 1][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.TX_FROM_USER;
                        }
                    }
                }
            }
        }

        if (gdvsim.flag_detConnForNema_ok) {
            for (int lsiv_phase = sum; lsiv_phase >= index; lsiv_phase--) {
                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                    if (gdvsim.detConnNemaVStat[lsiv_phase][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.detConnNemaValue[lsiv_phase + 1][lsiv_det] = gdvsim.detConnNemaValue[lsiv_phase][lsiv_det];
                        gdvsim.detConnNemaVStat[lsiv_phase + 1][lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                    else {
                        gdvsim.detConnNemaValue[lsiv_phase + 1][lsiv_det] = detConnNemaDefConn[lsiv_phase][lsiv_det];
                        gdvsim.detConnNemaVStat[lsiv_phase + 1][lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }

                    if (gdvsim.detConnNemaTStat[lsiv_phase][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.detConnNemaType[lsiv_phase + 1][lsiv_det] = gdvsim.detConnNemaType[lsiv_phase][lsiv_det];
                        gdvsim.detConnNemaTStat[lsiv_phase + 1][lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                    else {
                        gdvsim.detConnNemaType[lsiv_phase + 1][lsiv_det] = detConnNemaDefType[lsiv_phase][lsiv_det];
                        gdvsim.detConnNemaTStat[lsiv_phase + 1][lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }
        }

        // if(gdvsim.flag_nemaMovement_ok)
        {
            for (int lsiv_phase = sum; lsiv_phase >= index; lsiv_phase--) {
                if (gdvsim.numOfMovementStat[lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.numOfMovementValue[lsiv_phase + 1] = gdvsim.numOfMovementValue[lsiv_phase];
                    gdvsim.numOfMovementStat[lsiv_phase + 1] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    gdvsim.numOfMovementValue[lsiv_phase + 1] = numOfMovementDefault[lsiv_phase];
                    gdvsim.numOfMovementStat[lsiv_phase + 1] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }

                for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                    if (gdvsim.movementStat[lsiv_phase][lsiv_mvt] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.movementValue[lsiv_phase + 1][lsiv_mvt] = gdvsim.movementValue[lsiv_phase][lsiv_mvt];
                        gdvsim.movementStat[lsiv_phase + 1][lsiv_mvt] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                    else {
                        gdvsim.movementValue[lsiv_phase + 1][lsiv_mvt] = movementDefault[lsiv_phase][lsiv_mvt];
                        gdvsim.movementStat[lsiv_phase + 1][lsiv_mvt] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }

            {
                gdvsim.numOfMovementStat[sum + 1] = gdvsim.gclv_inter.TX_FROM_USER;
                gdvsim.movementStat[sum + 1][1] = gdvsim.gclv_inter.TX_FROM_USER;
            }
        } // end of if(gdvsim.flag_nemaMovement_ok)

        // if(gdvsim.flag_nemaOverlap_ok)
        {
            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (gdvsim.overlapStat[lsiv_olp][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID && gdvsim.overlapValue[lsiv_olp][lsiv_phase] >= index) {
                        gdvsim.overlapValue[lsiv_olp][lsiv_phase]++;
                    }
                }
            }
        } // end of if(gdvsim.flag_nemaOverlap_ok)

        if (gdvsim.flag_nemaRingGroup_ok) {
            int addRing = 0;
            int addGroup = 0;
            int addPhase = 0;

            for (int lsiv_ring = 1; lsiv_ring <= gdvsim.ringGroupNumOfRingValue; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= gdvsim.ringGroupNumOfGroupValue; lsiv_group++) {
                    for (int lsiv_phase = 1; lsiv_phase <= gdvsim.ringGroupNumOfPhasesValue[lsiv_ring][lsiv_group]; lsiv_phase++) {
                        if (gdvsim.ringGroupStat[lsiv_ring][lsiv_group][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID && gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] == index) {
                            addRing = lsiv_ring;
                            addGroup = lsiv_group;
                            addPhase = lsiv_phase;
                        }
                    }
                }
            }

            for (int lsiv_ring = 1; lsiv_ring <= gdvsim.ringGroupNumOfRingValue; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= gdvsim.ringGroupNumOfGroupValue; lsiv_group++) {
                    for (int lsiv_phase = 1; lsiv_phase <= gdvsim.ringGroupNumOfPhasesValue[lsiv_ring][lsiv_group]; lsiv_phase++) {
                        if (gdvsim.ringGroupStat[lsiv_ring][lsiv_group][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID && gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] >= index) {
                            gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase]++;
                        }
                    }
                }
            }
        } // end of if (gdvsim.flag_nemaRingGroup_ok)
    } // end of setValueAfterAdd

    void setValueAfterDel(int sum, int index) {
        for (int lsiv_phase = index; lsiv_phase < sum; lsiv_phase++) {
            for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
                if (lsiv_field == PED_MVMT_PAR) {
                    resetMovementParameter(lsiv_phase);
                    comboBox_nema[lsiv_phase][PED_MVMT_PAR].setSelectedItem(comboBox_nema[lsiv_phase + 1][PED_MVMT_PAR].getSelectedItem().toString());
                }
                else {
                    comboBox_nema[lsiv_phase][lsiv_field].setSelectedItem(comboBox_nema[lsiv_phase + 1][lsiv_field].getSelectedItem().toString());
                }
            }
        }

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL];

        comboBox_nema[sum][1].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN]));
        comboBox_nema[sum][2].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PAS_TIM]));
        comboBox_nema[sum][3].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1]));
        comboBox_nema[sum][4].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_2]));
        comboBox_nema[sum][5].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_1TO2_TIME]));
        comboBox_nema[sum][6].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_YEL_CHG]));
        comboBox_nema[sum][7].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_CLR]));
        comboBox_nema[sum][8].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_RED_REVERT]));
        comboBox_nema[sum][9].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_WALK]));
        comboBox_nema[sum][10].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_CLR]));
        comboBox_nema[sum][11].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_DUAL_ENT_PH]));
        comboBox_nema[sum][12].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_STORE_DEMAND]);
        comboBox_nema[sum][13].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MAX_RECALL]);
        comboBox_nema[sum][14].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_RECALL]);
        comboBox_nema[sum][15].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_RECALL]);
        comboBox_nema[sum][16].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_RECYCLE]);
        comboBox_nema[sum][17].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_CALLATPHTERM]);
        comboBox_nema[sum][18].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_COND_SERVICE]);
        comboBox_nema[sum][19].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_USE_VOLDENOPT]);
        comboBox_nema[sum][20].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_ADDINI]));
        comboBox_nema[sum][21].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MAXINI]));
        comboBox_nema[sum][22].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_T2RED]));
        comboBox_nema[sum][23].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_TB4RED]));
        comboBox_nema[sum][24].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP]));
        comboBox_nema[sum][25].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT]);
        comboBox_nema[sum][26].setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_DIST]);
        comboBox_nema[sum][27].setSelectedItem(Integer.toString(lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_VOL]));
        comboBox_nema[sum][28].setSelectedItem(Double.toString(lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR]));

        resetMovementParameter(sum);

        if (gdvsim.flag_greenSequence_ok) {
            for (int lsiv_phase = index; lsiv_phase < sum; lsiv_phase++) {
                for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        if (gdvsim.greenSeqStat[lsiv_phase + 1][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            gdvsim.greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane] = gdvsim.greenSeqValue[lsiv_phase + 1][lsiv_leg][lsiv_lane];
                            gdvsim.greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.TX_FROM_USER;
                        }
                    }
                }
            }

            for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    if (gdvsim.greenSeqStat[sum][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.greenSeqValue[sum][lsiv_leg][lsiv_lane] = greenSeqDefault[sum][lsiv_leg][lsiv_lane];
                        gdvsim.greenSeqStat[sum][lsiv_leg][lsiv_lane] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }
        }

        if (gdvsim.flag_detConnForNema_ok) {
            for (int lsiv_phase = index; lsiv_phase < sum; lsiv_phase++) {
                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                    if (gdvsim.detConnNemaVStat[lsiv_phase + 1][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.detConnNemaValue[lsiv_phase][lsiv_det] = gdvsim.detConnNemaValue[lsiv_phase + 1][lsiv_det];
                        gdvsim.detConnNemaVStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                    else {
                        gdvsim.detConnNemaValue[lsiv_phase][lsiv_det] = detConnNemaDefConn[lsiv_phase + 1][lsiv_det];
                        gdvsim.detConnNemaVStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }

                    if (gdvsim.detConnNemaTStat[lsiv_phase + 1][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.detConnNemaType[lsiv_phase][lsiv_det] = gdvsim.detConnNemaType[lsiv_phase + 1][lsiv_det];
                        gdvsim.detConnNemaTStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                    else {
                        gdvsim.detConnNemaType[lsiv_phase][lsiv_det] = detConnNemaDefType[lsiv_phase + 1][lsiv_det];
                        gdvsim.detConnNemaTStat[lsiv_phase][lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }

            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                gdvsim.detConnNemaValue[sum][lsiv_det] = detConnNemaDefConn[sum][lsiv_det];
                gdvsim.detConnNemaVStat[sum][lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.detConnNemaType[sum][lsiv_det] = detConnNemaDefType[sum][lsiv_det];
                gdvsim.detConnNemaTStat[sum][lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        // if(gdvsim.flag_nemaMovement_ok)
        {
            for (int lsiv_phase = index; lsiv_phase < sum; lsiv_phase++) {
                if (gdvsim.numOfMovementStat[lsiv_phase + 1] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.numOfMovementValue[lsiv_phase] = gdvsim.numOfMovementValue[lsiv_phase + 1];
                    gdvsim.numOfMovementStat[lsiv_phase] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    gdvsim.numOfMovementValue[lsiv_phase] = numOfMovementDefault[lsiv_phase + 1];
                    gdvsim.numOfMovementStat[lsiv_phase] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }

                for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                    if (gdvsim.movementStat[lsiv_phase + 1][lsiv_mvt] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.movementValue[lsiv_phase][lsiv_mvt] = gdvsim.movementValue[lsiv_phase + 1][lsiv_mvt];
                        gdvsim.movementStat[lsiv_phase][lsiv_mvt] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                    else {
                        gdvsim.movementValue[lsiv_phase][lsiv_mvt] = movementDefault[lsiv_phase + 1][lsiv_mvt];
                        gdvsim.movementStat[lsiv_phase][lsiv_mvt] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }

            {
                if (gdvsim.numOfMovementStat[sum] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.numOfMovementValue[sum] = numOfMovementDefault[sum];
                    gdvsim.numOfMovementStat[sum] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }

                for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                    if (gdvsim.numOfMovementStat[sum] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.numOfMovementValue[sum] = numOfMovementDefault[sum];
                        gdvsim.numOfMovementStat[sum] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }
        } // end of if(gdvsim.flag_nemaMovement_ok)

        // if(gdvsim.flag_nemaOverlap_ok)
        {
            int[] numOfPhasePerOverlap = new int[PARAMS.TEXAS_MODEL_NON + 1];

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                numOfPhasePerOverlap[lsiv_olp] = 0;
            }

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (gdvsim.overlapStat[lsiv_olp][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        numOfPhasePerOverlap[lsiv_olp]++;
                    }
                }
            }

            int numOfOverlap = 0;

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                if (gdvsim.overlapStat[lsiv_olp][1] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    numOfOverlap++;
                }
            }

            int[] overlapHasIndex = new int[PARAMS.TEXAS_MODEL_NON + 1];

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                overlapHasIndex[lsiv_olp] = 0;
            }

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (gdvsim.overlapStat[lsiv_olp][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID && gdvsim.overlapValue[lsiv_olp][lsiv_phase] == index) {
                        overlapHasIndex[lsiv_olp] = lsiv_phase;
                    }
                }
            }

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (gdvsim.overlapStat[lsiv_olp][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID && gdvsim.overlapValue[lsiv_olp][lsiv_phase] > index) {
                        gdvsim.overlapValue[lsiv_olp][lsiv_phase]--;
                    }
                }
            }

            for (int lsiv_olp = 1; lsiv_olp <= numOfOverlap; lsiv_olp++) {
                if (overlapHasIndex[lsiv_olp] != 0) {
                    for (int lsiv_phase = overlapHasIndex[lsiv_olp]; lsiv_phase < numOfPhasePerOverlap[lsiv_olp]; lsiv_phase++) {
                        gdvsim.overlapValue[lsiv_olp][lsiv_phase] = gdvsim.overlapValue[lsiv_olp][lsiv_phase + 1];
                    }

                    gdvsim.overlapValue[lsiv_olp][numOfPhasePerOverlap[lsiv_olp]] = 0;
                    gdvsim.overlapStat[lsiv_olp][numOfPhasePerOverlap[lsiv_olp]] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                numOfPhasePerOverlap[lsiv_olp] = 0;
            }

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (gdvsim.overlapStat[lsiv_olp][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        numOfPhasePerOverlap[lsiv_olp]++;
                    }
                }
            }

            for (int lsiv_olp = numOfOverlap - 1; lsiv_olp >= 1; lsiv_olp--) {
                if (numOfPhasePerOverlap[lsiv_olp] == 0) {
                    for (int lsiv_olp_i = lsiv_olp; lsiv_olp_i < PARAMS.TEXAS_MODEL_NON; lsiv_olp_i++) {
                        for (int lsiv_phase_i = 1; lsiv_phase_i <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase_i++) {
                            gdvsim.overlapValue[lsiv_olp_i][lsiv_phase_i] = gdvsim.overlapValue[lsiv_olp_i + 1][lsiv_phase_i];
                            gdvsim.overlapStat[lsiv_olp_i][lsiv_phase_i] = gdvsim.overlapStat[lsiv_olp_i + 1][lsiv_phase_i];
                        }
                    }

                    for (int lsiv_phase_i = 1; lsiv_phase_i <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase_i++) {
                        gdvsim.overlapValue[PARAMS.TEXAS_MODEL_NON][lsiv_phase_i] = 0;
                        gdvsim.overlapStat[PARAMS.TEXAS_MODEL_NON][lsiv_phase_i] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }
        }

        if (gdvsim.flag_nemaRingGroup_ok) {
            int delRing = 0;
            int delGroup = 0;
            int delPhase = 0;

            for (int lsiv_ring = 1; lsiv_ring <= gdvsim.ringGroupNumOfRingValue; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= gdvsim.ringGroupNumOfGroupValue; lsiv_group++) {
                    for (int lsiv_phase = 1; lsiv_phase <= gdvsim.ringGroupNumOfPhasesValue[lsiv_ring][lsiv_group]; lsiv_phase++) {
                        if (gdvsim.ringGroupStat[lsiv_ring][lsiv_group][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID && gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] == index) {
                            delRing = lsiv_ring;
                            delGroup = lsiv_group;
                            delPhase = lsiv_phase;
                        }
                    }
                }
            }

            for (int lsiv_ring = 1; lsiv_ring <= gdvsim.ringGroupNumOfRingValue; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= gdvsim.ringGroupNumOfGroupValue; lsiv_group++) {
                    for (int lsiv_phase = 1; lsiv_phase <= gdvsim.ringGroupNumOfPhasesValue[lsiv_ring][lsiv_group]; lsiv_phase++) {
                        if (gdvsim.ringGroupStat[lsiv_ring][lsiv_group][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID && gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] > index) {
                            gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase]--;
                        }
                    }
                }
            }

            if (gdvsim.ringGroupNumOfPhasesValue[delRing][delGroup] == 1) {}
            else {
                for (int lsiv_phase = delPhase; lsiv_phase < gdvsim.ringGroupNumOfPhasesValue[delRing][delGroup]; lsiv_phase++) {
                    gdvsim.ringGroupValue[delRing][delGroup][lsiv_phase] = gdvsim.ringGroupValue[delRing][delGroup][lsiv_phase + 1];
                }

                gdvsim.ringGroupValue[delRing][delGroup][gdvsim.ringGroupNumOfPhasesValue[delRing][delGroup]] = 1;
                gdvsim.ringGroupStat[delRing][delGroup][gdvsim.ringGroupNumOfPhasesValue[delRing][delGroup]] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                gdvsim.ringGroupNumOfPhasesValue[delRing][delGroup]--;
            }

        } // end of if (gdvsim.flag_nemaRingGroup_ok)

    } // end of setValueAfterDel

    void setValueAfterUp(int index) {
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == PED_MVMT_PAR) {
                String temp1;
                String temp2;

                temp1 = comboBox_nema[index][PED_MVMT_PAR].getSelectedItem().toString();
                temp2 = comboBox_nema[index - 1][PED_MVMT_PAR].getSelectedItem().toString();

                resetMovementParameter(index);
                resetMovementParameter(index - 1);

                comboBox_nema[index][PED_MVMT_PAR].setSelectedItem(temp2);
                comboBox_nema[index - 1][PED_MVMT_PAR].setSelectedItem(temp1);
            }
            else {
                String temp;
                temp = comboBox_nema[index][lsiv_field].getSelectedItem().toString();
                comboBox_nema[index][lsiv_field].setSelectedItem(comboBox_nema[index - 1][lsiv_field].getSelectedItem().toString());
                comboBox_nema[index - 1][lsiv_field].setSelectedItem(temp);
            }
        }

        if (gdvsim.flag_greenSequence_ok) {
            for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    if (gdvsim.greenSeqStat[index][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        String temp;

                        temp = gdvsim.greenSeqValue[index][lsiv_leg][lsiv_lane];
                        gdvsim.greenSeqValue[index][lsiv_leg][lsiv_lane] = gdvsim.greenSeqValue[index - 1][lsiv_leg][lsiv_lane];
                        gdvsim.greenSeqValue[index - 1][lsiv_leg][lsiv_lane] = temp;
                    }
                }
            }
        }

        if (gdvsim.flag_detConnForNema_ok) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                int tempV;
                String tempT;

                if (gdvsim.detConnNemaVStat[index][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    tempV = gdvsim.detConnNemaValue[index][lsiv_det];
                }
                else {
                    tempV = detConnNemaDefConn[index][lsiv_det];
                }

                if (gdvsim.detConnNemaTStat[index][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    tempT = gdvsim.detConnNemaType[index][lsiv_det];
                }
                else {
                    tempT = detConnNemaDefType[index][lsiv_det];
                }

                if (gdvsim.detConnNemaVStat[index - 1][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.detConnNemaValue[index][lsiv_det] = gdvsim.detConnNemaValue[index - 1][lsiv_det];
                }
                else {
                    gdvsim.detConnNemaValue[index][lsiv_det] = detConnNemaDefConn[index - 1][lsiv_det];
                }

                if (gdvsim.detConnNemaTStat[index - 1][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.detConnNemaType[index][lsiv_det] = gdvsim.detConnNemaType[index - 1][lsiv_det];
                }
                else {
                    gdvsim.detConnNemaType[index][lsiv_det] = detConnNemaDefType[index - 1][lsiv_det];
                }

                gdvsim.detConnNemaValue[index - 1][lsiv_det] = tempV;
                gdvsim.detConnNemaType[index - 1][lsiv_det] = tempT;
            }

            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                int tempV;
                int tempT;

                tempV = gdvsim.detConnNemaVStat[index][lsiv_det];
                tempT = gdvsim.detConnNemaTStat[index][lsiv_det];
                gdvsim.detConnNemaVStat[index][lsiv_det] = gdvsim.detConnNemaVStat[index - 1][lsiv_det];
                gdvsim.detConnNemaTStat[index][lsiv_det] = gdvsim.detConnNemaTStat[index - 1][lsiv_det];
                gdvsim.detConnNemaVStat[index - 1][lsiv_det] = tempV;
                gdvsim.detConnNemaTStat[index - 1][lsiv_det] = tempT;
            }
        }

        // if(gdvsim.flag_nemaMovement_ok)
        {
            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                int temp;

                temp = gdvsim.movementValue[index][lsiv_mvt];
                gdvsim.movementValue[index][lsiv_mvt] = gdvsim.movementValue[index - 1][lsiv_mvt];
                gdvsim.movementValue[index - 1][lsiv_mvt] = temp;
            }

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                int temp;

                temp = gdvsim.movementStat[index][lsiv_mvt];
                gdvsim.movementStat[index][lsiv_mvt] = gdvsim.movementStat[index - 1][lsiv_mvt];
                gdvsim.movementStat[index - 1][lsiv_mvt] = temp;
            }

            {
                int temp;

                temp = gdvsim.numOfMovementValue[index];
                gdvsim.numOfMovementValue[index] = gdvsim.numOfMovementValue[index - 1];
                gdvsim.numOfMovementValue[index - 1] = temp;
            }

            {
                int temp;

                temp = gdvsim.numOfMovementStat[index];
                gdvsim.numOfMovementStat[index] = gdvsim.numOfMovementStat[index - 1];
                gdvsim.numOfMovementStat[index - 1] = temp;
            }

        } // end of if(gdvsim.flag_nemaMovement_ok)

        // if(gdvsim.flag_nemaOverlap_ok)
        {
            boolean[][] overlapHasIndex = new boolean[PARAMS.TEXAS_MODEL_NON + 1][PARAMS.TEXAS_MODEL_NPN + 1];
            boolean[][] overlapHasIndexMiunsOne = new boolean[PARAMS.TEXAS_MODEL_NON + 1][PARAMS.TEXAS_MODEL_NPN + 1];

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    overlapHasIndex[lsiv_olp][lsiv_phase] = false;
                    overlapHasIndexMiunsOne[lsiv_olp][lsiv_phase] = false;
                }
            }

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (gdvsim.overlapStat[lsiv_olp][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID && gdvsim.overlapValue[lsiv_olp][lsiv_phase] == index) {
                        overlapHasIndex[lsiv_olp][lsiv_phase] = true;
                    }

                    if (gdvsim.overlapStat[lsiv_olp][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID && gdvsim.overlapValue[lsiv_olp][lsiv_phase] == (index - 1)) {
                        overlapHasIndexMiunsOne[lsiv_olp][lsiv_phase] = true;
                    }
                }
            }

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (overlapHasIndex[lsiv_olp][lsiv_phase]) {
                        gdvsim.overlapValue[lsiv_olp][lsiv_phase] = index - 1;
                    }

                    if (overlapHasIndexMiunsOne[lsiv_olp][lsiv_phase]) {
                        gdvsim.overlapValue[lsiv_olp][lsiv_phase] = index;
                    }
                }
            }
        }

        if (gdvsim.flag_nemaRingGroup_ok) {
            boolean[][][] ringGroupHasIndex = new boolean[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRGP + 1];
            boolean[][][] ringGroupHasIndexMiunsOne = new boolean[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRGP + 1];

            for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                        ringGroupHasIndex[lsiv_ring][lsiv_group][lsiv_phase] = false;
                        ringGroupHasIndexMiunsOne[lsiv_ring][lsiv_group][lsiv_phase] = false;
                    }
                }
            }

            for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                        if (gdvsim.ringGroupStat[lsiv_ring][lsiv_group][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID && gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] == index) {
                            ringGroupHasIndex[lsiv_ring][lsiv_group][lsiv_phase] = true;
                        }

                        if (gdvsim.ringGroupStat[lsiv_ring][lsiv_group][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID
                                && gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] == (index - 1)) {
                            ringGroupHasIndexMiunsOne[lsiv_ring][lsiv_group][lsiv_phase] = true;
                        }
                    }
                }
            }

            for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                        if (ringGroupHasIndex[lsiv_ring][lsiv_group][lsiv_phase]) {
                            gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] = index - 1;
                        }

                        if (ringGroupHasIndexMiunsOne[lsiv_ring][lsiv_group][lsiv_phase]) {
                            gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] = index;
                        }
                    }
                }
            }
        } // end of if (gdvsim.flag_nemaRingGroup_ok)

    } // end of setValueAfterUp()

    void setValueAfterDown(int index) {
        for (int lsiv_field = 1; lsiv_field <= NUMOFFIELD; lsiv_field++) {
            if (lsiv_field == PED_MVMT_PAR) {
                String temp1;
                String temp2;

                temp1 = comboBox_nema[index][PED_MVMT_PAR].getSelectedItem().toString();
                temp2 = comboBox_nema[index + 1][PED_MVMT_PAR].getSelectedItem().toString();

                resetMovementParameter(index);
                resetMovementParameter(index + 1);

                comboBox_nema[index][PED_MVMT_PAR].setSelectedItem(temp2);
                comboBox_nema[index + 1][PED_MVMT_PAR].setSelectedItem(temp1);
            }
            else {
                String temp;
                temp = comboBox_nema[index][lsiv_field].getSelectedItem().toString();
                comboBox_nema[index][lsiv_field].setSelectedItem(comboBox_nema[index + 1][lsiv_field].getSelectedItem().toString());
                comboBox_nema[index + 1][lsiv_field].setSelectedItem(temp);
            }
        }

        if (gdvsim.flag_greenSequence_ok) {
            for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    if (gdvsim.greenSeqStat[index + 1][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        String temp;

                        temp = gdvsim.greenSeqValue[index][lsiv_leg][lsiv_lane];
                        gdvsim.greenSeqValue[index][lsiv_leg][lsiv_lane] = gdvsim.greenSeqValue[index + 1][lsiv_leg][lsiv_lane];
                        gdvsim.greenSeqValue[index + 1][lsiv_leg][lsiv_lane] = temp;
                    }
                }
            }
        }

        if (gdvsim.flag_detConnForNema_ok) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                int tempV;
                String tempT;

                if (gdvsim.detConnNemaVStat[index][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    tempV = gdvsim.detConnNemaValue[index][lsiv_det];
                }
                else {
                    tempV = detConnNemaDefConn[index][lsiv_det];
                }

                if (gdvsim.detConnNemaTStat[index][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    tempT = gdvsim.detConnNemaType[index][lsiv_det];
                }
                else {
                    tempT = detConnNemaDefType[index][lsiv_det];
                }

                if (gdvsim.detConnNemaVStat[index + 1][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.detConnNemaValue[index][lsiv_det] = gdvsim.detConnNemaValue[index + 1][lsiv_det];
                }
                else {
                    gdvsim.detConnNemaValue[index][lsiv_det] = detConnNemaDefConn[index + 1][lsiv_det];
                }

                if (gdvsim.detConnNemaTStat[index + 1][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.detConnNemaType[index][lsiv_det] = gdvsim.detConnNemaType[index + 1][lsiv_det];
                }
                else {
                    gdvsim.detConnNemaType[index][lsiv_det] = detConnNemaDefType[index + 1][lsiv_det];
                }

                gdvsim.detConnNemaValue[index + 1][lsiv_det] = tempV;
                gdvsim.detConnNemaType[index + 1][lsiv_det] = tempT;
            }

            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                int tempV;
                int tempT;

                tempV = gdvsim.detConnNemaVStat[index][lsiv_det];
                tempT = gdvsim.detConnNemaTStat[index][lsiv_det];
                gdvsim.detConnNemaVStat[index][lsiv_det] = gdvsim.detConnNemaVStat[index + 1][lsiv_det];
                gdvsim.detConnNemaTStat[index][lsiv_det] = gdvsim.detConnNemaTStat[index + 1][lsiv_det];
                gdvsim.detConnNemaVStat[index + 1][lsiv_det] = tempV;
                gdvsim.detConnNemaTStat[index + 1][lsiv_det] = tempT;
            }
        }

        // if(gdvsim.flag_nemaMovement_ok)
        {
            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                int temp;

                temp = gdvsim.movementValue[index][lsiv_mvt];
                gdvsim.movementValue[index][lsiv_mvt] = gdvsim.movementValue[index + 1][lsiv_mvt];
                gdvsim.movementValue[index + 1][lsiv_mvt] = temp;
            }

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                int temp;

                temp = gdvsim.movementStat[index][lsiv_mvt];
                gdvsim.movementStat[index][lsiv_mvt] = gdvsim.movementStat[index + 1][lsiv_mvt];
                gdvsim.movementStat[index + 1][lsiv_mvt] = temp;
            }

            {
                int temp;

                temp = gdvsim.numOfMovementValue[index];
                gdvsim.numOfMovementValue[index] = gdvsim.numOfMovementValue[index + 1];
                gdvsim.numOfMovementValue[index + 1] = temp;
            }

            {
                int temp;

                temp = gdvsim.numOfMovementStat[index];
                gdvsim.numOfMovementStat[index] = gdvsim.numOfMovementStat[index + 1];
                gdvsim.numOfMovementStat[index + 1] = temp;
            }
        }

        // if(gdvsim.flag_nemaOverlap_ok)
        {
            boolean[][] overlapHasIndex = new boolean[PARAMS.TEXAS_MODEL_NON + 1][PARAMS.TEXAS_MODEL_NPN + 1];
            boolean[][] overlapHasIndexPlusOne = new boolean[PARAMS.TEXAS_MODEL_NON + 1][PARAMS.TEXAS_MODEL_NPN + 1];

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    overlapHasIndex[lsiv_olp][lsiv_phase] = false;
                    overlapHasIndexPlusOne[lsiv_olp][lsiv_phase] = false;
                }
            }

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (gdvsim.overlapStat[lsiv_olp][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID && gdvsim.overlapValue[lsiv_olp][lsiv_phase] == index) {
                        overlapHasIndex[lsiv_olp][lsiv_phase] = true;
                    }

                    if (gdvsim.overlapStat[lsiv_olp][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID && gdvsim.overlapValue[lsiv_olp][lsiv_phase] == (index + 1)) {
                        overlapHasIndexPlusOne[lsiv_olp][lsiv_phase] = true;
                    }
                }
            }

            for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (overlapHasIndex[lsiv_olp][lsiv_phase]) {
                        gdvsim.overlapValue[lsiv_olp][lsiv_phase] = index + 1;
                    }

                    if (overlapHasIndexPlusOne[lsiv_olp][lsiv_phase]) {
                        gdvsim.overlapValue[lsiv_olp][lsiv_phase] = index;
                    }
                }
            }
        }

        if (gdvsim.flag_nemaRingGroup_ok) {
            boolean[][][] ringGroupHasIndex = new boolean[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRGP + 1];
            boolean[][][] ringGroupHasIndexPlusOne = new boolean[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRGP + 1];

            for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                        ringGroupHasIndex[lsiv_ring][lsiv_group][lsiv_phase] = false;
                        ringGroupHasIndexPlusOne[lsiv_ring][lsiv_group][lsiv_phase] = false;
                    }
                }
            }

            for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                        if (gdvsim.ringGroupStat[lsiv_ring][lsiv_group][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID && gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] == index) {
                            ringGroupHasIndex[lsiv_ring][lsiv_group][lsiv_phase] = true;
                        }

                        if (gdvsim.ringGroupStat[lsiv_ring][lsiv_group][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID
                                && gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] == (index + 1)) {
                            ringGroupHasIndexPlusOne[lsiv_ring][lsiv_group][lsiv_phase] = true;
                        }
                    }
                }
            }

            for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                        if (ringGroupHasIndex[lsiv_ring][lsiv_group][lsiv_phase]) {
                            gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] = index + 1;
                        }

                        if (ringGroupHasIndexPlusOne[lsiv_ring][lsiv_group][lsiv_phase]) {
                            gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] = index;
                        }
                    }
                }
            }
        } // end of if (gdvsim.flag_nemaRingGroup_ok)
    } // end of setValueAfterDown()

    class AddActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                comboBox_nema[lsiv_phase][PED_MVMT].removeActionListener(pedMvmtActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT].removeKeyListener(pedMvmtKeyListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeActionListener(pedMvmtDistActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeKeyListener(pedMvmtDistKeyListener);
            }

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                if (event.getSource() == add[lsiv_phase]) {
                    int numOfPhasesBeforeClick;
                    numOfPhasesBeforeClick = getNumberOfPhases();
                    setStatus(numOfPhasesBeforeClick + 1);
                    setValueAfterAdd(numOfPhasesBeforeClick, lsiv_phase);
                    setStatus(numOfPhasesBeforeClick + 1);

                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfPhases()));

                    break;
                }
            }

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                comboBox_nema[lsiv_phase][PED_MVMT].addActionListener(pedMvmtActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT].addKeyListener(pedMvmtKeyListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].addActionListener(pedMvmtDistActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].addKeyListener(pedMvmtDistKeyListener);
            }
        }
    } // end of AddActionListener

    class AddKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    comboBox_nema[lsiv_phase][PED_MVMT].removeActionListener(pedMvmtActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT].removeKeyListener(pedMvmtKeyListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeActionListener(pedMvmtDistActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeKeyListener(pedMvmtDistKeyListener);
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (event.getSource() == add[lsiv_phase]) {
                        int numOfPhasesBeforeClick;
                        numOfPhasesBeforeClick = getNumberOfPhases();
                        setStatus(numOfPhasesBeforeClick + 1);
                        setValueAfterAdd(numOfPhasesBeforeClick, lsiv_phase);
                        setStatus(numOfPhasesBeforeClick + 1);

                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfPhases()));
                        break;
                    }
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    comboBox_nema[lsiv_phase][PED_MVMT].addActionListener(pedMvmtActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT].addKeyListener(pedMvmtKeyListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].addActionListener(pedMvmtDistActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].addKeyListener(pedMvmtDistKeyListener);
                }
            }
        }
    } // end of AddKeyListener

    class DelActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                comboBox_nema[lsiv_phase][PED_MVMT].removeActionListener(pedMvmtActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT].removeKeyListener(pedMvmtKeyListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeActionListener(pedMvmtDistActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeKeyListener(pedMvmtDistKeyListener);
            }

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                if (event.getSource() == del[lsiv_phase]) {
                    int numOfPhasesBeforeClick;
                    numOfPhasesBeforeClick = getNumberOfPhases();
                    setValueAfterDel(numOfPhasesBeforeClick, lsiv_phase);
                    setStatus(numOfPhasesBeforeClick - 1);

                    cbo_total.removeAllItems();
                    cbo_total.addItem(Integer.toString(getNumberOfPhases()));

                    if (!del[lsiv_phase].isEnabled()) {
                        okButton.requestFocus();
                    }

                    break;
                }
            }

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                comboBox_nema[lsiv_phase][PED_MVMT].addActionListener(pedMvmtActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT].addKeyListener(pedMvmtKeyListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].addActionListener(pedMvmtDistActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].addKeyListener(pedMvmtDistKeyListener);
            }
        }
    } // end of DelActionListener

    class DelKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    comboBox_nema[lsiv_phase][PED_MVMT].removeActionListener(pedMvmtActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT].removeKeyListener(pedMvmtKeyListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeActionListener(pedMvmtDistActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeKeyListener(pedMvmtDistKeyListener);
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (event.getSource() == del[lsiv_phase]) {
                        int numOfPhasesBeforeClick;
                        numOfPhasesBeforeClick = getNumberOfPhases();
                        setValueAfterDel(numOfPhasesBeforeClick, lsiv_phase);
                        setStatus(numOfPhasesBeforeClick - 1);

                        cbo_total.removeAllItems();
                        cbo_total.addItem(Integer.toString(getNumberOfPhases()));

                        if (!del[lsiv_phase].isEnabled()) {
                            okButton.requestFocus();
                        }

                        break;
                    }
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    comboBox_nema[lsiv_phase][PED_MVMT].addActionListener(pedMvmtActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT].addKeyListener(pedMvmtKeyListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].addActionListener(pedMvmtDistActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].addKeyListener(pedMvmtDistKeyListener);
                }
            }
        }
    } // end of DelKeyListener

    class UpActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                comboBox_nema[lsiv_phase][PED_MVMT].removeActionListener(pedMvmtActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT].removeKeyListener(pedMvmtKeyListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeActionListener(pedMvmtDistActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeKeyListener(pedMvmtDistKeyListener);
            }

            for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                if (event.getSource() == up[lsiv_phase]) {
                    setValueAfterUp(lsiv_phase);
                    setStatus(getNumberOfPhases());
                }
            }

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                comboBox_nema[lsiv_phase][PED_MVMT].addActionListener(pedMvmtActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT].addKeyListener(pedMvmtKeyListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].addActionListener(pedMvmtDistActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].addKeyListener(pedMvmtDistKeyListener);
            }
        }
    } // end of UpActionListener

    class UpKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    comboBox_nema[lsiv_phase][PED_MVMT].removeActionListener(pedMvmtActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT].removeKeyListener(pedMvmtKeyListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeActionListener(pedMvmtDistActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeKeyListener(pedMvmtDistKeyListener);
                }

                for (int lsiv_phase = 2; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (event.getSource() == up[lsiv_phase]) {
                        setValueAfterUp(lsiv_phase);
                        setStatus(getNumberOfPhases());
                    }
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    comboBox_nema[lsiv_phase][PED_MVMT].addActionListener(pedMvmtActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT].addKeyListener(pedMvmtKeyListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].addActionListener(pedMvmtDistActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].addKeyListener(pedMvmtDistKeyListener);
                }

            }
        }
    } // end of UpKeyListener

    class DownActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                comboBox_nema[lsiv_phase][PED_MVMT].removeActionListener(pedMvmtActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT].removeKeyListener(pedMvmtKeyListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeActionListener(pedMvmtDistActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeKeyListener(pedMvmtDistKeyListener);
            }

            for (int lsiv_phase = 1; lsiv_phase < PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                if (event.getSource() == down[lsiv_phase]) {
                    setValueAfterDown(lsiv_phase);
                    setStatus(getNumberOfPhases());
                }
            }

            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                comboBox_nema[lsiv_phase][PED_MVMT].addActionListener(pedMvmtActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT].addKeyListener(pedMvmtKeyListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].addActionListener(pedMvmtDistActionListener);
                comboBox_nema[lsiv_phase][PED_MVMT_DIST].addKeyListener(pedMvmtDistKeyListener);
            }
        }
    } // end of DownActionListener

    class DownKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    comboBox_nema[lsiv_phase][PED_MVMT].removeActionListener(pedMvmtActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT].removeKeyListener(pedMvmtKeyListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeActionListener(pedMvmtDistActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].removeKeyListener(pedMvmtDistKeyListener);
                }

                for (int lsiv_phase = 1; lsiv_phase < PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    if (event.getSource() == down[lsiv_phase]) {
                        setValueAfterDown(lsiv_phase);
                        setStatus(getNumberOfPhases());
                    }
                }

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                    comboBox_nema[lsiv_phase][PED_MVMT].addActionListener(pedMvmtActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT].addKeyListener(pedMvmtKeyListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].addActionListener(pedMvmtDistActionListener);
                    comboBox_nema[lsiv_phase][PED_MVMT_DIST].addKeyListener(pedMvmtDistKeyListener);
                }
            }
        }
    } // end of DownKeyListener

    void saveData() {
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases = getNumberOfPhases();
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            for (int item = gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN; item <= gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_VOLDEN_MINGAP; item++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[item] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phase++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_min_grn = Integer.valueOf(comboBox_nema[lsiv_phase][1].getSelectedItem().toString())
                    .intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_pas_tim = Double.valueOf(comboBox_nema[lsiv_phase][2].getSelectedItem().toString())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_max_1 = Integer.valueOf(comboBox_nema[lsiv_phase][3].getSelectedItem().toString())
                    .intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_max_2 = Integer.valueOf(comboBox_nema[lsiv_phase][4].getSelectedItem().toString())
                    .intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_max_1to2_time = Integer.valueOf(
                    comboBox_nema[lsiv_phase][5].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_yel_chg = Double.valueOf(comboBox_nema[lsiv_phase][6].getSelectedItem().toString())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_red_clr = Double.valueOf(comboBox_nema[lsiv_phase][7].getSelectedItem().toString())
                    .doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_red_revert = Double
                    .valueOf(comboBox_nema[lsiv_phase][8].getSelectedItem().toString()).doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_walk = Integer.valueOf(comboBox_nema[lsiv_phase][9].getSelectedItem().toString())
                    .intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_ped_clr = Integer.valueOf(comboBox_nema[lsiv_phase][10].getSelectedItem().toString())
                    .intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_dual_ent_ph = Integer.valueOf(
                    comboBox_nema[lsiv_phase][11].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_store_demand = comboBox_nema[lsiv_phase][12].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_max_recall = comboBox_nema[lsiv_phase][13].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_min_recall = comboBox_nema[lsiv_phase][14].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_recall = comboBox_nema[lsiv_phase][15].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_recycle = comboBox_nema[lsiv_phase][16].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_call_at_phase_term = comboBox_nema[lsiv_phase][17].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_conditional_service = comboBox_nema[lsiv_phase][18].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_use_volden_options = comboBox_nema[lsiv_phase][19].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_volden_add_ini = Double.valueOf(
                    comboBox_nema[lsiv_phase][20].getSelectedItem().toString()).doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_volden_max_ini = Integer.valueOf(
                    comboBox_nema[lsiv_phase][21].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_volden_t2r_veh = Integer.valueOf(
                    comboBox_nema[lsiv_phase][22].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_volden_tb4r_veh = Integer.valueOf(
                    comboBox_nema[lsiv_phase][23].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_volden_min_gap = Double.valueOf(
                    comboBox_nema[lsiv_phase][24].getSelectedItem().toString()).doubleValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt = comboBox_nema[lsiv_phase][25].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mstv_nem2_ped_mvmt_dist = comboBox_nema[lsiv_phase][26].getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].msiv_nem2_ped_mvmt_vol = Integer.valueOf(
                    comboBox_nema[lsiv_phase][27].getSelectedItem().toString()).intValue();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mdfv_nem2_ped_mvmt_par = Double.valueOf(
                    comboBox_nema[lsiv_phase][28].getSelectedItem().toString()).doubleValue();

            for (int item = gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_MIN_GRN; item <= gdvsim.gclv_inter.TX_FMT_SIM_NEM2_SIGNAL_PED_MVMT_PAR; item++) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_ph[lsiv_phase].mclv_aux.msia_stat[item] = gdvsim.gclv_inter.TX_FROM_USER;
            }

            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_nem2_simul_gapout = cbo_gapout.getSelectedItem().toString();
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] = gdvsim.gclv_inter.TX_FROM_USER;
        }

        if (gdvsim.flag_greenSequence_ok) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1
                                + lsiv_phase] = gdvsim.greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane];

                        if (gdvsim.greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01
                                    - 1 + lsiv_phase] = gdvsim.greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane];
                        }
                    }
                }
            }

            for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
                for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                    for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                        gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A - 1
                                + lsiv_overlap] = gdvsim.greenSeqStat[PARAMS.TEXAS_MODEL_NPN + lsiv_overlap][lsiv_leg][lsiv_lane];
                        if (gdvsim.greenSeqStat[PARAMS.TEXAS_MODEL_NPN + lsiv_overlap][lsiv_leg][lsiv_lane] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            gdvsim.gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[gdvsim.gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A
                                    - 1 + lsiv_overlap] = gdvsim.greenSeqValue[PARAMS.TEXAS_MODEL_NPN + lsiv_overlap][lsiv_leg][lsiv_lane];
                        }
                    }
                }
            }
        }

        if (gdvsim.flag_detConnForNema_ok) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                    if (gdvsim.detConnNemaVStat[lsiv_phase][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det] = gdvsim.detConnNemaValue[lsiv_phase][lsiv_det];
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_DET_01 - 1
                                + lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                    else {
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det] = detConnNemaDefConn[lsiv_phase][lsiv_det];
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_DET_01 - 1
                                + lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }

                    if (gdvsim.detConnNemaTStat[lsiv_phase][lsiv_det] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[lsiv_det] = gdvsim.detConnNemaType[lsiv_phase][lsiv_det];
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01 - 1
                                + lsiv_det] = gdvsim.gclv_inter.TX_FROM_USER;
                    }
                    else {
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[lsiv_det] = detConnNemaDefType[lsiv_phase][lsiv_det];
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01 - 1
                                + lsiv_det] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                    }
                }
            }
        }

        if (movementChanged()) {
            gdvsim.flag_greenSequence_ok = false;
        }

        gdvsim.flag_nemaMovement_ok = true;

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msiv_movement_defmov = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_DEFMOV];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_DEFMOV] = gdvsim.gclv_inter.TX_DEFAULT;

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            if (gdvsim.numOfMovementStat[lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_num[lsiv_phase] = gdvsim.numOfMovementValue[lsiv_phase];
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1
                        + lsiv_phase] = gdvsim.gclv_inter.TX_FROM_USER;
            }
            else {
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1
                        + lsiv_phase] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
            }

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                if (gdvsim.movementStat[lsiv_phase][lsiv_mvt] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list[lsiv_phase][lsiv_mvt] = gdvsim.movementValue[lsiv_phase][lsiv_mvt];
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt
                            + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt
                            + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }

        gdvsim.flag_nemaOverlap_ok = true;

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov = gdvsim.numOfOverlap;
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] = gdvsim.numOfOverlapStat;

        for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                if (gdvsim.overlapStat[lsiv_olp][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_olp].msia_phase[lsiv_phase] = gdvsim.overlapValue[lsiv_olp][lsiv_phase];
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_olp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1
                            + lsiv_phase] = gdvsim.gclv_inter.TX_FROM_USER;
                }
                else {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_olp].msia_phase[lsiv_phase] = 0;
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_olp].mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1
                            + lsiv_phase] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                }
            }
        }

        if (gdvsim.flag_nemaRingGroup_ok) {
            for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
                for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_num_ph[lsiv_ring][lsiv_group] = gdvsim.ringGroupNumOfPhasesValue[lsiv_ring][lsiv_group];
                    gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1 - 1
                            + lsiv_group + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG] = gdvsim.gclv_inter.TX_FROM_USER;

                    for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                        if (gdvsim.ringGroupStat[lsiv_ring][lsiv_group][lsiv_phase] != gdvsim.gclv_inter.TX_DATA_IS_INVALID) {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1
                                    + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.msia_ring_group_phases[lsiv_ring][lsiv_group][lsiv_phase] = gdvsim.ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase];
                        }
                        else {
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1
                                    + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP
                                    + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP] = gdvsim.gclv_inter.TX_DATA_IS_INVALID;
                        }
                    }
                }
            }
        } // end of (gdvsim.flag_nemaRingGroup_ok)

        lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX];

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msiv_num_chars_tx = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX_NUM_CHAR_TX];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX_NUM_CHAR_TX] = gdvsim.gclv_inter.TX_FROM_USER;

        for (int lsiv_phase = 1; lsiv_phase <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phase++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text[lsiv_phase] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_01
                    - 1 + lsiv_phase];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_01 - 1
                    + lsiv_phase] = gdvsim.gclv_inter.TX_DEFAULT;
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov; lsiv_overlap++) {
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[lsiv_overlap] = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_A
                    - 1 + lsiv_overlap];
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_A - 1
                    + lsiv_overlap] = gdvsim.gclv_inter.TX_DEFAULT;
        }

        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_phase_diag.msiv_phase_diag = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_DIAG].msia_def[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_DIAG_VALUE];
        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_phase_diag.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_PH_DIAG_VALUE] = gdvsim.gclv_inter.TX_DEFAULT;
    } // end of saveData()

    boolean movementChanged() {
        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1
                    + lsiv_phase] != gdvsim.numOfMovementStat[lsiv_phase]) {
                return true;
            }
            else {
                for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1
                            + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP] != gdvsim.movementStat[lsiv_phase][lsiv_mvt]) {
                        return true;
                    }
                    else {
                        if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.msia_movement_list[lsiv_phase][lsiv_mvt] != gdvsim.movementValue[lsiv_phase][lsiv_mvt]) {
                            return true;
                        }
                    }
                }
            }
        }

        return false;

    } // end of movementChanged

    boolean isError() {
        int numOfPhases = getNumberOfPhases();
        int minGreen;
        int max1;
        int max2;

        for (int lsiv_phase = 1; lsiv_phase <= numOfPhases; lsiv_phase++) {
            minGreen = Integer.valueOf(comboBox_nema[lsiv_phase][1].getSelectedItem().toString()).intValue();
            max1 = Integer.valueOf(comboBox_nema[lsiv_phase][3].getSelectedItem().toString()).intValue();
            max2 = Integer.valueOf(comboBox_nema[lsiv_phase][4].getSelectedItem().toString()).intValue();

            if (minGreen > max1) {
                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " Min Green = " + minGreen + " is greater than Maximum 1 = " + max1 + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (minGreen > max2) {
                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " Min Green = " + minGreen + " is greater than Maximum 2 = " + max2 + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            if (Integer.valueOf(comboBox_nema[lsiv_phase][11].getSelectedItem().toString()).intValue() > numOfPhases) {
                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " Dual Entry Phase Number is greater than Total Phases = " + numOfPhases + ".", "Error Message", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            int TimeToSwitch = Integer.valueOf(comboBox_nema[lsiv_phase][5].getSelectedItem().toString()).intValue();

            if (TimeToSwitch > simStartTime + simSimulationTime) {
                JOptionPane.showMessageDialog(null, "Phase " + lsiv_phase + " Time to Switch from Maximum 1 to Maximum 2 = " + TimeToSwitch
                        + " is greater than the SIM Parameter-Option Data Startup Time + Simulation Time = " + (simStartTime + simSimulationTime) + ".", "Error Message", JOptionPane.ERROR_MESSAGE);

                return true;
            }
        }

        return false;
    }

    class OkApplyActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == okButton || event.getSource() == applyButton) {
                if (!isError()) {
                    gdvsim.flag_nema_ok = true;

                    saveData();

                    if (event.getSource() == okButton) {
                        aFrame.dispose();
                    }
                }
            }

            if (event.getSource() == cancelButton) {
                aFrame.dispose();
            }
        }
    } // end of OkApplyActionListener

    class OkApplyKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == okButton || event.getSource() == applyButton) {
                    if (!isError()) {
                        gdvsim.flag_nema_ok = true;

                        saveData();

                        if (event.getSource() == okButton) {
                            aFrame.dispose();
                        }
                    }
                }

                if (event.getSource() == cancelButton) {
                    aFrame.dispose();
                }
            }
        } // end of keyPressed
    } // end of OkApplyKeyListener
} // end of class NemaDialog
