package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                                gdvsim.java                                 */
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
import java.awt.image.*;
import java.io.*;
import java.text.DecimalFormat;
import javax.imageio.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

public class gdvsim {

    public static BufferedImage bi = null;

    public static AffineTransform biAffineTransform = null;

    public static Intersection gclv_inter = new Intersection();

    public static DrawIntersection myIntersection = new DrawIntersection();

    public static JFrame mainFrame = new JFrame();

    public static JToolBar mainFrameToolBar = new JToolBar();

    public static JMenuBar mainFrameMenuBar = new JMenuBar();

    public static JMenu fileMenu = new JMenu("File");

    public static JMenu viewMenu = new JMenu("View");

    public static JMenu panMenu = new JMenu("Pan");

    public static JMenu zoomMenu = new JMenu("Zoom");

    public static JMenu centerInterchangeMenu = new JMenu("Center Diamond Interchange");

    public static JMenu helpMenu = new JMenu("Help");

    public static JMenu openMenu = new JMenu("Open");

    // public static JMenu saveAsMenu = new JMenu ("Save As" );
    public static JMenu imageMenu = new JMenu("Image File");

    public static JMenuItem fileMenuCommandsMenuItem = new JMenuItem("File Menu Commands", 'M');

    public static JMenuItem viewMenuCommandsMenuItem = new JMenuItem("View Menu Commands", 'M');

    public static JMenuItem pressF1MenuItem = new JMenuItem("Special key actions", 'S');

    public static JMenuItem aboutMenuItem = new JMenuItem("About gdvsim", 'A');

    public static JMenuItem StandardIntersectionMenuItem = new JMenuItem("Standard Intersection", 'T');

    public static JMenuItem DiamondInterchangeMenuItem = new JMenuItem("Diamond Interchange", 'D');

    public static JMenuItem activateMenuItem = new JMenuItem("Activate", 'V');

    public static JMenuItem newMenuItem = new JMenuItem("New", 'N');

    public static JMenuItem saveMenuItem = new JMenuItem("Save", 'S');

    public static JMenuItem exitMenuItem = new JMenuItem("Exit", 'E');

    public static JMenuItem existingFileMenuItem = new JMenuItem("Existing File", 'E');

    public static JMenuItem libraryMenuItem = new JMenuItem("Permanent Library", 'P');

    public static JMenuItem dataMenuItem = new JMenuItem("Data", 'D');

    public static JMenuItem printoutMenuItem = new JMenuItem("Printout", 'P');

    public static JMenuItem DXFMenuItem = new JMenuItem("DXF", 'X');

    public static JMenuItem attachImageFileMenuItem = new JMenuItem("Attach Image File ...", 'A');

    public static JMenuItem detachImageFileMenuItem = new JMenuItem("Detach Image File", 'D');

    public static JMenuItem up_1_8_MenuItem = new JMenuItem("Up 1/8");

    public static JMenuItem up_1_4_MenuItem = new JMenuItem("Up 1/4");

    public static JMenuItem up_1_2_MenuItem = new JMenuItem("Up 1/2");

    public static JMenuItem down_1_8_MenuItem = new JMenuItem("Down 1/8");

    public static JMenuItem down_1_4_MenuItem = new JMenuItem("Down 1/4");

    public static JMenuItem down_1_2_MenuItem = new JMenuItem("Down 1/2");

    public static JMenuItem left_1_8_MenuItem = new JMenuItem("Left 1/8");

    public static JMenuItem left_1_4_MenuItem = new JMenuItem("Left 1/4");

    public static JMenuItem left_1_2_MenuItem = new JMenuItem("Left 1/2");

    public static JMenuItem right_1_8_MenuItem = new JMenuItem("Right 1/8");

    public static JMenuItem right_1_4_MenuItem = new JMenuItem("Right 1/4");

    public static JMenuItem right_1_2_MenuItem = new JMenuItem("Right 1/2");

    public static JMenuItem zoomIn_2X_MenuItem = new JMenuItem("Zoom In 2X");

    public static JMenuItem zoomIn_4X_MenuItem = new JMenuItem("Zoom In 4X");

    public static JMenuItem zoomOut_2X_MenuItem = new JMenuItem("Zoom Out 2X");

    public static JMenuItem zoomOut_4X_MenuItem = new JMenuItem("Zoom Out 4X");

    public static JMenuItem zoomToMenuItem = new JMenuItem("Zoom To ...");

    public static JMenuItem zoomAreaMenuItem = new JMenuItem("Zoom Area ...");

    public static JMenuItem resetMenuItem = new JMenuItem("Reset Pan and Zoom", 'R');

    public static JMenuItem leftCenterMenuItem = new JMenuItem("Left Intersection", 'L');

    public static JMenuItem rightCenterMenuItem = new JMenuItem("Right Intersection", 'R');

    public static JMenuItem centerIntersectionMenuItem = new JMenuItem("Center Intersection", 'C');

    public static JMenuItem centerInterchangeMenuItem = new JMenuItem("Center Diamond Interchange", 'C');

    public static JCheckBoxMenuItem viewAttachedImage = new JCheckBoxMenuItem("View Attached Image", gclv_inter.mbov_view_attached_image);

    public static JCheckBoxMenuItem viewDetectorsJCheckBoxMenuItem = new JCheckBoxMenuItem("View Detectors", gclv_inter.mbov_view_detectors);

    public static JCheckBoxMenuItem viewLaneControlJCheckBoxMenuItem = new JCheckBoxMenuItem("View Lane Control", gclv_inter.mbov_view_lane_control);

    public static JCheckBoxMenuItem viewSDRsJCheckBoxMenuItem = new JCheckBoxMenuItem("View Sight Distance Restrictions", gclv_inter.mbov_view_sdrs);

    public static JCheckBoxMenuItem viewTurnMovementsJCheckBoxMenuItem = new JCheckBoxMenuItem("View Turn Movements", gclv_inter.mbov_view_turn_movement);

    public static JCheckBoxMenuItem viewUserArcsJCheckBoxMenuItem = new JCheckBoxMenuItem("View User Arcs", gclv_inter.mbov_view_user_arcs);

    public static JCheckBoxMenuItem viewUserLinesJCheckBoxMenuItem = new JCheckBoxMenuItem("View User Lines", gclv_inter.mbov_view_user_lines);

    public static JButton intersectionTypeButton = new JButton();

    public static JButton gdvDataButton = new JButton();

    public static JButton leg1Button = new JButton("Leg 1");

    public static JButton leg2Button = new JButton("Leg 2");

    public static JButton leg3Button = new JButton("Leg 3");

    public static JButton leg4Button = new JButton("Leg 4");

    public static JButton leg5Button = new JButton("Leg 5");

    public static JButton leg6Button = new JButton("Leg 6");

    public static JButton simulationButton = new JButton("Simulation Data");

    public static JButton laneControlButton = new JButton("Lane Control");

    public static JButton intersectionControlButton = new JButton();

    public static JButton greenIntervalSequenceButton = new JButton("Green Interval Sequence");

    public static JButton detectorButton = new JButton("Detectors");

    public static boolean flag_library = false;

    public static boolean flag_save = false;

    public static boolean flag_data = false;

    public static boolean flag_printout = false;

    public static boolean flag_DXF = false;

    public static boolean flag_OpenExistGeoFile = false;

    public static boolean flag_OpenExistSimFile = false;

    public static boolean flag_OpenNewFile = true;

    public static boolean flag_arc_ok = false;

    public static boolean flag_line_ok = false;

    public static boolean flag_drvVehByVeh_ok = false;

    public static boolean flag_drvVehByDrv_ok = false;

    public static boolean flag_plotOpt_ok = false;

    public static boolean flag_specialVehicle_ok = false;

    public static boolean flag_driverMixData_ok = false;

    public static boolean flag_driverClass_ok = false;

    public static boolean flag_vehicleClass_ok = false;

    public static boolean flag_VMSmesg_ok = false;

    public static boolean flag_simulationData_ok = false;

    public static boolean flag_laneControl_ok = false;

    public static boolean flag_greenSequence_ok = false;

    public static boolean flag_pretimed_ok = false;

    public static boolean flag_semiact_ok = false;

    public static boolean flag_fullact_ok = false;

    public static boolean flag_nema_ok = false;

    public static boolean flag_diamondTiming_ok = false;

    public static boolean flag_diamondInterval_ok = false;

    public static boolean flag_diamondOption_ok = false;

    public static boolean flag_detDataForNonDiamond_ok = false;

    public static boolean flag_detDataForDiamond_ok = false;

    public static boolean flag_detDataForTexdia_ok = false;

    public static boolean flag_detConnForNema_ok = false;

    public static boolean flag_detConnForNonNema_ok = false;

    public static boolean flag_nemaOverlap_ok = false;

    public static boolean flag_nemaMovement_ok = false;

    public static boolean flag_nemaRingGroup_ok = false;

    public static int SDR_count[] = { 0, 0, 0, 0, 0, 0 };

    public static int SDR_legnumber[][] = { { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 } };

    public static int SDR_setback[][] = { { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 } };

    public static int SDR_offset[][] = { { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 } };

    public static int[][] overlapValue = new int[PARAMS.TEXAS_MODEL_NON + 1][PARAMS.TEXAS_MODEL_NPN + 1];

    public static int[][] overlapStat = new int[PARAMS.TEXAS_MODEL_NON + 1][PARAMS.TEXAS_MODEL_NPN + 1];

    public static int numOfOverlap;

    public static int numOfOverlapStat;

    public static int[][] movementValue = new int[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NMP + 1];

    public static int[][] movementStat = new int[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NMP + 1];

    public static int[] numOfMovementValue = new int[PARAMS.TEXAS_MODEL_NPN + 1];

    public static int[] numOfMovementStat = new int[PARAMS.TEXAS_MODEL_NPN + 1];

    public static int[][][] ringGroupValue = new int[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRGP + 1];

    public static int[][][] ringGroupStat = new int[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRGP + 1];

    public static int[][] ringGroupNumOfPhasesValue = new int[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1];

    public static int[][] ringGroupNumOfPhasesStat = new int[PARAMS.TEXAS_MODEL_NRG + 1][PARAMS.TEXAS_MODEL_NRG + 1];

    public static int ringGroupNumOfRingValue;

    public static int ringGroupNumOfRingStat;

    public static int ringGroupNumOfGroupValue;

    public static int ringGroupNumOfGroupStat;

    public static int[][] detConnNemaValue = new int[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NPL + 1];

    public static String[][] detConnNemaType = new String[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NPL + 1];

    public static int[][] detConnNemaVStat = new int[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NPL + 1];

    public static int[][] detConnNemaTStat = new int[PARAMS.TEXAS_MODEL_NPN + 1][PARAMS.TEXAS_MODEL_NPL + 1];

    public static int[][] detConnNonNemaValue = new int[PARAMS.TEXAS_MODEL_NPH + 1][PARAMS.TEXAS_MODEL_NPL + 1];

    public static int[][] detConnNonNemaStat = new int[PARAMS.TEXAS_MODEL_NPH + 1][PARAMS.TEXAS_MODEL_NPL + 1];

    public static String[] detConnTypeNonNemaValue = new String[PARAMS.TEXAS_MODEL_NPH + 1];

    public static int[] detConnTypeNonNemaStat = new int[PARAMS.TEXAS_MODEL_NPH + 1];

    public static String[][][] greenSeqValue = new String[PARAMS.TEXAS_MODEL_NGI + 1][PARAMS.TEXAS_MODEL_NIA][PARAMS.TEXAS_MODEL_NAL + 1];

    public static int[][][] greenSeqStat = new int[PARAMS.TEXAS_MODEL_NGI + 1][PARAMS.TEXAS_MODEL_NIA][PARAMS.TEXAS_MODEL_NAL + 1];

    public static int[] det_classify_num = new int[PARAMS.TEXAS_MODEL_NLS + 1];

    public static String[][] det_classify_name = new String[PARAMS.TEXAS_MODEL_NLS + 1][PARAMS.TEXAS_MODEL_LDC + 1];

    public static int[][] det_classify_lower = new int[PARAMS.TEXAS_MODEL_NLS + 1][PARAMS.TEXAS_MODEL_LDC + 1];

    public static int[][] det_classify_upper = new int[PARAMS.TEXAS_MODEL_NLS + 1][PARAMS.TEXAS_MODEL_LDC + 1];

    public static int[] det_classify_num_stat = new int[PARAMS.TEXAS_MODEL_NLS + 1];

    public static int[][] det_classify_name_stat = new int[PARAMS.TEXAS_MODEL_NLS + 1][PARAMS.TEXAS_MODEL_LDC + 1];

    public static int[][] det_classify_lower_stat = new int[PARAMS.TEXAS_MODEL_NLS + 1][PARAMS.TEXAS_MODEL_LDC + 1];

    public static int[][] det_classify_upper_stat = new int[PARAMS.TEXAS_MODEL_NLS + 1][PARAMS.TEXAS_MODEL_LDC + 1];

    public static int[] veh_unit_num = new int[PARAMS.TEXAS_MODEL_NVC + 1];

    public static int[][] veh_unit_length = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_unit_width = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_unit_draw_seq = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_unit_fpd = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_unit_rpd = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_unit_rhpd = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_trans_length = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_trans_width = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[] veh_unit_num_stat = new int[PARAMS.TEXAS_MODEL_NVC + 1];

    public static int[][] veh_unit_length_stat = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_unit_width_stat = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_unit_draw_seq_stat = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_unit_fpd_stat = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_unit_rpd_stat = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_unit_rhpd_stat = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_trans_length_stat = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static int[][] veh_trans_width_stat = new int[PARAMS.TEXAS_MODEL_NVC + 1][PARAMS.TEXAS_MODEL_MNU + 1];

    public static double[][][] mcla_var_traf_period_value = new double[PARAMS.TEXAS_MODEL_NLGP1][PARAMS.TEXAS_MODEL_NVT + 1][PARAMS.TEXAS_MODEL_NVC + 1];

    public static int[][][] mcla_var_traf_period_stat = new int[PARAMS.TEXAS_MODEL_NLGP1][PARAMS.TEXAS_MODEL_NVT + 1][PARAMS.TEXAS_MODEL_NVC + 1];

    public static double currZoom = 1.0;

    public static double imgCurrZoom = 1.0;

    public static double curr_screenXShift = 0.0;

    public static double curr_screenYShift = 0.0;

    public static Rectangle2D.Double zoomRect = null;

    public static Line2D.Double[] lineConnect = null;

    public static int[] x_point = null;

    public static int[] y_point = null;

    public static boolean mvImg = true;

    public static boolean rotImg = false;

    public static boolean rotIntsct = false;

    public static double rotAng = 0.0;

    public static boolean lstSquaresMode = false;

    public static int lstSquaresPoints = 1;

    public static int a = 0;

    public static int blank = 0;

    public static int x1 = 0;

    public static int x2 = 0;

    public static int y1 = 0;

    public static int y2 = 0;

    int startX = 0;

    int startY = 0;

    int deltaX = 0;

    int deltaY = 0;

    int delta = 0;

    boolean startBox = false;

    double aspectRatio = 1.0;

    MouseMotionAdapter mma;

    MouseAdapter ma;

    int leg_number;

    String ParamGDVfile;

    String ParamSIMfile;

    AttachImageFileListener attachImageFileListener;

    DetachImageFileListener detachImageFileListener;

    ComponentActionListener componentActionListener;

    ComponentKeyListener componentKeyListener;

    HelpListener helpListener;

    ViewListener viewListener;

    CloseWindowListener closeWindowListener;

    public gdvsim(String argsOne, String argsTwo) {
        ParamGDVfile = argsOne;
        ParamSIMfile = argsTwo;

        mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gdvsim.gclv_inter.IVERSN);
        mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        Container container;
        JPanel wholePanel = new JPanel();
        container = wholePanel;
        JScrollPane scrollpane = new JScrollPane(wholePanel);
        scrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        mainFrame.getContentPane().add(scrollpane);

        mainFrameMenuBar.add(fileMenu);
        mainFrameMenuBar.add(viewMenu);
        mainFrameMenuBar.add(helpMenu);
        helpMenu.add(fileMenuCommandsMenuItem);
        helpMenu.add(viewMenuCommandsMenuItem);
        helpMenu.add(pressF1MenuItem);
        helpMenu.add(StandardIntersectionMenuItem);
        helpMenu.add(DiamondInterchangeMenuItem);
        helpMenu.add(aboutMenuItem);
        fileMenu.add(activateMenuItem);
        fileMenu.add(newMenuItem);
        fileMenu.add(openMenu);
        fileMenu.add(saveMenuItem);
        // fileMenu .add(saveAsMenu );
        fileMenu.addSeparator();
        fileMenu.add(exitMenuItem);
        viewMenu.add(panMenu);
        viewMenu.add(zoomMenu);
        viewMenu.add(resetMenuItem);
        viewMenu.add(viewAttachedImage);
        viewMenu.add(viewDetectorsJCheckBoxMenuItem);
        viewMenu.add(viewLaneControlJCheckBoxMenuItem);
        viewMenu.add(viewSDRsJCheckBoxMenuItem);
        viewMenu.add(viewTurnMovementsJCheckBoxMenuItem);
        viewMenu.add(viewUserArcsJCheckBoxMenuItem);
        viewMenu.add(viewUserLinesJCheckBoxMenuItem);
        // saveAsMenu .add(dataMenuItem );
        // saveAsMenu .add(printoutMenuItem );
        // saveAsMenu .add(DXFMenuItem );
        openMenu.add(libraryMenuItem);
        openMenu.add(existingFileMenuItem);
        imageMenu.add(attachImageFileMenuItem);
        imageMenu.add(detachImageFileMenuItem);
        panMenu.add(up_1_8_MenuItem);
        panMenu.add(up_1_4_MenuItem);
        panMenu.add(up_1_2_MenuItem);
        panMenu.add(down_1_8_MenuItem);
        panMenu.add(down_1_4_MenuItem);
        panMenu.add(down_1_2_MenuItem);
        panMenu.add(left_1_8_MenuItem);
        panMenu.add(left_1_4_MenuItem);
        panMenu.add(left_1_2_MenuItem);
        panMenu.add(right_1_8_MenuItem);
        panMenu.add(right_1_4_MenuItem);
        panMenu.add(right_1_2_MenuItem);
        zoomMenu.add(zoomToMenuItem);
        zoomMenu.add(zoomIn_2X_MenuItem);
        zoomMenu.add(zoomIn_4X_MenuItem);
        zoomMenu.add(zoomOut_2X_MenuItem);
        zoomMenu.add(zoomOut_4X_MenuItem);
        zoomMenu.add(zoomAreaMenuItem);
        mainFrameToolBar.add(intersectionTypeButton);
        mainFrameToolBar.add(gdvDataButton);
        mainFrameToolBar.add(leg1Button);
        mainFrameToolBar.add(leg2Button);
        mainFrameToolBar.add(leg3Button);
        mainFrameToolBar.add(leg4Button);
        mainFrameToolBar.add(leg5Button);
        mainFrameToolBar.add(leg6Button);
        mainFrameToolBar.add(simulationButton);
        mainFrameToolBar.add(laneControlButton);
        mainFrameToolBar.add(intersectionControlButton);
        mainFrameToolBar.add(greenIntervalSequenceButton);
        mainFrameToolBar.add(detectorButton);

        fileMenu.setMnemonic(KeyEvent.VK_F);
        viewMenu.setMnemonic(KeyEvent.VK_V);
        centerInterchangeMenu.setMnemonic(KeyEvent.VK_C);
        helpMenu.setMnemonic(KeyEvent.VK_H);
        openMenu.setMnemonic(KeyEvent.VK_O);
        // saveAsMenu .setMnemonic(KeyEvent.VK_A);
        imageMenu.setMnemonic(KeyEvent.VK_I);
        panMenu.setMnemonic(KeyEvent.VK_P);
        zoomMenu.setMnemonic(KeyEvent.VK_Z);
        intersectionTypeButton.setMnemonic(KeyEvent.VK_I);
        gdvDataButton.setMnemonic(KeyEvent.VK_G);
        leg1Button.setMnemonic(KeyEvent.VK_1);
        leg2Button.setMnemonic(KeyEvent.VK_2);
        leg3Button.setMnemonic(KeyEvent.VK_3);
        leg4Button.setMnemonic(KeyEvent.VK_4);
        leg5Button.setMnemonic(KeyEvent.VK_5);
        leg6Button.setMnemonic(KeyEvent.VK_6);
        simulationButton.setMnemonic(KeyEvent.VK_S);
        laneControlButton.setMnemonic(KeyEvent.VK_L);
        intersectionControlButton.setMnemonic(KeyEvent.VK_T);
        greenIntervalSequenceButton.setMnemonic(KeyEvent.VK_R);
        detectorButton.setMnemonic(KeyEvent.VK_D);

        // saveAsMenu.setDisplayedMnemonicIndex(5);

        detachImageFileMenuItem.setVisible(false);
        intersectionTypeButton.setVisible(false);
        gdvDataButton.setVisible(false);
        leg1Button.setVisible(false);
        leg2Button.setVisible(false);
        leg3Button.setVisible(false);
        leg4Button.setVisible(false);
        leg5Button.setVisible(false);
        leg6Button.setVisible(false);
        simulationButton.setVisible(false);
        laneControlButton.setVisible(false);
        intersectionControlButton.setVisible(false);
        greenIntervalSequenceButton.setVisible(false);
        detectorButton.setVisible(false);

        attachImageFileListener = new AttachImageFileListener();
        detachImageFileListener = new DetachImageFileListener();
        componentActionListener = new ComponentActionListener();
        componentKeyListener = new ComponentKeyListener();
        helpListener = new HelpListener();
        viewListener = new ViewListener();

        attachImageFileMenuItem.addActionListener(attachImageFileListener);
        detachImageFileMenuItem.addActionListener(detachImageFileListener);

        intersectionTypeButton.addActionListener(componentActionListener);
        gdvDataButton.addActionListener(componentActionListener);
        leg1Button.addActionListener(componentActionListener);
        leg2Button.addActionListener(componentActionListener);
        leg3Button.addActionListener(componentActionListener);
        leg4Button.addActionListener(componentActionListener);
        leg5Button.addActionListener(componentActionListener);
        leg6Button.addActionListener(componentActionListener);
        simulationButton.addActionListener(componentActionListener);
        laneControlButton.addActionListener(componentActionListener);
        intersectionControlButton.addActionListener(componentActionListener);
        detectorButton.addActionListener(componentActionListener);
        greenIntervalSequenceButton.addActionListener(componentActionListener);
        fileMenuCommandsMenuItem.addActionListener(componentActionListener);
        viewMenuCommandsMenuItem.addActionListener(componentActionListener);
        pressF1MenuItem.addActionListener(componentActionListener);
        aboutMenuItem.addActionListener(componentActionListener);
        StandardIntersectionMenuItem.addActionListener(componentActionListener);
        DiamondInterchangeMenuItem.addActionListener(componentActionListener);
        activateMenuItem.addActionListener(componentActionListener);
        newMenuItem.addActionListener(componentActionListener);
        saveMenuItem.addActionListener(componentActionListener);
        exitMenuItem.addActionListener(componentActionListener);
        libraryMenuItem.addActionListener(componentActionListener);
        existingFileMenuItem.addActionListener(componentActionListener);
        dataMenuItem.addActionListener(componentActionListener);
        printoutMenuItem.addActionListener(componentActionListener);
        DXFMenuItem.addActionListener(componentActionListener);

        intersectionTypeButton.addKeyListener(componentKeyListener);
        gdvDataButton.addKeyListener(componentKeyListener);
        leg1Button.addKeyListener(componentKeyListener);
        leg2Button.addKeyListener(componentKeyListener);
        leg3Button.addKeyListener(componentKeyListener);
        leg4Button.addKeyListener(componentKeyListener);
        leg5Button.addKeyListener(componentKeyListener);
        leg6Button.addKeyListener(componentKeyListener);
        simulationButton.addKeyListener(componentKeyListener);
        laneControlButton.addKeyListener(componentKeyListener);
        intersectionControlButton.addKeyListener(componentKeyListener);
        detectorButton.addKeyListener(componentKeyListener);
        greenIntervalSequenceButton.addKeyListener(componentKeyListener);
        fileMenuCommandsMenuItem.addKeyListener(componentKeyListener);
        viewMenuCommandsMenuItem.addKeyListener(componentKeyListener);
        pressF1MenuItem.addKeyListener(componentKeyListener);
        aboutMenuItem.addKeyListener(componentKeyListener);
        StandardIntersectionMenuItem.addKeyListener(componentKeyListener);
        DiamondInterchangeMenuItem.addKeyListener(componentKeyListener);
        activateMenuItem.addKeyListener(componentKeyListener);
        newMenuItem.addKeyListener(componentKeyListener);
        saveMenuItem.addKeyListener(componentKeyListener);
        exitMenuItem.addKeyListener(componentKeyListener);
        libraryMenuItem.addKeyListener(componentKeyListener);
        existingFileMenuItem.addKeyListener(componentKeyListener);
        dataMenuItem.addKeyListener(componentKeyListener);
        printoutMenuItem.addKeyListener(componentKeyListener);
        DXFMenuItem.addKeyListener(componentKeyListener);

        intersectionTypeButton.addKeyListener(helpListener);
        gdvDataButton.addKeyListener(helpListener);
        leg1Button.addKeyListener(helpListener);
        leg2Button.addKeyListener(helpListener);
        leg3Button.addKeyListener(helpListener);
        leg4Button.addKeyListener(helpListener);
        leg5Button.addKeyListener(helpListener);
        leg6Button.addKeyListener(helpListener);
        simulationButton.addKeyListener(helpListener);
        laneControlButton.addKeyListener(helpListener);
        intersectionControlButton.addKeyListener(helpListener);
        detectorButton.addKeyListener(helpListener);
        greenIntervalSequenceButton.addKeyListener(helpListener);

        up_1_8_MenuItem.addActionListener(viewListener);
        up_1_4_MenuItem.addActionListener(viewListener);
        up_1_2_MenuItem.addActionListener(viewListener);
        down_1_8_MenuItem.addActionListener(viewListener);
        down_1_4_MenuItem.addActionListener(viewListener);
        down_1_2_MenuItem.addActionListener(viewListener);
        left_1_8_MenuItem.addActionListener(viewListener);
        left_1_4_MenuItem.addActionListener(viewListener);
        left_1_2_MenuItem.addActionListener(viewListener);
        right_1_8_MenuItem.addActionListener(viewListener);
        right_1_4_MenuItem.addActionListener(viewListener);
        right_1_2_MenuItem.addActionListener(viewListener);
        zoomIn_2X_MenuItem.addActionListener(viewListener);
        zoomIn_4X_MenuItem.addActionListener(viewListener);
        zoomOut_2X_MenuItem.addActionListener(viewListener);
        zoomOut_4X_MenuItem.addActionListener(viewListener);
        resetMenuItem.addActionListener(viewListener);
        zoomToMenuItem.addActionListener(viewListener);
        zoomAreaMenuItem.addActionListener(viewListener);
        centerIntersectionMenuItem.addActionListener(viewListener);
        centerInterchangeMenuItem.addActionListener(viewListener);
        leftCenterMenuItem.addActionListener(viewListener);
        rightCenterMenuItem.addActionListener(viewListener);
        viewAttachedImage.addActionListener(viewListener);
        viewDetectorsJCheckBoxMenuItem.addActionListener(viewListener);
        viewLaneControlJCheckBoxMenuItem.addActionListener(viewListener);
        viewSDRsJCheckBoxMenuItem.addActionListener(viewListener);
        viewTurnMovementsJCheckBoxMenuItem.addActionListener(viewListener);
        viewUserArcsJCheckBoxMenuItem.addActionListener(viewListener);
        viewUserLinesJCheckBoxMenuItem.addActionListener(viewListener);

        fileMenu.getAccessibleContext().setAccessibleDescription("The file menu in this program that has menu items");
        viewMenu.getAccessibleContext().setAccessibleDescription("The view menu in this program that has menu items");
        helpMenu.getAccessibleContext().setAccessibleDescription("The help menu in this program that has menu items");
        newMenuItem.getAccessibleContext().setAccessibleDescription("The new menu in this program that has menu items");
        viewAttachedImage.getAccessibleContext().setAccessibleDescription("This menu item is for enabling or disabling the viewing of the attached image");
        viewDetectorsJCheckBoxMenuItem.getAccessibleContext().setAccessibleDescription("This menu item is for enabling or disabling the viewing of Detectors");
        viewLaneControlJCheckBoxMenuItem.getAccessibleContext().setAccessibleDescription("This menu item is for enabling or disabling the viewing of Lane Control");
        viewSDRsJCheckBoxMenuItem.getAccessibleContext().setAccessibleDescription("This menu item is for enabling or disabling the viewing of Sight Distance Restrictions");
        viewTurnMovementsJCheckBoxMenuItem.getAccessibleContext().setAccessibleDescription("This menu item is for enabling or disabling the viewing of Turn Movemements");
        viewUserArcsJCheckBoxMenuItem.getAccessibleContext().setAccessibleDescription("This menu item is for enabling or disabling the viewing of User Arcs");
        viewUserLinesJCheckBoxMenuItem.getAccessibleContext().setAccessibleDescription("This menu item is for enabling or disabling the viewing of User Lines");

        fileMenu.getAccessibleContext().setAccessibleName("The file menu in this program that has menu items");
        viewMenu.getAccessibleContext().setAccessibleName("The view menu in this program that has menu items");
        helpMenu.getAccessibleContext().setAccessibleName("The help menu in this program that has menu items");
        newMenuItem.getAccessibleContext().setAccessibleName("The new menu in this program that has menu items");
        viewAttachedImage.getAccessibleContext().setAccessibleName("This menu item is for enabling or disabling the viewing of the attached image");
        viewDetectorsJCheckBoxMenuItem.getAccessibleContext().setAccessibleName("This menu item is for enabling or disabling the viewing of Detectors");
        viewLaneControlJCheckBoxMenuItem.getAccessibleContext().setAccessibleName("This menu item is for enabling or disabling the viewing of Lane Control");
        viewSDRsJCheckBoxMenuItem.getAccessibleContext().setAccessibleName("This menu item is for enabling or disabling the viewing of Sight Distance Restrictions");
        viewTurnMovementsJCheckBoxMenuItem.getAccessibleContext().setAccessibleName("This menu item is for enabling or disabling the viewing of Turn Movemements");
        viewUserArcsJCheckBoxMenuItem.getAccessibleContext().setAccessibleName("This menu item is for enabling or disabling the viewing of User Arcs");
        viewUserLinesJCheckBoxMenuItem.getAccessibleContext().setAccessibleName("This menu item is for enabling or disabling the viewing of User Lines");

        mainFrame.setSize(1020, 720);
        mainFrame.setJMenuBar(mainFrameMenuBar);
        mainFrame.setVisible(true);
        myIntersection.setBackground(new Color(121, 178, 129));
        container.setLayout(new BorderLayout());
        container.add(mainFrameToolBar, BorderLayout.NORTH);
        container.add(myIntersection, BorderLayout.CENTER);

        initialization();

        Toolkit tk = Toolkit.getDefaultToolkit();
        byte bogus[] = { (byte)0 };

        myIntersection.setVisible(false);
        myIntersection.setEnabled(false);

        mainFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        if (!(ParamGDVfile.equals("ParameterOne"))) {
            openFileWithCommandLineArgument();
            if (flag_OpenExistGeoFile) {
                saveMenuItem.setEnabled(true);
                myIntersection.setVisible(true);
                myIntersection.draw();
            }
        }

        closeWindowListener = new CloseWindowListener();
        mainFrame.addKeyListener(closeWindowListener);

        WindowListener windowCloseRedX = new WindowAdapter() {

            public synchronized void windowClosing(WindowEvent e) {
                gdvsim.gclv_inter.closeAll();
                mainFrame.dispose();
                System.exit(0);
            }
        };

        mainFrame.addWindowListener(windowCloseRedX);
    } // end of method gdvsim

    class CloseWindowListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ESCAPE && mainFrame != null) {
                gdvsim.gclv_inter.closeAll();
                mainFrame.dispose();
                System.exit(0);
            }

        }// end of method keyPressed
    } // end of class CloseWindowListener

    public static void initialization() {
        TX_Fmt lclv_tx_fmt;

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs = 0;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_ARC_HEADER_NUM_ARCS] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines = 0;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_LINE_HEADER_NUM_LINES] = gclv_inter.TX_SET_BY_SOFTWARE;

        lclv_tx_fmt = gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gclv_inter.TX_FMT_GDV_PLOT_OPTS];

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_path_type = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_option = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_type = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_appr = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_intr = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_max_radius_path = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_min_dist_paths = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mdfv_plot_paper_wdth = lclv_tx_fmt.mdfa_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH];

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH] = gclv_inter.TX_FROM_USER;

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix = "NO";
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX] = gclv_inter.TX_SET_BY_SOFTWARE;

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data = "NO";
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] = gclv_inter.TX_SET_BY_SOFTWARE;

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_data = "NO";
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA] = gclv_inter.TX_SET_BY_SOFTWARE;

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVC + 1); lsiv_i++) {
            gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[lsiv_i] = "";
        }

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_02] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_03] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_04] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_05] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_06] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_07] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_08] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_09] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_10] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_11] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_12] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_13] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_14] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_15] = gclv_inter.TX_SET_BY_SOFTWARE;

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVC + 1); lsiv_i++) {
            gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[lsiv_i] = "";
        }

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_2] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_3] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_4] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_5] = gclv_inter.TX_SET_BY_SOFTWARE;

        for (int lsiv_veh = 1; lsiv_veh <= PARAMS.TEXAS_MODEL_NVC; lsiv_veh++) {
            lclv_tx_fmt = gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gclv_inter.TX_FMT_GDV_DRIVER_MIX_01 - 1 + lsiv_veh];

            for (int lsiv_drv = 1; lsiv_drv <= PARAMS.TEXAS_MODEL_NDC; lsiv_drv++) {
                gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mdfa_driver_mix[lsiv_veh][lsiv_drv] = lclv_tx_fmt.mdfa_def[gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1
                        + lsiv_veh];
                gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_driver_mix.mcla_aux[lsiv_veh].msia_stat[gclv_inter.TX_FMT_GDV_DRIVER_MIX_01_DRIVERMIX_1 - 1
                        + lsiv_drv] = gclv_inter.TX_FROM_USER;
            }
        }

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVC + 1); lsiv_i++) {
            gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.msia_veh_length[lsiv_i] = 0;
        }

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_01] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_02] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_03] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_04] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_05] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_06] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_07] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_08] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_09] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_10] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_11] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_12] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_13] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_14] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_length.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_LENGTH_15] = gclv_inter.TX_SET_BY_SOFTWARE;

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVC + 1); lsiv_i++) {
            gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.msia_veh_oper_char[lsiv_i] = 0;
        }

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_01] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_02] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_03] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_04] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_05] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_06] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_07] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_08] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_09] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_10] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_11] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_12] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_13] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_14] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_OPER_CHAR_15] = gclv_inter.TX_SET_BY_SOFTWARE;

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVC + 1); lsiv_i++) {
            gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.msia_veh_max_decel[lsiv_i] = 0;
        }

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_01] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_02] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_03] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_04] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_05] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_06] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_07] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_08] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_09] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_10] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_11] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_12] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_13] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_14] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_decel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_DECEL_15] = gclv_inter.TX_SET_BY_SOFTWARE;

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVC + 1); lsiv_i++) {
            gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.msia_veh_max_accel[lsiv_i] = 0;
        }

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_01] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_02] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_03] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_04] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_05] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_06] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_07] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_08] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_09] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_10] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_11] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_12] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_13] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_14] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_accel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_ACCEL_15] = gclv_inter.TX_SET_BY_SOFTWARE;

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVC + 1); lsiv_i++) {
            gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.msia_veh_max_vel[lsiv_i] = 0;
        }

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_01] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_02] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_03] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_04] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_05] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_06] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_07] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_08] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_09] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_10] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_11] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_12] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_13] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_14] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_max_vel.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MAX_VEL_15] = gclv_inter.TX_SET_BY_SOFTWARE;

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NVC + 1); lsiv_i++) {
            gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.msia_veh_min_rad[lsiv_i] = 0;
        }

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_01] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_02] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_03] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_04] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_05] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_06] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_07] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_08] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_09] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_10] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_11] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_12] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_13] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_14] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_veh_min_rad.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_VEH_MIN_RAD_15] = gclv_inter.TX_SET_BY_SOFTWARE;

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NDC + 1); lsiv_i++) {
            gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_oper_char.msia_drv_oper_char[lsiv_i] = 0;
        }

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_1] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_2] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_3] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_4] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_oper_char.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_DRV_OPER_CHAR_5] = gclv_inter.TX_SET_BY_SOFTWARE;

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NDC + 1); lsiv_i++) {
            gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_PIJR_time.mdfv_drv_PIJR_time[lsiv_i] = 0;
        }

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_PIJR_time.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_1] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_PIJR_time.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_2] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_PIJR_time.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_3] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_PIJR_time.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_4] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_drv_PIJR_time.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_DRV_PIJR_TIME_5] = gclv_inter.TX_SET_BY_SOFTWARE;

        lclv_tx_fmt = gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF];

        for (int lsiv_olp = 1; lsiv_olp <= PARAMS.TEXAS_MODEL_NON; lsiv_olp++) {
            for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
                overlapStat[lsiv_olp][lsiv_phase] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_olp].mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01
                        - 1 + lsiv_phase];
                overlapValue[lsiv_olp][lsiv_phase] = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1 + lsiv_phase];
            }
        }

        numOfOverlap = 0;
        numOfOverlapStat = gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV];

        lclv_tx_fmt = gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT];

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            numOfMovementStat[lsiv_phase] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1
                    + lsiv_phase];
            numOfMovementValue[lsiv_phase] = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_NUM_PH01 - 1 + lsiv_phase];

            for (int lsiv_mvt = 1; lsiv_mvt <= PARAMS.TEXAS_MODEL_NMP; lsiv_mvt++) {
                movementStat[lsiv_phase][lsiv_mvt] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_movement.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1
                        - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP];
                movementValue[lsiv_phase][lsiv_mvt] = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_SIM_NEMA_MOVEMENT_PH01_MV1 - 1 + lsiv_mvt + (lsiv_phase - 1) * PARAMS.TEXAS_MODEL_NMP];
            }
        } // end of for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++)

        lclv_tx_fmt = gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gclv_inter.TX_FMT_SIM_NEMA_RING_GRP];

        ringGroupNumOfRingStat = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_RINGS];
        ringGroupNumOfRingValue = 0;

        ringGroupNumOfGroupStat = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NUM_GRPS];
        ringGroupNumOfGroupValue = 0;

        for (int lsiv_ring = 1; lsiv_ring <= PARAMS.TEXAS_MODEL_NRG; lsiv_ring++) {
            for (int lsiv_group = 1; lsiv_group <= PARAMS.TEXAS_MODEL_NRG; lsiv_group++) {
                ringGroupNumOfPhasesStat[lsiv_ring][lsiv_group] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_NPH_R1G1
                        - 1 + lsiv_group + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG];
                ringGroupNumOfPhasesValue[lsiv_ring][lsiv_group] = 1;

                for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NRGP; lsiv_phase++) {
                    ringGroupStat[lsiv_ring][lsiv_group][lsiv_phase] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ring_group.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01
                            - 1 + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP];
                    ringGroupValue[lsiv_ring][lsiv_group][lsiv_phase] = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_SIM_NEMA_RING_GRP_R1G1_P01 - 1 + lsiv_phase + (lsiv_group - 1) * PARAMS.TEXAS_MODEL_NRGP
                            + (lsiv_ring - 1) * PARAMS.TEXAS_MODEL_NRG * PARAMS.TEXAS_MODEL_NRGP];
                }
            }
        }

        lclv_tx_fmt = gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gclv_inter.TX_FMT_SIM_DETECT_CONN2H];

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                detConnNemaVStat[lsiv_phase][lsiv_det] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_DETECT_CONN2H_DET_01
                        - 1 + lsiv_det];
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                if (gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_DETECT_CONN2H_DET_01 - 1
                        + lsiv_det] != gclv_inter.TX_DATA_IS_INVALID) {
                    detConnNemaValue[lsiv_phase][lsiv_det] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det];
                }
                else {
                    detConnNemaValue[lsiv_phase][lsiv_det] = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_SIM_DETECT_CONN2H_DET_01 - 1 + lsiv_det];
                }

                if (gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01 - 1
                        + lsiv_det] != gclv_inter.TX_DATA_IS_INVALID) {
                    detConnNemaType[lsiv_phase][lsiv_det] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[lsiv_det];
                }
                else {
                    detConnNemaType[lsiv_phase][lsiv_det] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01 - 1 + lsiv_det];
                }
            }
        }

        lclv_tx_fmt = gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gclv_inter.TX_FMT_SIM_DETECT_CONSFA];

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            detConnTypeNonNemaStat[lsiv_phase] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE
                    - 1 + lsiv_phase];

            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                detConnNonNemaStat[lsiv_phase][lsiv_det] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01
                        - 1 + lsiv_det];
            }
        }

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPH; lsiv_phase++) {
            if (gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE - 1
                    + lsiv_phase] != gclv_inter.TX_DATA_IS_INVALID) {
                detConnTypeNonNemaValue[lsiv_phase] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mstv_conn_type;
            }
            else {
                detConnTypeNonNemaValue[lsiv_phase] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE - 1 + lsiv_phase];
            }

            for (int lsiv_det = 1; lsiv_det <= PARAMS.TEXAS_MODEL_NPL; lsiv_det++) {
                if (gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1
                        + lsiv_det] != gclv_inter.TX_DATA_IS_INVALID) {
                    detConnNonNemaValue[lsiv_phase][lsiv_det] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[lsiv_det];
                }
                else {
                    detConnNonNemaValue[lsiv_phase][lsiv_det] = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1 + lsiv_det];
                }
            }
        }

        String[][][] greenSeqDefault = new String[PARAMS.TEXAS_MODEL_NGI + 1][PARAMS.TEXAS_MODEL_NIA][PARAMS.TEXAS_MODEL_NAL + 1];

        for (int lsiv_phase = 1; lsiv_phase <= PARAMS.TEXAS_MODEL_NPN; lsiv_phase++) {
            for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    String defaultValue = " ";

                    if (gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0 - 1
                            + lsiv_lane] != gclv_inter.TX_DATA_IS_INVALID) {
                        String lc = " ";
                        lc = gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont;

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

                    if (gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_LANE_CONT_CONT_0 - 1
                            + lsiv_lane] != gclv_inter.TX_DATA_IS_INVALID) {
                        String lc = " ";
                        lc = gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont;

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
                    greenSeqStat[lsiv_phase][lsiv_leg][lsiv_lane] = gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01
                            - 1 + lsiv_phase];

                    if (gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1
                            + lsiv_phase] != gclv_inter.TX_DATA_IS_INVALID) {
                        greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane] = gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01
                                - 1 + lsiv_phase];
                    }
                    else {
                        greenSeqValue[lsiv_phase][lsiv_leg][lsiv_lane] = greenSeqDefault[lsiv_phase][lsiv_leg][lsiv_lane];
                    }
                }
            }
        }

        for (int lsiv_overlap = 1; lsiv_overlap <= PARAMS.TEXAS_MODEL_NON; lsiv_overlap++) {
            for (int lsiv_leg = 0; lsiv_leg < PARAMS.TEXAS_MODEL_NIA; lsiv_leg++) {
                for (int lsiv_lane = 1; lsiv_lane <= PARAMS.TEXAS_MODEL_NAL; lsiv_lane++) {
                    greenSeqStat[PARAMS.TEXAS_MODEL_NPN
                            + lsiv_overlap][lsiv_leg][lsiv_lane] = gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A
                                    - 1 + lsiv_overlap];
                    if (gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A - 1
                            + lsiv_overlap] != gclv_inter.TX_DATA_IS_INVALID) {
                        greenSeqValue[PARAMS.TEXAS_MODEL_NPN
                                + lsiv_overlap][lsiv_leg][lsiv_lane] = gclv_inter.mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[gclv_inter.TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A
                                        - 1 + lsiv_overlap];
                    }
                    else {
                        greenSeqValue[PARAMS.TEXAS_MODEL_NPN + lsiv_overlap][lsiv_leg][lsiv_lane] = greenSeqDefault[PARAMS.TEXAS_MODEL_NPN + lsiv_overlap][lsiv_leg][lsiv_lane];
                    }
                }
            }
        }
    }

    public static void windowAfterOpenFile() {
        if (gclv_inter.mbov_is_diamond_interchange) {
            intersectionTypeButton.setText("Diamond Interchange");
            gdvDataButton.setText("Diamond GDV Data");
            gdvDataButton.setMnemonic(KeyEvent.VK_D);
            viewMenu.remove(centerIntersectionMenuItem);
            centerInterchangeMenu.add(leftCenterMenuItem);
            centerInterchangeMenu.add(rightCenterMenuItem);
            viewMenu.add(centerInterchangeMenu);
        }
        else {
            intersectionTypeButton.setText("Standard Intersection");
            gdvDataButton.setText("GDV Data");
            gdvDataButton.setMnemonic(KeyEvent.VK_G);
            viewMenu.remove(centerInterchangeMenu);
            viewMenu.add(centerIntersectionMenuItem);
        }

        intersectionTypeButton.setVisible(true);
        intersectionTypeButton.setEnabled(false);
        gdvDataButton.setVisible(true);

        if (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs == 3) {
            leg1Button.setVisible(true);
            leg2Button.setVisible(true);
            leg3Button.setVisible(true);
            leg4Button.setVisible(false);
            leg5Button.setVisible(false);
            leg6Button.setVisible(false);
        }

        if (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs == 4) {
            leg1Button.setVisible(true);
            leg2Button.setVisible(true);
            leg3Button.setVisible(true);
            leg4Button.setVisible(true);
            leg5Button.setVisible(false);
            leg6Button.setVisible(false);
        }

        if (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs == 5) {
            leg1Button.setVisible(true);
            leg2Button.setVisible(true);
            leg3Button.setVisible(true);
            leg4Button.setVisible(true);
            leg5Button.setVisible(true);
            leg6Button.setVisible(false);
        }

        if (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs == 6) {
            leg1Button.setVisible(true);
            leg2Button.setVisible(true);
            leg3Button.setVisible(true);
            leg4Button.setVisible(true);
            leg5Button.setVisible(true);
            leg6Button.setVisible(true);
        }

        simulationButton.setVisible(true);
        laneControlButton.setVisible(false);
        intersectionControlButton.setVisible(false);
        greenIntervalSequenceButton.setVisible(false);
        detectorButton.setVisible(false);

        gdvDataButton.requestFocus(true);

    } // end of windowAfterOpenFile()

    class HelpListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_F1) {
                if (event.getSource() == intersectionTypeButton) {
                    new HelpDialog(true, "Intersection Type Button", "Intersection Type Button opens Intersection Type Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == gdvDataButton) {
                    new HelpDialog(true, "GDV Data Button", "GDV Data Button opens GDV Data Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == leg1Button) {
                    new HelpDialog(true, "Leg 1 Button", "Leg 1 Button opens Leg 1 Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == leg2Button) {
                    new HelpDialog(true, "Leg 2 Button", "Leg 2 Button opens Leg 2 Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == leg3Button) {
                    new HelpDialog(true, "Leg 3 Button", "Leg 3 Button opens Leg 3 Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == leg4Button) {
                    new HelpDialog(true, "Leg 4 Button", "Leg 4 Button opens Leg 4 Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == leg5Button) {
                    new HelpDialog(true, "Leg 5 Button", "Leg 5 Button opens Leg 5 Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == leg6Button) {
                    new HelpDialog(true, "Leg 6 Button", "Leg 6 Button opens Leg 6 Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == simulationButton) {
                    new HelpDialog(true, "Simulation Button", "Simulation Button opens Simulation Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == laneControlButton) {
                    new HelpDialog(true, "Lane Control Button", "Lane Control Button opens Lane Control Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == greenIntervalSequenceButton) {
                    new HelpDialog(true, "Green Interval Sequence Button", "Green Interval Sequence Button opens Green Interval Sequence Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == detectorButton) {
                    new HelpDialog(true, "Detector Button", "Detector Button opens Detector Dialog.", " ", " ", " ", " ", " ", " ", " ");
                }
                else if (event.getSource() == intersectionControlButton) {
                    if (gdvsim.gclv_inter.mbov_is_ic_PRETIMED) {
                        new HelpDialog(true, "PRETIMED Button", "PRETIMED Button opens PRETIMED Dialog.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT) {
                        new HelpDialog(true, "SEMI-ACT Button", "SEMI-ACT Button opens SEMI-ACT Dialog.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (gdvsim.gclv_inter.mbov_is_ic_FULL_ACT) {
                        new HelpDialog(true, "FULL-ACT Button", "FULL-ACT Button opens FULL-ACT Dialog.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (gdvsim.gclv_inter.mbov_is_ic_TEX_DIA) {
                        new HelpDialog(true, "TEX-DIA Button", "TEX-DIA Button opens TEX-DIA Dialog.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
                        new HelpDialog(true, "NEMA Button", "NEMA Button opens NEMA Dialog.", " ", " ", " ", " ", " ", " ", " ");
                    }
                    else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
                        new HelpDialog(true, "HARDWARE Button", "HARDWARE Button opens HARDWARE Dialog.", " ", " ", " ", " ", " ", " ", " ");
                    }
                }
            }
        }
    } // end of class HelpListener

    class AttachImageFileListener implements ActionListener {

        JDialog dialog;

        public void actionPerformed(ActionEvent event) {
            TX_Fmt lclv_tx_fmt;

            lclv_tx_fmt = gdvsim.gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE];

            final JPanel jp = new JPanel();
            JLabel nameLabel = new JLabel(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_NAME] /* "Name" */);
            JLabel typeLabel = new JLabel(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_TYPE] /* "Type" */);

            String[] array_image_file_types = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_TYPE].substring(1).split("\\|");
            String[] array_image_attachment_types = lclv_tx_fmt.msta_val[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_ATTACHMENT].substring(1).split("\\|");

            JLabel attachmentLabel = new JLabel(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_ATTACHMENT] /* "Attachment" */);
            JLabel translationXLabel = new JLabel(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_TRANSLATION_X] /* "Translation" */);
            JLabel translationYLabel = new JLabel(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_TRANSLATION_Y] /* "Translation" */);
            JLabel rotationLabel = new JLabel(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_ROTATION] /* "Rotation" */);
            JLabel scaleLabel = new JLabel(lclv_tx_fmt.msta_desc[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_SCALE] /* "Scale" */);

            final JButton browseButton = new JButton("Browse");
            final JTextField nameField = new JTextField();
            final JComboBox typeCombo = new JComboBox(array_image_file_types);
            final JComboBox attachmentCombo = new JComboBox(array_image_attachment_types);
            final JTextField translationXField = new JTextField("        ");
            final JSpinner translationXSpinner = new JSpinner(new SpinnerNumberModel(new Double(0.0), new Double(-9999.99), new Double(9999.99), new Double(0.1)));
            final JTextField translationYField = new JTextField("        ");
            final JSpinner translationYSpinner = new JSpinner(new SpinnerNumberModel(new Double(0.0), new Double(-9999.99), new Double(9999.99), new Double(0.1)));
            final JTextField rotationField = new JTextField();
            final JTextField scaleField = new JTextField();
            final JButton okButton = new JButton("OK");
            final JButton applyButton = new JButton("Apply");
            final JButton cancelButton = new JButton("Cancel");
            final JButton lstSquaresButton = new JButton("Define Translation, Rotation, and Scale using Least Squares Method");

            attachmentCombo.setSelectedItem(lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_ATTACHMENT]);
            rotationField.setText(Integer.toString(0));
            scaleField.setText(Integer.toString(1));

            okButton.setMnemonic(KeyEvent.VK_O);
            applyButton.setMnemonic(KeyEvent.VK_A);
            cancelButton.setMnemonic(KeyEvent.VK_C);

            Box nameBox = Box.createHorizontalBox();
            nameBox.add(nameLabel);
            nameBox.add(Box.createHorizontalStrut(5));
            nameBox.add(nameField);
            nameBox.add(Box.createHorizontalStrut(5));
            nameBox.add(browseButton);

            Box typeBox = Box.createHorizontalBox();
            typeBox.add(typeLabel);
            typeBox.add(Box.createHorizontalStrut(5));
            typeBox.add(typeCombo);

            Box attachmentBox = Box.createHorizontalBox();
            attachmentBox.add(attachmentLabel);
            attachmentBox.add(Box.createHorizontalStrut(5));
            attachmentBox.add(attachmentCombo);

            Box translationXBox = Box.createHorizontalBox();
            translationXBox.add(translationXLabel);
            translationXBox.add(Box.createHorizontalStrut(5));
            // translationXBox.add(translationXField );
            translationXBox.add(translationXSpinner);

            Box translationYBox = Box.createHorizontalBox();
            translationYBox.add(translationYLabel);
            translationYBox.add(Box.createHorizontalStrut(5));
            // translationBox.add(translationYField );
            translationYBox.add(translationYSpinner);

            JLabel translationNoteLabel = new JLabel("Shift-LeftMouseButton to move the Image");
            translationNoteLabel.setHorizontalAlignment(SwingConstants.RIGHT);

            Box transNoteBox = Box.createHorizontalBox();
            transNoteBox.add(translationNoteLabel);
            transNoteBox.add(Box.createGlue());

            // translationBox.add(Box.createHorizontalStrut(5));
            // translationBox.add(translationYLabel );
            // translationBox.add(translationYSpinner );
            // translationBox.add(Box.createHorizontalStrut(5));
            // translationBox.add(translationYField );

            Box rotationBox = Box.createHorizontalBox();
            rotationBox.add(rotationLabel);
            rotationBox.add(Box.createHorizontalStrut(5));
            rotationBox.add(rotationField);

            JLabel rotationNoteLabel = new JLabel("Control-LeftMouseButton to rotate the Image");
            Box rotNoteBox = Box.createHorizontalBox();
            rotNoteBox.add(rotationNoteLabel);
            rotNoteBox.add(Box.createGlue());

            Box scaleBox = Box.createHorizontalBox();
            scaleBox.add(scaleLabel);
            scaleBox.add(Box.createHorizontalStrut(5));
            scaleBox.add(scaleField);

            JLabel scaleNoteLabel = new JLabel("Shift-MouseWheel to scale the Image");
            Box scaleNoteBox = Box.createHorizontalBox();
            scaleNoteBox.add(scaleNoteLabel);
            scaleNoteBox.add(Box.createGlue());

            Box lstSquaresBox = Box.createHorizontalBox();
            lstSquaresBox.add(lstSquaresButton);

            Box actionsBox = Box.createHorizontalBox();
            actionsBox.add(okButton);
            actionsBox.add(Box.createHorizontalStrut(5));
            actionsBox.add(applyButton);
            actionsBox.add(Box.createHorizontalStrut(5));
            actionsBox.add(cancelButton);

            Box box = Box.createVerticalBox();
            box.add(nameBox);
            box.add(Box.createVerticalStrut(5));
            box.add(typeBox);
            box.add(Box.createVerticalStrut(5));
            box.add(attachmentBox);
            box.add(Box.createVerticalStrut(5));
            box.add(translationXBox);
            box.add(Box.createVerticalStrut(5));
            box.add(translationYBox);
            box.add(Box.createVerticalStrut(5));
            box.add(transNoteBox);
            box.add(Box.createVerticalStrut(5));
            box.add(rotationBox);
            box.add(Box.createVerticalStrut(5));
            box.add(rotNoteBox);
            box.add(Box.createVerticalStrut(5));
            box.add(scaleBox);
            box.add(Box.createVerticalStrut(5));
            box.add(scaleNoteBox);
            box.add(Box.createVerticalStrut(5));
            box.add(lstSquaresBox);
            box.add(Box.createVerticalStrut(10));
            box.add(actionsBox);
            jp.add(box);

            if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached != null) {
                if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached.equals("YES")) {
                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_name != null) {
                        nameField.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_name);
                    }

                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_type != null) {
                        typeCombo.setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_type);
                    }

                    if (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_attachment != null) {
                        attachmentCombo.setSelectedItem(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_attachment);
                    }

                    translationXSpinner.setValue(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x);
                    translationYSpinner.setValue(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y);
                    rotationField.setText(new Double(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_rotation).toString());
                    scaleField.setText(new Double(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_scale).toString());
                }
            }

            ActionListener al = new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    Object source = e.getSource();

                    if (source == browseButton) {
                        FileDialog d = new FileDialog(gdvsim.mainFrame, "Attach Image File", FileDialog.LOAD);
                        d.setDirectory(".");
                        d.setVisible(true);

                        if (d.getFile() != null) {
                            String filename = d.getDirectory() + d.getFile();

                            if (!filename.endsWith(".jpg")) {
                                filename = filename.concat(".jpg");
                            }

                            nameField.setText(filename);
                        }
                    }
                    else if (source == applyButton) {
                        try {
                            String filename = nameField.getText();
                            gdvsim.bi = ImageIO.read(new File(filename));

                            if (!rotationField.getText().equals("")) {
                                biAffineTransform = AffineTransform.getRotateInstance(Double.parseDouble(rotationField.getText()) * Math.PI / 180.0, gdvsim.bi.getWidth() / 2,
                                        gdvsim.bi.getHeight() / 2);
                            }
                            else {
                                biAffineTransform = new AffineTransform();
                            }

                            double scale = 1.0;

                            if (!scaleField.getText().trim().equals("")) {
                                scale = Double.parseDouble(scaleField.getText());
                            }

                            // gdvsim.imgCurrZoom = scale;

                            if ((Double)translationXSpinner.getValue() != 0.0) {
                                biAffineTransform.preConcatenate(AffineTransform.getTranslateInstance(((Double)translationXSpinner.getValue()) * scale, 0.0));
                            }

                            if ((Double)translationYSpinner.getValue() != 0.0) {
                                biAffineTransform.preConcatenate(AffineTransform.getTranslateInstance(0.0, ((Double)translationYSpinner.getValue()) * scale));
                            }

                            myIntersection.repaint();
                            detachImageFileMenuItem.setVisible(true);

                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached = "YES";
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_name = filename;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_type = typeCombo.getSelectedItem().toString();
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_attachment = attachmentCombo.getSelectedItem().toString();
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x = ((Double)translationXSpinner.getValue()).doubleValue();
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y = ((Double)translationYSpinner.getValue()).doubleValue();
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_rotation = Double.parseDouble(rotationField.getText());
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_scale = Double.parseDouble(scaleField.getText());

                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_NAME] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_ATTACHMENT] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_TRANSLATION_X] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_TRANSLATION_Y] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_ROTATION] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_SCALE] = gdvsim.gclv_inter.TX_FROM_USER;
                        }
                        catch (Exception excpt) {
                            System.err.println("Error reading Image File: " + excpt);
                        }
                    }
                    else if (source == okButton) {
                        try {
                            String filename = nameField.getText();
                            gdvsim.bi = ImageIO.read(new File(filename));

                            if (!rotationField.getText().equals("")) {
                                biAffineTransform = AffineTransform.getRotateInstance(Double.parseDouble(rotationField.getText()) * Math.PI / 180.0, gdvsim.bi.getWidth() / 2,
                                        gdvsim.bi.getHeight() / 2);
                            }
                            else {
                                biAffineTransform = new AffineTransform();
                            }

                            double scale = 1.0;

                            if (!scaleField.getText().trim().equals("")) {
                                scale = Double.parseDouble(scaleField.getText());
                            }

                            // gdvsim.imgCurrZoom = scale;

                            if ((Double)translationXSpinner.getValue() != 0.0) {
                                biAffineTransform.preConcatenate(AffineTransform.getTranslateInstance(((Double)translationXSpinner.getValue()) * scale, 0.0));
                            }

                            if ((Double)translationYSpinner.getValue() != 0.0) {
                                biAffineTransform.preConcatenate(AffineTransform.getTranslateInstance(0.0, ((Double)translationYSpinner.getValue()) * scale));
                            }

                            myIntersection.repaint();
                            detachImageFileMenuItem.setVisible(true);

                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached = "YES";
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_name = filename;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_type = typeCombo.getSelectedItem().toString();
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_attachment = attachmentCombo.getSelectedItem().toString();
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x = ((Double)translationXSpinner.getValue()).doubleValue();
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y = ((Double)translationYSpinner.getValue()).doubleValue();
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_rotation = Double.parseDouble(rotationField.getText());
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_scale = Double.parseDouble(scaleField.getText());

                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_NAME] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_TYPE] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_ATTACHMENT] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_TRANSLATION_X] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_TRANSLATION_Y] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_ROTATION] = gdvsim.gclv_inter.TX_FROM_USER;
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_IMAGE_FILE_SCALE] = gdvsim.gclv_inter.TX_FROM_USER;
                        }
                        catch (Exception excpt) {
                            System.err.println("Error reading Image File: " + excpt);
                        }

                        dialog.setVisible(false);
                        dialog.dispose();
                    }
                    else if (source == cancelButton) {
                        dialog.setVisible(false);
                        dialog.dispose();
                    }
                    else if (source == lstSquaresButton) {
                        lstSquaresMode = true;

                        // Object[] options = { };
                        // JOptionPane pane = new
                        // JOptionPane(jp,JOptionPane.PLAIN_MESSAGE,JOptionPane.DEFAULT_OPTION,null,
                        // options, null);
                        // dialog = pane.createDialog(null, lclv_tx_fmt.mstv_name);
                        // dialog.setVisible(true);
                        String inputValue = JOptionPane.showInputDialog("Enter the number of points");
                        lstSquaresPoints = Integer.parseInt(inputValue);

                        if (gdvsim.bi == null) {
                            try {
                                String filename = nameField.getText();
                                gdvsim.bi = ImageIO.read(new File(filename));

                                if (!rotationField.getText().equals("")) {
                                    biAffineTransform = AffineTransform.getRotateInstance(Double.parseDouble(rotationField.getText()) * Math.PI / 180.0, gdvsim.bi.getWidth() / 2,
                                            gdvsim.bi.getHeight() / 2);
                                }
                                else {
                                    biAffineTransform = new AffineTransform();
                                }

                                double scale = 1.0;

                                if (!scaleField.getText().trim().equals("")) {
                                    scale = Double.parseDouble(scaleField.getText());
                                }

                                // gdvsim.imgCurrZoom = scale;

                                if ((Double)translationXSpinner.getValue() != 0.0) {
                                    biAffineTransform.preConcatenate(AffineTransform.getTranslateInstance(((Double)translationXSpinner.getValue()) * scale, 0.0));
                                }

                                if ((Double)translationYSpinner.getValue() != 0.0) {
                                    biAffineTransform.preConcatenate(AffineTransform.getTranslateInstance(0.0, ((Double)translationYSpinner.getValue()) * scale));
                                }

                                myIntersection.repaint();
                                detachImageFileMenuItem.setVisible(true);

                                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached = "YES";
                                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_name = filename;
                                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_type = typeCombo.getSelectedItem().toString();
                                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_attachment = attachmentCombo.getSelectedItem().toString();
                                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x = ((Double)translationXSpinner.getValue()).doubleValue();
                                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y = ((Double)translationYSpinner.getValue()).doubleValue();
                                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_rotation = Double.parseDouble(rotationField.getText());
                                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_scale = Double.parseDouble(scaleField.getText());
                            }
                            catch (Exception excpt) {
                                System.err.println("Error reading Image File: " + excpt);
                            }
                        }

                        dialog.setVisible(false);
                        dialog.dispose();

                        /****
                         * final JPanel jp = new JPanel(); JDialog dialog; Object[] options = { };
                         * JOptionPane pane = new
                         * JOptionPane(jp,JOptionPane.PLAIN_MESSAGE,JOptionPane.
                         * DEFAULT_OPTION,null, options, null); dialog = pane.createDialog(null,
                         * "Pick points"); dialog.setVisible(true); //new
                         * JOptionPane(null,JOptionPane
                         * .PLAIN_MESSAGE,JOptionPane.DEFAULT_OPTION,null, null, null);
                         ****/
                    }
                }
            };

            KeyAdapter ka = new KeyAdapter() {

                public synchronized void keyPressed(KeyEvent e) {}
            };

            browseButton.addActionListener(al);
            nameField.addActionListener(al);
            typeCombo.addActionListener(al);
            attachmentCombo.addActionListener(al);
            translationXField.addActionListener(al);
            translationYField.addActionListener(al);
            rotationField.addActionListener(al);
            scaleField.addActionListener(al);
            applyButton.addActionListener(al);
            okButton.addActionListener(al);
            cancelButton.addActionListener(al);
            lstSquaresButton.addActionListener(al);

            Object[] options = {};
            JOptionPane pane = new JOptionPane(jp, JOptionPane.PLAIN_MESSAGE, JOptionPane.DEFAULT_OPTION, null, options, null);
            dialog = pane.createDialog(null, lclv_tx_fmt.mstv_name);
            dialog.setVisible(true);
        }
    } // end of class AttachImageFileListener

    class DetachImageFileListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            gdvsim.bi = null;
            gdvsim.biAffineTransform = null;
            myIntersection.repaint();
            detachImageFileMenuItem.setVisible(false);
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached = "NO";
            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH] = gdvsim.gclv_inter.TX_FROM_USER;
        }
    } // end of class DetachImageFileListener

    boolean ActivateAction() {
        if (gclv_inter.mstv_gdvdataFile.trim().length() == 0) {
            gclv_inter.mstv_errorMessage = "Error in gdvsim: no gdvdata file is open.";
            gclv_inter.errorMessage();
            gclv_inter.msiv_returnCode = gclv_inter.RETURN_FATAL_ERROR;
            return false;
        }

        if (gclv_inter.mstv_simdataFile.trim().length() == 0) {
            gclv_inter.mstv_errorMessage = "Error in gdvsim: no simdata file is open.";
            gclv_inter.errorMessage();
            gclv_inter.msiv_returnCode = gclv_inter.RETURN_FATAL_ERROR;
            return false;
        }

        gclv_inter.filesActivate(gclv_inter.mstv_gdvdataFile, gclv_inter.mstv_simdataFile);

        if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
            return false;
        }

        return true;

    } // end of method ActivateAction

    void NewAction() {
        currZoom = 1.0;
        imgCurrZoom = 1.0;
        curr_screenXShift = 0.0;
        curr_screenYShift = 0.0;

        resetGlobalValue();

        intersectionTypeButton.setText("Intersection Type");
        intersectionTypeButton.setVisible(true);
        intersectionTypeButton.setEnabled(true);
        intersectionTypeButton.requestFocus(true);

        gdvDataButton.setVisible(false);
        leg1Button.setVisible(false);
        leg2Button.setVisible(false);
        leg3Button.setVisible(false);
        leg4Button.setVisible(false);
        leg5Button.setVisible(false);
        leg6Button.setVisible(false);
        simulationButton.setVisible(false);
        laneControlButton.setVisible(false);
        intersectionControlButton.setVisible(false);
        greenIntervalSequenceButton.setVisible(false);
        detectorButton.setVisible(false);

        myIntersection.setVisible(false);
        myIntersection.setEnabled(false);

        fileMenu.remove(imageMenu);
        gdvsim.bi = null;

    } // end of method NewAction

    class ComponentActionListener implements ActionListener {

        public void actionPerformed(ActionEvent event) {
            if (event.getSource() == intersectionTypeButton) {
                new IntersectionTypeDialog();
            }
            else if (event.getSource() == gdvDataButton) {
                new GDVDataDialog();
            }
            else if (event.getSource() == leg1Button) {
                leg_number = 1;
                new LegDataDialog(leg_number);
            }
            else if (event.getSource() == leg2Button) {
                leg_number = 2;
                new LegDataDialog(leg_number);
            }
            else if (event.getSource() == leg3Button) {
                leg_number = 3;
                new LegDataDialog(leg_number);
            }
            else if (event.getSource() == leg4Button) {
                leg_number = 4;
                new LegDataDialog(leg_number);
            }
            else if (event.getSource() == leg5Button) {
                leg_number = 5;
                new LegDataDialog(leg_number);
            }
            else if (event.getSource() == leg6Button) {
                leg_number = 6;
                new LegDataDialog(leg_number);
            }
            else if (event.getSource() == simulationButton) {
                if (!SimulationAction()) {
                    return;
                }
            }
            else if (event.getSource() == laneControlButton) {
                new LaneControlDialog();
            }
            else if (event.getSource() == intersectionControlButton) {
                IntersectionControlAction();
            }
            else if (event.getSource() == greenIntervalSequenceButton) {
                new GreenIntervalSequenceDialog();
            }
            else if (event.getSource() == detectorButton) {
                new DetectorDialog();
            }
            else if (event.getSource() == fileMenuCommandsMenuItem) {
                new AboutDialog("FileMenuCommands");
            }
            else if (event.getSource() == viewMenuCommandsMenuItem) {
                new AboutDialog("ViewMenuCommands");
            }
            else if (event.getSource() == pressF1MenuItem) {
                new AboutDialog("Specialkeyactions");
            }
            else if (event.getSource() == StandardIntersectionMenuItem) {
                new AboutDialog("Standard Intersection");
            }
            else if (event.getSource() == DiamondInterchangeMenuItem) {
                new AboutDialog("Diamond Interchange");
            }
            else if (event.getSource() == aboutMenuItem) {
                new AboutDialog("About");
            }
            else if (event.getSource() == activateMenuItem) {
                if (!ActivateAction()) {
                    return;
                }
            }
            else if (event.getSource() == newMenuItem) {
                NewAction();
            }
            else if (event.getSource() == saveMenuItem) {
                flag_save = false;
                SaveAction();
            }
            else if (event.getSource() == exitMenuItem) {
                gdvsim.gclv_inter.closeAll();
                mainFrame.dispose();
                System.exit(0);
            }
            else if (event.getSource() == libraryMenuItem) {
                flag_library = false;

                gclv_inter.filesReadSketch();

                if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
                    return;
                }

                new SketchNameListDialog();
            }
            else if (event.getSource() == existingFileMenuItem) {
                ExistingFileAction();

                if (flag_OpenExistGeoFile) {
                    saveMenuItem.setEnabled(true);
                    myIntersection.setVisible(true);
                    myIntersection.draw();
                }
            }
            else if (event.getSource() == dataMenuItem) {
                flag_data = false;

                DataAction();

                if (flag_data) {
                    saveMenuItem.setEnabled(true);
                }
            }
            else if (event.getSource() == printoutMenuItem) {
                flag_printout = false;

                PrintoutAction();

                if (flag_printout) {
                    saveMenuItem.setEnabled(true);
                }
            }
            else if (event.getSource() == DXFMenuItem) {
                flag_DXF = false;

                SaveAsDXFAction();

                if (flag_DXF) {
                    saveMenuItem.setEnabled(true);
                }
            }
        }
    } // end of class ComponentActionListener

    class ComponentKeyListener extends KeyAdapter {

        public void keyPressed(KeyEvent event) {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                if (event.getSource() == intersectionTypeButton) {
                    new IntersectionTypeDialog();
                }
                else if (event.getSource() == gdvDataButton) {
                    new GDVDataDialog();
                }
                else if (event.getSource() == leg1Button) {
                    leg_number = 1;
                    new LegDataDialog(leg_number);
                }
                else if (event.getSource() == leg2Button) {
                    leg_number = 2;
                    new LegDataDialog(leg_number);
                }
                else if (event.getSource() == leg3Button) {
                    leg_number = 3;
                    new LegDataDialog(leg_number);
                }
                else if (event.getSource() == leg4Button) {
                    leg_number = 4;
                    new LegDataDialog(leg_number);
                }
                else if (event.getSource() == leg5Button) {
                    leg_number = 5;
                    new LegDataDialog(leg_number);
                }
                else if (event.getSource() == leg6Button) {
                    leg_number = 6;
                    new LegDataDialog(leg_number);
                }
                else if (event.getSource() == simulationButton) {
                    if (!SimulationAction()) {
                        return;
                    }
                }
                else if (event.getSource() == laneControlButton) {
                    new LaneControlDialog();
                }
                else if (event.getSource() == intersectionControlButton) {
                    IntersectionControlAction();
                }
                else if (event.getSource() == greenIntervalSequenceButton) {
                    new GreenIntervalSequenceDialog();
                }
                else if (event.getSource() == detectorButton) {
                    new DetectorDialog();
                }
                else if (event.getSource() == fileMenuCommandsMenuItem) {
                    new AboutDialog("FileMenuCommands");
                }
                else if (event.getSource() == viewMenuCommandsMenuItem) {
                    new AboutDialog("ViewMenuCommands");
                }
                else if (event.getSource() == pressF1MenuItem) {
                    new AboutDialog("Specialkeyactions");
                }
                else if (event.getSource() == StandardIntersectionMenuItem) {
                    new AboutDialog("Standard Intersection");
                }
                else if (event.getSource() == DiamondInterchangeMenuItem) {
                    new AboutDialog("Diamond Interchange");
                }
                else if (event.getSource() == aboutMenuItem) {
                    new AboutDialog("About");
                }
                else if (event.getSource() == activateMenuItem) {
                    if (!ActivateAction()) {
                        return;
                    }
                }
                else if (event.getSource() == newMenuItem) {
                    NewAction();
                }
                else if (event.getSource() == saveMenuItem) {
                    flag_save = false;
                    SaveAction();
                }
                else if (event.getSource() == exitMenuItem) {
                    gdvsim.gclv_inter.closeAll();
                    mainFrame.dispose();
                    System.exit(0);
                }
                else if (event.getSource() == libraryMenuItem) {
                    flag_library = false;

                    gclv_inter.filesReadSketch();

                    if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
                        return;
                    }

                    new SketchNameListDialog();
                }
                else if (event.getSource() == existingFileMenuItem) {
                    ExistingFileAction();

                    if (flag_OpenExistGeoFile) {
                        saveMenuItem.setEnabled(true);
                        myIntersection.setVisible(true);
                        myIntersection.draw();
                    }
                }
                else if (event.getSource() == dataMenuItem) {
                    flag_data = false;

                    DataAction();

                    if (flag_data) {
                        saveMenuItem.setEnabled(true);
                    }
                }
                else if (event.getSource() == printoutMenuItem) {
                    flag_printout = false;

                    PrintoutAction();

                    if (flag_printout) {
                        saveMenuItem.setEnabled(true);
                    }
                }
                else if (event.getSource() == DXFMenuItem) {
                    flag_DXF = false;

                    SaveAsDXFAction();

                    if (flag_DXF) {
                        saveMenuItem.setEnabled(true);
                    }
                }
            }
        }
    } // end of class ComponentKeyListener

    boolean SimulationAction() {
        if (!gclv_inter.mbov_GDV_Data_OK) {
            JOptionPane.showMessageDialog(null, "GDV Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        if (gclv_inter.mbov_is_diamond_interchange) {
            if (!gclv_inter.mbov_Diamond_Leg_OK) {
                JOptionPane.showMessageDialog(null, "Diamond Leg Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (!gclv_inter.mbov_Diamond_Lane_OK) {
                JOptionPane.showMessageDialog(null, "Diamond Lane Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }

        if (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_NO_LEGS] == gclv_inter.TX_DATA_IS_INVALID) {
            JOptionPane.showMessageDialog(null, "number of legs invalid.", "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        for (int lsiv_leg = 1; lsiv_leg <= gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs; lsiv_leg++) {
            if (!gclv_inter.mboa_Leg_OK[lsiv_leg]) {
                JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb != 0) {
                if (!gclv_inter.mboa_Leg_Inbound_Data_OK[lsiv_leg]) {
                    JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " Inbound Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }

            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb != 0 && gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out != 0) {
                if (!gclv_inter.mboa_Leg_Lane_Data_OK[lsiv_leg]) {
                    JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " Lane Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }

            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb != 0) {
                if (!gclv_inter.mboa_Leg_Outbound_Data_OK[lsiv_leg]) {
                    JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " Outbound Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }

            if (gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb != 0 && gdvsim.gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out != 0) {
                if (!gclv_inter.mboa_Leg_SDR_Data_OK[lsiv_leg]) {
                    JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " SDR Data has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }

            if (!gclv_inter.mboa_Leg_Varying_Traffic_Period_OK[lsiv_leg]) {
                JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " Varying traffic period has not been accepted.", "Error Message", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }

        gclv_inter.checkForErrorsGDV();

        if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
            return false;
        }

        new SimulationDataDialog();

        return true;

    } // end of method SimulationAction

    void IntersectionControlAction() {
        if (gdvsim.gclv_inter.mbov_is_ic_PRETIMED) {
            new PretimedDialog();
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT) {
            new SemiactDialog();
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_FULL_ACT) {
            new FullactDialog();
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_TEX_DIA) {
            new TexasDiamondTimingDialog();
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            new NemaDialog();
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            new HardwareDialog();
        }
    } // end of method IntersectionControlAction

    class ViewListener implements ActionListener {

        public void actionPerformed(ActionEvent e) {
            Object source = e.getSource();

            if (source == up_1_8_MenuItem) {
                curr_screenYShift = 0.125 + curr_screenYShift;
                myIntersection.repaint();
            }
            else if (source == up_1_4_MenuItem) {
                curr_screenYShift = 0.25 + curr_screenYShift;
                myIntersection.repaint();
            }
            else if (source == up_1_2_MenuItem) {
                curr_screenYShift = 0.5 + curr_screenYShift;
                myIntersection.repaint();
            }
            else if (source == down_1_8_MenuItem) {
                curr_screenYShift = -0.125 + curr_screenYShift;
                myIntersection.repaint();
            }
            else if (source == down_1_4_MenuItem) {
                curr_screenYShift = -0.25 + curr_screenYShift;
                myIntersection.repaint();
            }
            else if (source == down_1_2_MenuItem) {
                curr_screenYShift = -0.5 + curr_screenYShift;
                myIntersection.repaint();
            }
            else if (source == left_1_8_MenuItem) {
                curr_screenXShift = 0.125 + curr_screenXShift;
                myIntersection.repaint();
            }
            else if (source == left_1_4_MenuItem) {
                curr_screenXShift = 0.25 + curr_screenXShift;
                myIntersection.repaint();
            }
            else if (source == left_1_2_MenuItem) {
                curr_screenXShift = 0.5 + curr_screenXShift;
                myIntersection.repaint();
            }
            else if (source == right_1_8_MenuItem) {
                curr_screenXShift = -0.125 + curr_screenXShift;
                myIntersection.repaint();
            }
            else if (source == right_1_4_MenuItem) {
                curr_screenXShift = -0.25 + curr_screenXShift;
                myIntersection.repaint();
            }
            else if (source == right_1_2_MenuItem) {
                curr_screenXShift = -0.5 + curr_screenXShift;
                myIntersection.repaint();
            }
            else if (source == zoomToMenuItem) {
                new gdvZoomDialog(mainFrame);
            }
            else if (source == zoomIn_2X_MenuItem) {
                currZoom = currZoom * 2.0;
                imgCurrZoom = imgCurrZoom * 2.0;
                myIntersection.repaint();
            }
            else if (source == zoomIn_4X_MenuItem) {
                currZoom = currZoom * 4.0;
                imgCurrZoom = imgCurrZoom * 4.0;
                myIntersection.repaint();
            }
            else if (source == zoomOut_2X_MenuItem) {
                currZoom = currZoom / 2.0;
                imgCurrZoom = imgCurrZoom / 2.0;
                myIntersection.repaint();
            }
            else if (source == zoomOut_4X_MenuItem) {
                currZoom = currZoom / 4.0;
                imgCurrZoom = imgCurrZoom / 4.0;
                myIntersection.repaint();
            }
            else if (source == zoomAreaMenuItem) {
                myIntersection.removeMouseWheelListener(myIntersection.mwl);
                myIntersection.removeMouseMotionListener(myIntersection.mma);
                myIntersection.removeMouseListener(myIntersection.ma);
                ma = new MouseAdapter() {

                    public synchronized void mousePressed(MouseEvent e) {
                        int shiftmask = InputEvent.SHIFT_DOWN_MASK;
                        int ctrlmask = InputEvent.CTRL_DOWN_MASK;
                        int onmask = InputEvent.BUTTON1_DOWN_MASK;

                        if ((e.getModifiersEx() & onmask) == onmask && !((e.getModifiersEx() & shiftmask) == shiftmask) && !((e.getModifiersEx() & ctrlmask) == ctrlmask)) {
                            if (!startBox) {
                                delta = 0;
                                startX = e.getX();
                                startY = e.getY();
                                aspectRatio = (myIntersection.getWidth() * 1.0) / myIntersection.getHeight();
                            }
                        }
                    }

                    public synchronized void mouseClicked(MouseEvent e) {
                        int shiftmask = InputEvent.SHIFT_DOWN_MASK;
                        int ctrlmask = InputEvent.CTRL_DOWN_MASK;
                        int onmask = InputEvent.BUTTON1_DOWN_MASK;

                        if ((e.getButton() == MouseEvent.BUTTON1) && (!startBox) && (!((e.getModifiersEx() & shiftmask) == shiftmask)) && (!((e.getModifiersEx() & ctrlmask) == ctrlmask))) {
                            startBox = true;
                            startX = e.getX();
                            startY = e.getY();
                            aspectRatio = (myIntersection.getWidth() * 1.0) / myIntersection.getHeight();
                        }
                        else if ((e.getButton() == MouseEvent.BUTTON1) && startBox) {
                            startBox = false;
                            zoomRect = null;
                            double xShift = ((deltaX > 0 ? startX : startX - aspectRatio * delta) + aspectRatio * delta / 2.0) / (myIntersection.getWidth() / 2.0) - 1.0;
                            double yShift = ((deltaY > 0 ? startY : startY - delta) + delta / 2.0) / (myIntersection.getHeight() / 2.0) - 1.0;
                            if (delta > 5) {
                                currZoom = 1.0 * (myIntersection.getHeight() * currZoom / delta);
                                imgCurrZoom = 1.0 * (myIntersection.getHeight() * imgCurrZoom / delta);
                                curr_screenXShift = -1.0 * (xShift * myIntersection.getHeight() / (1.0 * delta)) / 2.0 + curr_screenXShift * myIntersection.getHeight() / (1.0 * delta);
                                curr_screenYShift = -1.0 * (yShift * myIntersection.getHeight() / (1.0 * delta)) / 2.0 + curr_screenYShift * myIntersection.getHeight() / (1.0 * delta);
                                myIntersection.repaint();
                                // Race condition??
                                myIntersection.removeMouseMotionListener(mma);
                                myIntersection.removeMouseListener(ma);
                                myIntersection.addMouseWheelListener(myIntersection.mwl);
                                myIntersection.addMouseMotionListener(myIntersection.mma);
                                myIntersection.addMouseListener(myIntersection.ma);
                            }
                            delta = 0;
                            deltaX = 0;
                            deltaY = 0;
                            startX = 0;
                            startY = 0;
                        }
                    }

                    public synchronized void mouseReleased(MouseEvent e) {
                        if ((e.getButton() == MouseEvent.BUTTON1) && (!startBox)) {
                            zoomRect = null;
                            double xShift = ((deltaX > 0 ? startX : startX - aspectRatio * delta) + aspectRatio * delta / 2.0) / (myIntersection.getWidth() / 2.0) - 1.0;
                            double yShift = ((deltaY > 0 ? startY : startY - delta) + delta / 2.0) / (myIntersection.getHeight() / 2.0) - 1.0;
                            if (delta > 5) {
                                currZoom = 1.0 * (myIntersection.getHeight() * currZoom / delta);
                                imgCurrZoom = 1.0 * (myIntersection.getHeight() * imgCurrZoom / delta);
                                curr_screenXShift = -1.0 * (xShift * myIntersection.getHeight() / (1.0 * delta)) / 2.0 + curr_screenXShift * myIntersection.getHeight() / (1.0 * delta);
                                curr_screenYShift = -1.0 * (yShift * myIntersection.getHeight() / (1.0 * delta)) / 2.0 + curr_screenYShift * myIntersection.getHeight() / (1.0 * delta);
                                myIntersection.repaint();
                                // Race condition??
                                myIntersection.removeMouseMotionListener(mma);
                                myIntersection.removeMouseListener(ma);
                                myIntersection.addMouseWheelListener(myIntersection.mwl);
                                myIntersection.addMouseMotionListener(myIntersection.mma);
                                myIntersection.addMouseListener(myIntersection.ma);
                            }
                        }
                    }

                    public synchronized void mouseEntered(MouseEvent e) {}
                };

                mma = new MouseMotionAdapter() {

                    public synchronized void mouseDragged(MouseEvent e) {
                        int shiftmask = InputEvent.SHIFT_DOWN_MASK;
                        int ctrlmask = InputEvent.CTRL_DOWN_MASK;
                        int onmask = InputEvent.BUTTON1_DOWN_MASK;

                        if ((e.getModifiersEx() & onmask) == onmask && !((e.getModifiersEx() & shiftmask) == shiftmask) && !((e.getModifiersEx() & ctrlmask) == ctrlmask)) {
                            deltaX = e.getX() - startX;
                            deltaY = e.getY() - startY;
                            delta = (Math.min(Math.abs(deltaX), Math.abs(deltaY)));
                            zoomRect = new Rectangle2D.Double((deltaX > 0 ? startX : startX - aspectRatio * delta), (deltaY > 0 ? startY : startY - delta), (aspectRatio * delta), (delta));
                            myIntersection.repaint();
                        }
                    }

                    public synchronized void mouseMoved(MouseEvent e) {
                        if (startBox) {
                            deltaX = e.getX() - startX;
                            deltaY = e.getY() - startY;
                            delta = (Math.min(Math.abs(deltaX), Math.abs(deltaY)));
                            zoomRect = new Rectangle2D.Double((deltaX > 0 ? startX : startX - aspectRatio * delta), (deltaY > 0 ? startY : startY - delta), (aspectRatio * delta), (delta));
                            myIntersection.repaint();
                        }
                    }
                };

                myIntersection.addMouseListener(ma);
                myIntersection.addMouseMotionListener(mma);
            }
            else if (source == resetMenuItem) {
                currZoom = 1.0;
                imgCurrZoom = 1.0;
                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_scale = imgCurrZoom;
                curr_screenXShift = 0.0;
                curr_screenYShift = 0.0;
                myIntersection.repaint();
            }
            else if (source == leftCenterMenuItem) {
                curr_screenXShift = ((double)gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between / PARAMS.TEXAS_MODEL_XYCNTR) * currZoom;
                curr_screenYShift = 0.0;
                myIntersection.repaint();
            }
            else if (source == rightCenterMenuItem) {

                curr_screenXShift = -1.0 * ((double)gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between / PARAMS.TEXAS_MODEL_XYCNTR) * currZoom;
                curr_screenYShift = 0.0;
                myIntersection.repaint();
            }
            else if (source == centerIntersectionMenuItem) {
                curr_screenXShift = 0.0;
                curr_screenYShift = 0.0;
                myIntersection.repaint();
            }
            else if (source == viewAttachedImage) {
                gclv_inter.mbov_view_attached_image = viewAttachedImage.isSelected();
                myIntersection.repaint();
            }
            else if (source == viewDetectorsJCheckBoxMenuItem) {
                gclv_inter.mbov_view_detectors = viewDetectorsJCheckBoxMenuItem.isSelected();
                myIntersection.repaint();
            }
            else if (source == viewLaneControlJCheckBoxMenuItem) {
                gclv_inter.mbov_view_lane_control = viewLaneControlJCheckBoxMenuItem.isSelected();
                myIntersection.repaint();
            }
            else if (source == viewSDRsJCheckBoxMenuItem) {
                gclv_inter.mbov_view_sdrs = viewSDRsJCheckBoxMenuItem.isSelected();
                myIntersection.repaint();
            }
            else if (source == viewTurnMovementsJCheckBoxMenuItem) {
                gclv_inter.mbov_view_turn_movement = viewTurnMovementsJCheckBoxMenuItem.isSelected();
                myIntersection.repaint();
            }
            else if (source == viewUserArcsJCheckBoxMenuItem) {
                gclv_inter.mbov_view_user_arcs = viewUserArcsJCheckBoxMenuItem.isSelected();
                myIntersection.repaint();
            }
            else if (source == viewUserLinesJCheckBoxMenuItem) {
                gclv_inter.mbov_view_user_lines = viewUserLinesJCheckBoxMenuItem.isSelected();
                myIntersection.repaint();
            }
        }
    } // end of class ViewListener

    public static void main(String psta_args[] /* command line parameters */
    ) {
        if (psta_args.length > 2) {
            JOptionPane.showMessageDialog(null, "Too much command-line parameter.", "Command Line Error", JOptionPane.ERROR_MESSAGE);
            return;
        }

        gclv_inter.init(psta_args);
        if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
            return;
        }

        gclv_inter.mbov_debug_calculateGraphics = false;
        gclv_inter.mbov_debug_checkForErrorsGDV = false;
        gclv_inter.mbov_debug_checkForErrorsSIM = false;
        gclv_inter.mbov_debug_filesReadGDV = false;
        gclv_inter.mbov_debug_filesReadSIM = false;
        gclv_inter.mbov_debug_filesReadSketch = false;
        gclv_inter.mbov_debug_filesWriteDXF = false;
        gclv_inter.mbov_debug_filesWriteGDV = false;
        gclv_inter.mbov_debug_filesWriteSIM = false;
        gclv_inter.mbov_debug_GDVCON = false;
        gclv_inter.mbov_debug_init = false;
        gclv_inter.mbov_debug_menu = false;
        gclv_inter.mbov_debug_other = false;
        gclv_inter.mbov_debug_paint = false;
        gclv_inter.mbov_debug_printGDV = false;
        gclv_inter.mbov_debug_printSIM = false;
        gclv_inter.mbov_debug_readFromCard = false;
        gclv_inter.mbov_debug_readFromCard1 = false;
        gclv_inter.mbov_debug_setAllInvalid = false;
        gclv_inter.mbov_debug_writeToCard = false;
        gclv_inter.mbov_debug_writeToCard1 = false;

        if (psta_args.length == 0) {
            new gdvsim("ParameterOne", "ParameterTwo");
        }
        else if (psta_args.length == 1) {
            new gdvsim(psta_args[0], "ParameterTwo");
        }
        else if (psta_args.length == 2) {
            new gdvsim(psta_args[0], psta_args[1]);
        }
    } // end of method main

    public void openFileWithCommandLineArgument() {
        if (gclv_inter.mbov_is_diamond_interchange) {
            if ((gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0)
                    || (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)) {
                if (gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 1 has" + gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + " Inbound Lane"
                            + (gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
                if (gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 3 has " + gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + " Outbound Lane"
                            + (gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
                if (gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 4 has" + gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + " Inbound Lane"
                            + (gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
                if (gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 6 has" + gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + " Outbound Lane"
                            + (gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }
        }

        File fileGDV;
        File fileSIM;

        fileGDV = new File(ParamGDVfile);
        if (!fileGDV.exists()) {
            JOptionPane.showMessageDialog(null, "GDV File Does Not Exist", "GDV File Does Not Exist", JOptionPane.ERROR_MESSAGE);
            return;
        }

        gclv_inter.filesReadGDV(ParamGDVfile);
        if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
            return;
        }

        currZoom = 1.0;
        imgCurrZoom = 1.0;
        curr_screenXShift = 0.0;
        curr_screenYShift = 0.0;

        gdvsim.fileMenu.add(gdvsim.imageMenu, gdvsim.fileMenu.getMenuComponentCount() - 2);

        gclv_inter.mbov_view_detectors = true;
        gclv_inter.mbov_view_lane_control = true;
        gclv_inter.mbov_view_sdrs = true;
        gclv_inter.mbov_view_turn_movement = true;
        gclv_inter.mbov_view_user_arcs = true;
        gclv_inter.mbov_view_user_lines = true;

        for (int lsiv_leg = 0; lsiv_leg < 6; lsiv_leg++) {
            SDR_count[lsiv_leg] = 0;
            for (int lsiv_sdr = 0; lsiv_sdr < 4; lsiv_sdr++) {
                SDR_legnumber[lsiv_leg][lsiv_sdr] = 0;
                SDR_setback[lsiv_leg][lsiv_sdr] = 0;
                SDR_offset[lsiv_leg][lsiv_sdr] = 0;
            }
        }

        for (int lsiv_sdr = 1; lsiv_sdr <= gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs; lsiv_sdr++) {
            if (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_setback == 0)
                continue;
            int lsiv_leg = gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_leg_number;
            int lsiv_cnt = ++SDR_count[lsiv_leg - 1];
            SDR_legnumber[lsiv_leg - 1][lsiv_cnt - 1] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_leg_number;
            SDR_setback[lsiv_leg - 1][lsiv_cnt - 1] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_setback;
            SDR_offset[lsiv_leg - 1][lsiv_cnt - 1] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_offset;
        }

        mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gclv_inter.IVERSN + " - GDV=" + gclv_inter.mstv_gdvdataFile);
        resetSimulationGlobalValue();
        gclv_inter.mbov_GDV_Data_OK = true;
        gclv_inter.mbov_Diamond_Lane_OK = true;
        gclv_inter.mbov_Diamond_Leg_OK = true;
        gclv_inter.mbov_Free_UTurns_OK = true;

        flag_arc_ok = true;
        flag_line_ok = true;
        flag_drvVehByVeh_ok = true;
        flag_drvVehByDrv_ok = true;
        flag_plotOpt_ok = true;
        flag_specialVehicle_ok = true;
        flag_driverMixData_ok = true;
        flag_driverClass_ok = true;
        flag_vehicleClass_ok = true;

        for (int lsiv_i = 1; lsiv_i <= 6; lsiv_i++) {
            gclv_inter.mboa_Leg_OK[lsiv_i] = true;
            gclv_inter.mboa_Leg_Inbound_Data_OK[lsiv_i] = true;
            gclv_inter.mboa_Leg_Lane_Data_OK[lsiv_i] = true;
            gclv_inter.mboa_Leg_Outbound_Data_OK[lsiv_i] = true;
            gclv_inter.mboa_Leg_SDR_Data_OK[lsiv_i] = true;
            gclv_inter.mboa_Leg_Traffic_Mix_OK[lsiv_i] = true;
        }
        windowAfterOpenFile();

        if (ParamSIMfile.equals("ParameterTwo"))
            return;

        fileSIM = new File(ParamSIMfile);
        if (!fileSIM.exists()) {
            JOptionPane.showMessageDialog(null, "SIM File Does Not Exist", "SIM File Does Not Exist", JOptionPane.ERROR_MESSAGE);
            return;
        }

        gclv_inter.filesReadSIM(ParamSIMfile);

        if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
            return;
        }

        flag_library = false;
        flag_OpenExistGeoFile = true;
        flag_OpenExistSimFile = true;
        flag_OpenNewFile = false;
        flag_simulationData_ok = true;
        flag_greenSequence_ok = true;
        flag_laneControl_ok = true;
        flag_pretimed_ok = true;
        flag_semiact_ok = true;
        flag_fullact_ok = true;
        flag_nema_ok = true;
        flag_diamondTiming_ok = true;
        flag_diamondInterval_ok = true;
        flag_diamondOption_ok = true;
        flag_nemaMovement_ok = true;
        flag_nemaRingGroup_ok = true;
        flag_nemaOverlap_ok = true;
        flag_detDataForNonDiamond_ok = true;
        flag_detDataForDiamond_ok = true;
        flag_detDataForTexdia_ok = true;
        flag_detConnForNema_ok = true;
        flag_detConnForNonNema_ok = true;
        flag_VMSmesg_ok = true;
        flag_simulationData_ok = true;
        flag_laneControl_ok = true;

        simulationButton.setVisible(true);
        laneControlButton.setVisible(true);

        if (gdvsim.gclv_inter.mbov_is_ic_PRETIMED) {
            intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            intersectionControlButton.setVisible(true);
            intersectionControlButton.setMnemonic(KeyEvent.VK_P);

            flag_pretimed_ok = true;
            flag_greenSequence_ok = true;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT) {
            intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            intersectionControlButton.setVisible(true);
            intersectionControlButton.setMnemonic(KeyEvent.VK_E);

            flag_semiact_ok = true;
            flag_greenSequence_ok = true;

            if (gclv_inter.mbov_is_diamond_interchange) {
                flag_detDataForDiamond_ok = true;
                flag_detConnForNonNema_ok = true;
            }
            else {
                flag_detDataForNonDiamond_ok = true;
                flag_detConnForNonNema_ok = true;
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_FULL_ACT) {
            intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            intersectionControlButton.setVisible(true);
            intersectionControlButton.setMnemonic(KeyEvent.VK_F);

            flag_fullact_ok = true;
            flag_greenSequence_ok = true;

            if (gclv_inter.mbov_is_diamond_interchange) {
                flag_detDataForDiamond_ok = true;
                flag_detConnForNonNema_ok = true;
            }
            else {
                flag_detDataForNonDiamond_ok = true;
                flag_detConnForNonNema_ok = true;
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_TEX_DIA) {
            intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            intersectionControlButton.setVisible(true);
            intersectionControlButton.setMnemonic(KeyEvent.VK_T);

            flag_diamondTiming_ok = true;
            flag_diamondInterval_ok = true;
            flag_diamondOption_ok = true;
            flag_greenSequence_ok = true;
            flag_detDataForTexdia_ok = true;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            intersectionControlButton.setVisible(true);
            intersectionControlButton.setMnemonic(KeyEvent.VK_N);

            flag_nema_ok = true;
            flag_nemaMovement_ok = true;
            flag_nemaRingGroup_ok = true;
            flag_nemaOverlap_ok = true;
            flag_greenSequence_ok = true;

            if (gclv_inter.mbov_is_diamond_interchange) {
                flag_detDataForDiamond_ok = true;
                flag_detConnForNema_ok = true;
            }
            else {
                flag_detDataForNonDiamond_ok = true;
                flag_detConnForNema_ok = true;
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            intersectionControlButton.setVisible(true);
            intersectionControlButton.setMnemonic(KeyEvent.VK_H);

            flag_nema_ok = true;
            flag_nemaMovement_ok = true;
            flag_greenSequence_ok = true;

            if (gclv_inter.mbov_is_diamond_interchange) {
                flag_detDataForDiamond_ok = true;
                flag_detConnForNema_ok = true;
            }
            else {
                flag_detDataForNonDiamond_ok = true;
                flag_detConnForNema_ok = true;
            }
        }
        else {
            intersectionControlButton.setVisible(false);
        }

        greenIntervalSequenceButton.setVisible(gdvsim.gclv_inter.mbov_is_ic_signal_controlled);

        detectorButton.setVisible(gdvsim.gclv_inter.mbov_is_ic_detector_data);

        mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gclv_inter.IVERSN + " - GDV=" + gclv_inter.mstv_gdvdataFile + "  SIM=" + gclv_inter.mstv_simdataFile);

        if (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached.equals("YES")) {
            try {
                gdvsim.bi = ImageIO.read(new File(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_name));
                biAffineTransform = AffineTransform.getTranslateInstance(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x
                        * gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_scale,
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y
                                * gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_scale);
                biAffineTransform.rotate(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_rotation * Math.PI / 180.0, gdvsim.bi.getWidth() / 2,
                        gdvsim.bi.getHeight() / 2);
                myIntersection.repaint();
                detachImageFileMenuItem.setVisible(true);
            }
            catch (Exception excpt) {
                System.err.println("Error reading Image File: " + excpt);
            }
        }
    } // end of method openFileWithCommandLineArgument

    public static class DrawIntersection extends JPanel {

        public MouseAdapter ma;

        public MouseMotionAdapter mma;

        public MouseWheelListener mwl;

        public Rectangle2D.Double zoomRect = null;

        int startX = 0;

        int startY = 0;

        int deltaX = 0;

        int deltaY = 0;

        public DrawIntersection() {
            mwl = new MouseWheelListener() {

                public void mouseWheelMoved(MouseWheelEvent e) {
                    int shiftmask = InputEvent.SHIFT_DOWN_MASK;
                    int ctrlmask = InputEvent.CTRL_DOWN_MASK;
                    if (!((e.getModifiersEx() & shiftmask) == shiftmask) && !((e.getModifiersEx() & ctrlmask) == ctrlmask)) {
                        // gdvsim.mvImg = true;
                        int zoomDelta = e.getWheelRotation();
                        currZoom = currZoom * (1 - (0.1) * zoomDelta);
                        imgCurrZoom = imgCurrZoom * (1 - (0.1) * zoomDelta);
                        // gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_scale
                        // = imgCurrZoom;
                        repaint();
                    }
                    else {
                        if (gclv_inter.mbov_view_attached_image) {
                            // gdvsim.mvImg = false;
                            int zoomDelta = e.getWheelRotation();
                            imgCurrZoom = imgCurrZoom * (1 - (0.1) * zoomDelta);
                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_scale *= (1 - (0.1) * zoomDelta); // imgCurrZoom;
                            repaint();
                        }
                    }
                }
            };

            addMouseWheelListener(mwl);

            ma = new MouseAdapter() {

                public synchronized void mouseClicked(MouseEvent e) {
                    if (lstSquaresMode == true && lstSquaresPoints > 0) {
                        if (a == 0 && blank == 0) {
                            blank = 1;
                            lineConnect = new Line2D.Double[lstSquaresPoints];
                            for (int i = 0; i < lstSquaresPoints; i++) {
                                lineConnect[i] = null;
                            }
                            x_point = new int[lstSquaresPoints * 2];
                            y_point = new int[lstSquaresPoints * 2];
                            x1 = e.getX();
                            y1 = e.getY();
                            x_point[0] = e.getX();
                            y_point[0] = e.getY();
                        }
                        else if (a == (lstSquaresPoints - 1) && blank == 1) {
                            lstSquaresMode = false;
                            blank = 0;
                            for (int i = 0; i < lstSquaresPoints; i++) {
                                lineConnect[i] = null;
                            }
                            lineConnect = null;
                            x_point[2 * a + 1] = e.getX();
                            y_point[2 * a + 1] = e.getY();
                            if (lstSquaresPoints == 1) {
                                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x += (x_point[2 * a + 1] - x_point[2 * a]);
                                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y += (y_point[2 * a + 1] - y_point[2 * a]);
                            }
                            else if (lstSquaresPoints == 2) {
                                double lgdv = imgCurrZoom;
                                // System.out.println(" lgdv " + lgdv);
                                double b4w = bi.getWidth() * imgCurrZoom / 2.0;
                                double b4h = bi.getHeight() * imgCurrZoom / 2.0;
                                // System.out.println("bi width " + bi.getWidth() );
                                // System.out.println( " scaled bi width b4 " +
                                // bi.getWidth()*imgCurrZoom);
                                gdvsim.imgCurrZoom = (1.0 * (x_point[2 * a - 1] - x_point[2 * a + 1]) / (x_point[2 * a - 2] - x_point[2 * a])) * (gdvsim.imgCurrZoom);
                                // System.out.println( " scaled bi width after " +
                                // bi.getWidth()*imgCurrZoom);

                                double afw = bi.getWidth() * imgCurrZoom / 2.0;

                                // System.out.println("Right corner point is " +
                                // (myIntersection.getWidth()/2 + b4w +
                                // (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x
                                // + 4)*lgdv) );

                                // System.out.println("Left corner point is " +
                                // ((myIntersection.getWidth()/2 + b4w +
                                // (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x
                                // + 4)*lgdv) - b4w*2.0));

                                double centerImageX = ((myIntersection.getWidth() / 2 + b4w
                                        + (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x + 4)
                                                * lgdv)
                                        - b4w);

                                double centerImageY = ((myIntersection.getHeight() / 2 + b4h + (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y)
                                        * lgdv) - b4h);

                                // System.out.println("Center point of image is " +
                                // ((myIntersection.getWidth()/2 + b4w +
                                // (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x
                                // + 4)*lgdv) - b4w)); //double dlt = (afw - x_point[2*a -1]) - (b4w
                                // - x_point[2*a -2]);
                                // double dlt = (myIntersection.getWidth()/2.0 - x_point[2*a -1]) -
                                // (myIntersection.getWidth()/2.0 - x_point[2*a -2]);

                                double xdlt = (centerImageX /*
                                                             * + x_point[2*(a+1) -2]
                                                             */ - myIntersection.getWidth() / 2.0);
                                double ydlt = (centerImageY /*
                                                             * + x_point[2*(a+1) -2]
                                                             */ - myIntersection.getHeight() / 2.0);

                                // double dlt = (b4w + x_point[2*(a+1) -2] -
                                // myIntersection.getWidth()/2.0);
                                // System.out.println(" Intersection width " +
                                // myIntersection.getWidth());
                                // System.out.println("Trans x is " +
                                // gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x
                                // + " right corner x is " +x_point[2*(a+1) -2] + " a " + a + " left
                                // corner x is " +x_point[2*(a) -2]);

                                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y -= (ydlt / lgdv);

                                gdvsim.imgCurrZoom = ((myIntersection.getWidth() / 2.0) - x_point[2 * a + 1]) / ((myIntersection.getWidth() / 2.0) - x_point[2 * a]) * (gdvsim.imgCurrZoom);

                                // gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x
                                // +=
                                // ((1.0*x_point[2*a+1]*((x_point[2*a-2] -
                                // x_point[2*a]))/(x_point[2*a -1] - x_point[2*a+1])) -
                                // x_point[2*a])/(gdvsim.imgCurrZoom);
                                // gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x
                                // +=
                                // 1.0*(x_point[2*a+1] - x_point[2*a])/gdvsim.imgCurrZoom;
                                // gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y
                                // +=
                                // ((1.0*y_point[2*a+1]*((x_point[2*a-2] -
                                // x_point[2*a]))/(x_point[2*a -1] - x_point[2*a+1])) -
                                // y_point[2*a])/(gdvsim.imgCurrZoom);
                            }
                            else {
                                gdvsim.imgCurrZoom = ((myIntersection.getWidth() / 2.0) - x_point[2 * a + 1]) / ((myIntersection.getWidth() / 2.0) - x_point[2 * a]) * (gdvsim.imgCurrZoom);
                            }
                            repaint();
                            a = 0;
                        }
                        else {
                            if (blank == 1) {
                                x_point[2 * a + 1] = e.getX();
                                y_point[2 * a + 1] = e.getY();
                                blank = 0;
                                a++;
                            }
                            else {
                                x_point[2 * a] = e.getX();
                                y_point[2 * a] = e.getY();
                                blank = 1;
                            }
                            x1 = e.getX();
                            y1 = e.getY();
                        }
                    }
                }

                public synchronized void mouseReleased(MouseEvent e) {
                    if (gdvsim.rotImg) {
                        gdvsim.rotImg = false;
                        repaint();
                    }

                    if (gdvsim.rotIntsct) {
                        gdvsim.rotIntsct = false;
                        repaint();
                    }
                }

                public synchronized void mousePressed(MouseEvent e) {
                    int shiftmask = InputEvent.SHIFT_DOWN_MASK;
                    int ctrlmask = InputEvent.CTRL_DOWN_MASK;
                    int altmask = InputEvent.ALT_DOWN_MASK;
                    int onmask = InputEvent.BUTTON1_DOWN_MASK;

                    if ((e.getModifiersEx() & onmask) == onmask) {
                        // if( !((e.getModifiersEx() & shiftmask) == shiftmask ) &&
                        // !((e.getModifiersEx() & ctrlmask) == ctrlmask) && !((e.getModifiersEx() &
                        // altmask) == altmask))
                        if ((e.getModifiersEx() & (shiftmask | ctrlmask | altmask)) == 0) {
                            gdvsim.mvImg = true;
                            startX = e.getX();
                            startY = e.getY();
                        }
                        // lse if( ((e.getModifiersEx() & shiftmask) == shiftmask ) &&
                        // !((e.getModifiersEx() & ctrlmask) == ctrlmask) && !((e.getModifiersEx() &
                        // altmask) == altmask))
                        else if ((e.getModifiersEx() & (shiftmask | ctrlmask | altmask)) == shiftmask) {
                            if (gclv_inter.mbov_view_attached_image) {
                                gdvsim.mvImg = true;
                                startX = e.getX();
                                startY = e.getY();
                            }
                        }
                        // else if( !((e.getModifiersEx() & shiftmask) == shiftmask ) &&
                        // ((e.getModifiersEx() & ctrlmask) == ctrlmask) && !((e.getModifiersEx() &
                        // altmask) == altmask))
                        else if ((e.getModifiersEx() & (shiftmask | ctrlmask | altmask)) == ctrlmask) {
                            gdvsim.mvImg = false;
                            gdvsim.rotImg = true;
                            startX = e.getX();
                            startY = e.getY();
                        }
                        else if (((e.getModifiersEx() & altmask) != 0 /* altmask */))
                        // else if( (e.getModifiersEx() & (shiftmask | ctrlmask | altmask)) ==
                        // altmask)
                        {
                            gdvsim.rotIntsct = true;
                            gdvsim.rotImg = true;
                            startX = e.getX();
                            startY = e.getY();
                            // repaint();
                        }
                    }
                }
            };

            mma = new MouseMotionAdapter() {

                public synchronized void mouseMoved(MouseEvent e) {
                    if (lstSquaresMode == true && lstSquaresPoints > 0 && a <= lstSquaresPoints && blank == 1) {
                        deltaX = e.getX() - startX;
                        deltaY = e.getY() - startY;
                        lineConnect[a] = new Line2D.Double(startX, startY, e.getX(), e.getY());
                        repaint();
                    }
                }

                public synchronized void mouseDragged(MouseEvent e) {
                    int shiftmask = InputEvent.SHIFT_DOWN_MASK;
                    int ctrlmask = InputEvent.CTRL_DOWN_MASK;
                    int altmask = InputEvent.ALT_DOWN_MASK;
                    int onmask = InputEvent.BUTTON1_DOWN_MASK;

                    if ((e.getModifiersEx() & onmask) == onmask) {
                        if (!((e.getModifiersEx() & shiftmask) == shiftmask) && !((e.getModifiersEx() & ctrlmask) == ctrlmask)) {
                            gdvsim.mvImg = true;
                            deltaX = e.getX() - startX;
                            deltaY = e.getY() - startY;
                            startX = e.getX();
                            startY = e.getY();
                            curr_screenXShift = (deltaX / (1.0 * myIntersection.getWidth())) + curr_screenXShift;
                            curr_screenYShift = (deltaY / (1.0 * myIntersection.getHeight())) + curr_screenYShift;
                            repaint();
                        }
                        else if (((e.getModifiersEx() & shiftmask) == shiftmask) && !((e.getModifiersEx() & ctrlmask) == ctrlmask)) {
                            if (gclv_inter.mbov_view_attached_image) {
                                gdvsim.mvImg = true;
                                deltaX = e.getX() - startX;
                                deltaY = e.getY() - startY;
                                startX = e.getX();
                                startY = e.getY();
                                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x += deltaX;
                                gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y += deltaY;
                                repaint();
                            }
                        }
                        else if (!((e.getModifiersEx() & shiftmask) == shiftmask) && ((e.getModifiersEx() & ctrlmask) == ctrlmask)) {
                            // gdvsim.mvImg = false;
                            // deltaX = e.getX() - startX;
                            // deltaY = e.getY() - startY;
                            // startX = e.getX();
                            // startY = e.getY();
                            // if(deltaX != 0)
                            // gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_rotation
                            // =
                            // (/*gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_rotation
                            // +*/ Math.toDegrees(Math.atan2(deltaY,deltaX)))%360;

                            double xc = gdvsim.myIntersection.getWidth() / 2.0;
                            double yc = gdvsim.myIntersection.getHeight() / 2.0;
                            xc += gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x; // gdvsim.imgCurrZoom;
                            xc += myIntersection.getWidth() * gdvsim.curr_screenXShift;

                            // gdvsim.imgCurrZoom ,
                            yc += gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y; // gdvsim.imgCurrZoom;
                            yc += myIntersection.getHeight() * gdvsim.curr_screenYShift;

                            // gdvsim.imgCurrZoom ));
                            double x1 = startX;
                            double y1 = startY;
                            double x2 = e.getX();
                            double y2 = e.getY();
                            double diffX = x2 - x1;
                            double diffY = y2 - y1;
                            int sgn = 1;

                            /****
                             * if(diffY < 0 && diffX < 0 && x1 > xc && ) sgn = -1; else if(diffY < 0
                             * && diffX > 0) sgn =-1 ; else if(diffY > 0 && diffX < 0) sgn = 1; else
                             * if(diffY > 0 && diffX > 0) sgn = 1;
                             ****/

                            startX = (int)x2;
                            startY = (int)y2;

                            // startX = e.getX();
                            // startY = e.getY();

                            double x1c = x1 - xc;
                            double x2c = x2 - xc;
                            double y1c = y1 - yc;
                            double y2c = y2 - yc;

                            double slope = (y2 - y1) / (x2 - x1);
                            double intcpt = (y2c * (x2c - x1c) - (y2c - y1c) * x2c) / (x2c - x1c);
                            double xp = -1.0 * intcpt / (2 * slope) + xc;
                            double yp = intcpt / 2 + yc;

                            xp = x1;
                            yp = y1;

                            /****
                             * if(yp < 0 && xp < 0) sgn = -1; else if(yp < 0 && xp > 0) sgn = 1 ;
                             * else if(yp > 0 && xp < 0) sgn = -1; else if(yp > 0 && xp > 0) sgn =
                             * 1; if(x2 >= xp && y2 >= yp && xp >= xc && yp >= yc) sgn = -1; else
                             * if(x1 >= xp && y1 >= yp && xp >= xc && yp >= yc) sgn = 1; else if(x2
                             * >= xp && y2 >= yp && xp <= xc && yp >= yc) sgn = 1; else if(x1 >= xp
                             * && y1 >= yp && xp <= xc && yp >= yc) sgn = -1; else if(x2 >= xp && y2
                             * <= yp && xp <= xc && yp <= yc) sgn = -1; else if(x1 >= xp && y1 <= yp
                             * && xp <= xc && yp <= yc) sgn = 1; else if(x2 >= xp && y2 <= yp && xp
                             * >= xc && yp >= yc) sgn = 1; else if(x1 >= xp && y1 <= yp && xp >= xc
                             * && yp >= yc) sgn = -1; if(x2 >= x1 && y2 >= y1 && xp >= xc && yp >=
                             * yc) sgn = 1; else if(x1 >= x2 && y1 >= y2 && xp >= xc && yp >= yc)
                             * sgn = -1; else if(x2 >= x1 && y2 >= y1 && xp <= xc && yp >= yc) sgn =
                             * -1; else if(x1 >= x2 && y1 >= y2 && xp <= xc && yp >= yc) sgn = 1;
                             * else if(x2 >= x1 && y2 <= y1 && xp <= xc && yp <= yc) sgn = 1; else
                             * if(x1 >= x2 && y1 <= y2 && xp <= xc && yp <= yc) sgn = -1; else if(x2
                             * >= x1 && y2 <= y1 && xp >= xc && yp >= yc) sgn = -1; else if(x1 >= x2
                             * && y1 <= y2 && xp >= xc && yp >= yc) sgn = 1;
                             ***/

                            sgn = -1 * Line2D.relativeCCW(xc, yc, x1, y1, x2, y2);

                            double a_sq = Math.pow((x1 - xc), 2) + Math.pow((y1 - yc), 2);
                            double b_sq = Math.pow((x2 - xc), 2) + Math.pow((y2 - yc), 2);
                            double c_sq = Math.pow((x2 - x1), 2) + Math.pow((y2 - y1), 2);

                            double a = Math.sqrt(a_sq);
                            double b = Math.sqrt(b_sq);
                            double c = Math.sqrt(c_sq);

                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_rotation = (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_rotation
                                    + sgn
                                            * Math.toDegrees(Math.acos((a_sq + b_sq - c_sq) / (2.0 * a * b))))
                                    % 360;
                            // curr_screenXShift = (deltaX/(1.0*myIntersection.getWidth())) +
                            // curr_screenXShift;
                            // curr_screenYShift = (deltaY/(1.0*myIntersection.getHeight())) +
                            // curr_screenYShift;
                            repaint();
                        }
                        else if (((e.getModifiersEx() & altmask) != 0)) {
                            double xc = gdvsim.myIntersection.getWidth() / 2.0;
                            double yc = gdvsim.myIntersection.getHeight() / 2.0;

                            xc += gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x;
                            xc += myIntersection.getWidth() * gdvsim.curr_screenXShift;
                            yc += gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y;
                            yc += myIntersection.getHeight() * gdvsim.curr_screenYShift;

                            double x1 = startX;
                            double y1 = startY;
                            double x2 = e.getX();
                            double y2 = e.getY();
                            double diffX = x2 - x1;
                            double diffY = y2 - y1;

                            int sgn = 1;

                            startX = (int)x2;
                            startY = (int)y2;

                            double x1c = x1 - xc;
                            double x2c = x2 - xc;
                            double y1c = y1 - yc;
                            double y2c = y2 - yc;

                            double slope = (y2 - y1) / (x2 - x1);

                            double intcpt = (y2c * (x2c - x1c) - (y2c - y1c) * x2c) / (x2c - x1c);
                            double xp = -1.0 * intcpt / (2 * slope) + xc;
                            double yp = intcpt / 2 + yc;

                            xp = x1;
                            yp = y1;

                            sgn = -1 * Line2D.relativeCCW(xc, yc, x1, y1, x2, y2);

                            double a_sq = Math.pow((x1 - xc), 2) + Math.pow((y1 - yc), 2);
                            double b_sq = Math.pow((x2 - xc), 2) + Math.pow((y2 - yc), 2);
                            double c_sq = Math.pow((x2 - x1), 2) + Math.pow((y2 - y1), 2);

                            double a = Math.sqrt(a_sq);
                            double b = Math.sqrt(b_sq);
                            double c = Math.sqrt(c_sq);

                            double rotation = sgn * Math.toDegrees(Math.acos((a_sq + b_sq - c_sq) / (2.0 * a * b)));

                            gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_rotation = (gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_rotation
                                    + rotation) % 360;
                            gdvsim.rotAng = rotation;
                            repaint();
                        }
                    }
                }
            };

            addMouseListener(ma);
            addMouseMotionListener(mma);

            // gdvsim.mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
        }

        public void paintComponent(Graphics g) {
            super.paintComponent(g);

            // if(gdvsim.bi != null)
            // ((Graphics2D) g).drawImage(gdvsim.bi, biAffineTransform , gdvsim.mainFrame);
            gclv_inter.calculate_graphics_and_paint(g, myIntersection.getHeight(), myIntersection.getWidth());
        }

        public void draw() {
            repaint();
        }

    } // end of class DrawIntersection

    public static void resetGeometryGlobalValue() {
        for (int lsiv_i = 0; lsiv_i < 6; lsiv_i++) {
            for (int lsiv_j = 0; lsiv_j < 4; lsiv_j++) {
                SDR_legnumber[lsiv_i][lsiv_j] = 0;
                SDR_setback[lsiv_i][lsiv_j] = 0;
                SDR_offset[lsiv_i][lsiv_j] = 0;
            }
        }
    } // end of method resetGeometryGlobalValue

    public static void resetSimulationGlobalValue() {
        flag_simulationData_ok = false;
        flag_greenSequence_ok = false;
        flag_laneControl_ok = false;
        flag_pretimed_ok = false;
        flag_semiact_ok = false;
        flag_fullact_ok = false;
        flag_nema_ok = false;
        flag_diamondTiming_ok = false;
        flag_diamondInterval_ok = false;
        flag_diamondOption_ok = false;
        flag_nemaMovement_ok = false;
        flag_nemaRingGroup_ok = false;
        flag_nemaOverlap_ok = false;
        flag_detDataForNonDiamond_ok = false;
        flag_detDataForDiamond_ok = false;
        flag_detDataForTexdia_ok = false;
        flag_detConnForNema_ok = false;
        flag_detConnForNonNema_ok = false;
        flag_VMSmesg_ok = false;

        gclv_inter.mstv_simdataFile = "";
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det = 0;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_HEADER_NO_OF_DET] = gclv_inter.TX_FROM_USER;
    } // end of resetSimulationGlobalValue

    public static void resetGlobalValue() {
        flag_simulationData_ok = false;
        flag_greenSequence_ok = false;
        flag_laneControl_ok = false;
        flag_pretimed_ok = false;
        flag_semiact_ok = false;
        flag_fullact_ok = false;
        flag_nema_ok = false;
        flag_diamondTiming_ok = false;
        flag_diamondInterval_ok = false;
        flag_diamondOption_ok = false;
        flag_nemaMovement_ok = false;
        flag_nemaRingGroup_ok = false;
        flag_nemaOverlap_ok = false;
        flag_detDataForNonDiamond_ok = false;
        flag_detDataForDiamond_ok = false;
        flag_detDataForTexdia_ok = false;
        flag_detConnForNema_ok = false;
        flag_detConnForNonNema_ok = false;
        flag_VMSmesg_ok = false;
        flag_arc_ok = false;
        flag_line_ok = false;
        flag_drvVehByVeh_ok = false;
        flag_drvVehByDrv_ok = false;
        flag_plotOpt_ok = false;
        flag_specialVehicle_ok = false;
        flag_driverMixData_ok = false;
        flag_driverClass_ok = false;
        flag_vehicleClass_ok = false;

        gclv_inter.mbov_GDV_Data_OK = false;
        gclv_inter.mbov_Diamond_Lane_OK = false;
        gclv_inter.mbov_Diamond_Leg_OK = false;
        gclv_inter.mbov_Free_UTurns_OK = false;

        for (int lsiv_i = 1; lsiv_i <= 6; lsiv_i++) {
            gclv_inter.mboa_Leg_OK[lsiv_i] = false;
            gclv_inter.mboa_Leg_Inbound_Data_OK[lsiv_i] = false;
            gclv_inter.mboa_Leg_Lane_Data_OK[lsiv_i] = false;
            gclv_inter.mboa_Leg_Outbound_Data_OK[lsiv_i] = false;
            gclv_inter.mboa_Leg_SDR_Data_OK[lsiv_i] = false;
            gclv_inter.mboa_Leg_Traffic_Mix_OK[lsiv_i] = false;
        }

        flag_library = false;
        flag_save = false;
        flag_data = false;
        flag_printout = false;
        flag_DXF = false;
        flag_OpenExistGeoFile = false;
        flag_OpenExistSimFile = false;
        flag_OpenNewFile = true;

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_title.mclv_text.mstv_card = null;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_title.mclv_text.mstv_card = null;

        for (int lsiv_i = 0; lsiv_i < 6; lsiv_i++) {
            for (int lsiv_j = 0; lsiv_j < 4; lsiv_j++) {
                SDR_legnumber[lsiv_i][lsiv_j] = 0;
                SDR_setback[lsiv_i][lsiv_j] = 0;
                SDR_offset[lsiv_i][lsiv_j] = 0;
            }
        }

        gclv_inter.setAllInvalid();

        initialization();

        gclv_inter.mstv_gdvdataFile = "";
        gclv_inter.mstv_simdataFile = "";

        TX_Fmt lclv_tx_fmt;

        lclv_tx_fmt = gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gclv_inter.TX_FMT_GDV_PLOT_OPTS];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_path_type = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_option = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_type = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_appr = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_intr = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_max_radius_path = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_min_dist_paths = lclv_tx_fmt.msia_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mdfv_plot_paper_wdth = lclv_tx_fmt.mdfa_def[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH] = gclv_inter.TX_FROM_USER;

        lclv_tx_fmt = gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gclv_inter.TX_FMT_GDV_PAR_OPT_2];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_data = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[1] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[2] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_02];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[3] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_03];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[4] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_04];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[5] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_05];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[6] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_06];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[7] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_07];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[8] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_08];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[9] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_09];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[10] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_10];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[11] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_11];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[12] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_12];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[13] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_13];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[14] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_14];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_veh_cl[15] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_15];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[1] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[2] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_2];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[3] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_3];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[4] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_4];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.msta_sum_drv_cl[5] = lclv_tx_fmt.msta_def[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_5];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_01] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_02] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_03] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_04] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_05] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_06] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_07] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_08] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_09] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_10] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_11] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_12] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_13] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_14] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_VEH_CL_15] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_1] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_2] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_3] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_4] = gclv_inter.TX_FROM_USER;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[gclv_inter.TX_FMT_GDV_PAR_OPT_2_SUM_DRV_CL_5] = gclv_inter.TX_FROM_USER;

        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov = 0;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases = 0;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det = 0;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] = gclv_inter.TX_SET_BY_SOFTWARE;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_HEADER_NO_OF_DET] = gclv_inter.TX_SET_BY_SOFTWARE;

        lclv_tx_fmt = gclv_inter.mclv_tx_mdl_formats.mcla_tx_fmt[gclv_inter.TX_FMT_SIM_PAR_OPT];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_nem2_simul_gapout = lclv_tx_fmt.msta_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dilemma_zone_time_beg = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dilemma_zone_time_end = lclv_tx_fmt.mdfa_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_hitl_sleep_time = lclv_tx_fmt.msia_def[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME];
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] = gdvsim.gclv_inter.TX_DEFAULT;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT] = gdvsim.gclv_inter.TX_DEFAULT;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT] = gdvsim.gclv_inter.TX_DEFAULT;
        gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[gdvsim.gclv_inter.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] = gdvsim.gclv_inter.TX_DEFAULT;

        mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gdvsim.gclv_inter.IVERSN + " - New");
    } // end of resetGlobalValue()

    void ExistingFileAction() {
        if (gclv_inter.mbov_is_diamond_interchange) {
            if ((gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0)
                    || (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)) {
                if (gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 1 has" + gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + " Inbound Lane"
                            + (gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
                if (gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 3 has " + gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + " Outbound Lane"
                            + (gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
                if (gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 4 has" + gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + " Inbound Lane"
                            + (gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
                if (gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 6 has" + gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + " Outbound Lane"
                            + (gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }
        }

        JFileChooser fileChooser = new JFileChooser();

        try {
            File f = new File(new File(".").getCanonicalPath());
            fileChooser.setCurrentDirectory(f);
        }
        catch (IOException e) {}

        fileChooser.setDialogTitle("Open GDV File");

        int resultGDV;
        int resultSIM;
        File fileGDV;
        File fileSIM;

        while (true) {
            resultGDV = fileChooser.showOpenDialog(null);

            if (resultGDV == JFileChooser.CANCEL_OPTION) {
                return;
            }

            fileGDV = fileChooser.getSelectedFile();

            if ((fileGDV == null) || fileGDV.getName().equals("")) {
                JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            if (!fileGDV.exists()) {
                JOptionPane.showMessageDialog(null, "GDV File Does Not Exist", "GDV File Does Not Exist", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            break;
        }

        String filenameGDV = fileGDV.getName();

        gclv_inter.filesReadGDV("" + fileChooser.getCurrentDirectory() + File.separator + filenameGDV);
        if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
            return;
        }

        currZoom = 1.0;
        curr_screenXShift = 0.0;
        curr_screenYShift = 0.0;

        // bs
        gdvsim.fileMenu.add(gdvsim.imageMenu, gdvsim.fileMenu.getMenuComponentCount() - 2);
        gclv_inter.mbov_view_detectors = true;
        gclv_inter.mbov_view_lane_control = true;
        gclv_inter.mbov_view_sdrs = true;
        gclv_inter.mbov_view_turn_movement = true;
        gclv_inter.mbov_view_user_arcs = true;
        gclv_inter.mbov_view_user_lines = true;

        for (int lsiv_leg = 0; lsiv_leg < 6; lsiv_leg++) {
            SDR_count[lsiv_leg] = 0;
            for (int lsiv_sdr = 0; lsiv_sdr < 4; lsiv_sdr++) {
                SDR_legnumber[lsiv_leg][lsiv_sdr] = 0;
                SDR_setback[lsiv_leg][lsiv_sdr] = 0;
                SDR_offset[lsiv_leg][lsiv_sdr] = 0;
            }
        }

        for (int lsiv_sdr = 1; lsiv_sdr <= gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs; lsiv_sdr++) {
            if (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_setback == 0)
                continue;
            int lsiv_leg = gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_leg_number;
            int lsiv_cnt = ++SDR_count[lsiv_leg - 1];
            SDR_legnumber[lsiv_leg - 1][lsiv_cnt - 1] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_leg_number;
            SDR_setback[lsiv_leg - 1][lsiv_cnt - 1] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_setback;
            SDR_offset[lsiv_leg - 1][lsiv_cnt - 1] = gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_offset;
        }

        mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gdvsim.gclv_inter.IVERSN + " - GDV=" + gclv_inter.mstv_gdvdataFile);
        resetSimulationGlobalValue();
        gclv_inter.mbov_GDV_Data_OK = true;
        gclv_inter.mbov_Diamond_Lane_OK = true;
        gclv_inter.mbov_Diamond_Leg_OK = true;
        gclv_inter.mbov_Free_UTurns_OK = true;

        flag_arc_ok = true;
        flag_line_ok = true;
        flag_drvVehByVeh_ok = true;
        flag_drvVehByDrv_ok = true;
        flag_plotOpt_ok = true;
        flag_specialVehicle_ok = true;
        flag_driverMixData_ok = true;
        flag_driverClass_ok = true;
        flag_vehicleClass_ok = true;

        for (int lsiv_i = 1; lsiv_i <= 6; lsiv_i++) {
            gclv_inter.mboa_Leg_OK[lsiv_i] = true;
            gclv_inter.mboa_Leg_Inbound_Data_OK[lsiv_i] = true;
            gclv_inter.mboa_Leg_Lane_Data_OK[lsiv_i] = true;
            gclv_inter.mboa_Leg_Outbound_Data_OK[lsiv_i] = true;
            gclv_inter.mboa_Leg_SDR_Data_OK[lsiv_i] = true;
            gclv_inter.mboa_Leg_Traffic_Mix_OK[lsiv_i] = true;
        }

        windowAfterOpenFile();

        fileChooser.setDialogTitle("Open SIM File");

        while (true) {
            resultSIM = fileChooser.showOpenDialog(null);

            flag_library = false;
            flag_OpenExistGeoFile = true;
            flag_OpenExistSimFile = false;
            flag_OpenNewFile = false;

            if (resultSIM == JFileChooser.CANCEL_OPTION) {
                flag_simulationData_ok = false;
                gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov = 0;
                gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases = 0;
                gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_det = 0;
                gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] = gclv_inter.TX_SET_BY_SOFTWARE;
                gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_HEADER_NO_OF_PHASES] = gclv_inter.TX_SET_BY_SOFTWARE;
                gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[gclv_inter.TX_FMT_SIM_HEADER_NO_OF_DET] = gclv_inter.TX_SET_BY_SOFTWARE;
                myIntersection.setVisible(true);
                myIntersection.draw();
                return;
            }

            fileSIM = fileChooser.getSelectedFile();

            if ((fileSIM == null) || fileSIM.getName().equals("")) {
                JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            if (!fileSIM.exists()) {
                JOptionPane.showMessageDialog(null, "SIM File Does Not Exist", "SIM File Does Not Exist", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            break;
        }

        String filenameSIM = fileSIM.getName();

        gclv_inter.filesReadSIM("" + fileChooser.getCurrentDirectory() + File.separator + filenameSIM);

        if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
            return;
        }

        flag_library = false;
        flag_OpenExistGeoFile = true;
        flag_OpenExistSimFile = true;
        flag_OpenNewFile = false;
        flag_simulationData_ok = true;
        flag_greenSequence_ok = true;
        flag_laneControl_ok = true;
        flag_pretimed_ok = true;
        flag_semiact_ok = true;
        flag_fullact_ok = true;
        flag_nema_ok = true;
        flag_diamondTiming_ok = true;
        flag_diamondInterval_ok = true;
        flag_diamondOption_ok = true;
        flag_nemaMovement_ok = true;
        flag_nemaRingGroup_ok = true;
        flag_nemaOverlap_ok = true;
        flag_detDataForNonDiamond_ok = true;
        flag_detDataForDiamond_ok = true;
        flag_detDataForTexdia_ok = true;
        flag_detConnForNema_ok = true;
        flag_detConnForNonNema_ok = true;
        flag_VMSmesg_ok = true;
        flag_simulationData_ok = true;
        flag_laneControl_ok = true;

        simulationButton.setVisible(true);
        laneControlButton.setVisible(true);

        if (gdvsim.gclv_inter.mbov_is_ic_PRETIMED) {
            intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            intersectionControlButton.setVisible(true);
            intersectionControlButton.setMnemonic(KeyEvent.VK_P);

            flag_pretimed_ok = true;
            flag_greenSequence_ok = true;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT) {
            intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            intersectionControlButton.setVisible(true);
            intersectionControlButton.setMnemonic(KeyEvent.VK_E);

            flag_semiact_ok = true;
            flag_greenSequence_ok = true;

            if (gclv_inter.mbov_is_diamond_interchange) {
                flag_detDataForDiamond_ok = true;
                flag_detConnForNonNema_ok = true;
            }
            else {
                flag_detDataForNonDiamond_ok = true;
                flag_detConnForNonNema_ok = true;
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_FULL_ACT) {
            intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            intersectionControlButton.setVisible(true);
            intersectionControlButton.setMnemonic(KeyEvent.VK_F);

            flag_fullact_ok = true;
            flag_greenSequence_ok = true;

            if (gclv_inter.mbov_is_diamond_interchange) {
                flag_detDataForDiamond_ok = true;
                flag_detConnForNonNema_ok = true;
            }
            else {
                flag_detDataForNonDiamond_ok = true;
                flag_detConnForNonNema_ok = true;
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_TEX_DIA) {
            intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            intersectionControlButton.setVisible(true);
            intersectionControlButton.setMnemonic(KeyEvent.VK_T);

            flag_diamondTiming_ok = true;
            flag_diamondInterval_ok = true;
            flag_diamondOption_ok = true;
            flag_greenSequence_ok = true;
            flag_detDataForTexdia_ok = true;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            intersectionControlButton.setVisible(true);
            intersectionControlButton.setMnemonic(KeyEvent.VK_N);

            flag_nema_ok = true;
            flag_nemaMovement_ok = true;
            flag_nemaRingGroup_ok = true;
            flag_nemaOverlap_ok = true;
            flag_greenSequence_ok = true;

            if (gclv_inter.mbov_is_diamond_interchange) {
                flag_detDataForDiamond_ok = true;
                flag_detConnForNema_ok = true;
            }
            else {
                flag_detDataForNonDiamond_ok = true;
                flag_detConnForNema_ok = true;
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            intersectionControlButton.setText(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control);
            intersectionControlButton.setVisible(true);
            intersectionControlButton.setMnemonic(KeyEvent.VK_H);

            flag_nema_ok = true;
            flag_nemaMovement_ok = true;
            flag_nemaOverlap_ok = true;
            flag_greenSequence_ok = true;

            if (gclv_inter.mbov_is_diamond_interchange) {
                flag_detDataForDiamond_ok = true;
                flag_detConnForNema_ok = true;
            }
            else {
                flag_detDataForNonDiamond_ok = true;
                flag_detConnForNema_ok = true;
            }
        }
        else {
            intersectionControlButton.setVisible(false);
        }

        greenIntervalSequenceButton.setVisible(gdvsim.gclv_inter.mbov_is_ic_signal_controlled);

        detectorButton.setVisible(gdvsim.gclv_inter.mbov_is_ic_detector_data);

        mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gdvsim.gclv_inter.IVERSN + " - GDV=" + gclv_inter.mstv_gdvdataFile + "  SIM=" + gclv_inter.mstv_simdataFile);

        if (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached.equals("YES")) {
            try {
                gdvsim.bi = ImageIO.read(new File(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mstv_image_file_name));
                biAffineTransform = AffineTransform.getTranslateInstance(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_x
                        * gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_scale,
                        gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_translation_y
                                * gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_scale);
                biAffineTransform.rotate(gdvsim.gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_image_file.mdfv_image_file_rotation * Math.PI / 180.0, gdvsim.bi.getWidth() / 2,
                        gdvsim.bi.getHeight() / 2);
                myIntersection.repaint();
                detachImageFileMenuItem.setVisible(true);
            }
            catch (Exception excpt) {
                System.err.println("Error reading Image File: " + excpt);
            }
        }
    } // end of ExistingFileAction

    void SaveAction() {
        if (gclv_inter.mstv_gdvdataFile.length() > 0) {
            if (!CheckGeometryData()) {
                flag_data = false;
                return;
            }

            int lsiv_tot_inb = 0;
            int lsiv_tot_out = 0;
            for (int lsiv_leg = 1; lsiv_leg <= gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs; lsiv_leg++) {
                lsiv_tot_inb += gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
                lsiv_tot_out += gclv_inter.mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out;
            }
            if (lsiv_tot_inb == 0) {
                JOptionPane.showMessageDialog(null, "The total number of inbound lanes is zero.", "Inbound Lane Error", JOptionPane.ERROR_MESSAGE);
                return;
            }
            if (lsiv_tot_out == 0) {
                JOptionPane.showMessageDialog(null, "The total number of outbound lanes is zero.", "Outbound Lane Error", JOptionPane.ERROR_MESSAGE);
                return;
            }

            if (gclv_inter.mbov_is_diamond_interchange) {
                if ((gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0)
                        || (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)) {
                    if (gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                        JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 1 has" + gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb
                                + " Inbound Lane" + (gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "")
                                + " defined.\nThe TEXAS Model does not allow both options.  Please correct.", "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                        return;
                    }
                    if (gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                        JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 3 has " + gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out
                                + " Outbound Lane" + (gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "")
                                + " defined.\nThe TEXAS Model does not allow both options.  Please correct.", "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                        return;
                    }
                    if (gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                        JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 4 has" + gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb
                                + " Inbound Lane" + (gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "")
                                + " defined.\nThe TEXAS Model does not allow both options.  Please correct.", "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                        return;
                    }
                    if (gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                        JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 6 has" + gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out
                                + " Outbound Lane" + (gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "")
                                + " defined.\nThe TEXAS Model does not allow both options.  Please correct.", "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                        return;
                    }
                }
            }

            gclv_inter.filesWriteGDV(gclv_inter.mstv_gdvdataFile);

            if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
                gclv_inter.mstv_gdvdataFile = "";
                flag_save = false;
                return;
            }
            else {
                mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gdvsim.gclv_inter.IVERSN + " - GDV=" + gclv_inter.mstv_gdvdataFile);

                if (gclv_inter.mstv_simdataFile.length() > 0) {
                    if (!CheckSimulationData()) {
                        flag_data = false;
                        return;
                    }

                    gclv_inter.filesWriteSIM(gclv_inter.mstv_simdataFile);

                    if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
                        gclv_inter.mstv_simdataFile = "";
                        flag_save = false;
                        return;
                    }
                    mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gdvsim.gclv_inter.IVERSN + " - GDV=" + gclv_inter.mstv_gdvdataFile + "  SIM=" + gclv_inter.mstv_simdataFile);
                }

                flag_save = true;
                return;
            }
        }

        JFileChooser fileChooser = new JFileChooser();

        try {
            File f = new File(new File(".").getCanonicalPath());
            fileChooser.setCurrentDirectory(f);
        }
        catch (IOException e) {}

        int resultGDV;
        int resultSIM;
        File fileGDV;
        File fileSIM;

        while (true) {
            fileChooser.setDialogTitle("Save As GDV File (<projectname_>gdvdata)");
            resultGDV = fileChooser.showSaveDialog(null);

            if (resultGDV == JFileChooser.CANCEL_OPTION) {
                flag_save = false;
                return;
            }

            fileGDV = fileChooser.getSelectedFile();

            if ((fileGDV == null) || fileGDV.getName().equals("")) {
                JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            if (fileGDV.exists()) {
                if (JOptionPane
                        .showConfirmDialog(null, "GDV File '" + fileGDV.getName() + "' exists.  Would you like to replace the existing file?", "Confirm File Replace",
                                JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                    break;
                }
                else {
                    continue;
                }
            }

            break;
        }

        if (!CheckGeometryData()) {
            flag_data = false;
            return;
        }

        String filenameGDV = fileGDV.getName();

        gclv_inter.filesWriteGDV("" + fileChooser.getCurrentDirectory() + File.separator + filenameGDV);

        if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
            gclv_inter.mstv_gdvdataFile = "";
            flag_save = false;
            return;
        }
        else {
            mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gdvsim.gclv_inter.IVERSN + " - GDV=" + gclv_inter.mstv_gdvdataFile);
            fileChooser.setDialogTitle("Save SIM File");

            while (true) {
                resultSIM = fileChooser.showSaveDialog(null);

                if (resultSIM == JFileChooser.CANCEL_OPTION) {
                    gclv_inter.mstv_simdataFile = "";
                    flag_save = false;
                    return;
                }

                fileSIM = fileChooser.getSelectedFile();

                if ((fileSIM == null) || fileSIM.getName().equals("")) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                if (fileSIM.exists()) {
                    if (JOptionPane.showConfirmDialog(null, "SIM File '" + fileSIM.getName() + "' exists.  Would you like to replace the existing file?", "Confirm File Replace",
                            JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                        break;
                    }
                    else {
                        continue;
                    }
                }

                break;
            }

            if (!CheckSimulationData()) {
                flag_data = false;
                return;
            }

            String filenameSIM = fileSIM.getName();

            gclv_inter.filesWriteSIM("" + fileChooser.getCurrentDirectory() + File.separator + filenameSIM);

            if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
                gclv_inter.mstv_simdataFile = "";
                flag_save = false;
                return;
            }

            mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gdvsim.gclv_inter.IVERSN + " - GDV=" + gclv_inter.mstv_gdvdataFile + "  SIM=" + gclv_inter.mstv_simdataFile);
            flag_save = true;
        }
    } // end of method SaveAction

    void DataAction() {
        if (gclv_inter.mbov_is_diamond_interchange) {
            if ((gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0)
                    || (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)) {
                if (gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 1 has" + gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + " Inbound Lane"
                            + (gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
                if (gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 3 has " + gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + " Outbound Lane"
                            + (gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
                if (gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 4 has" + gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + " Inbound Lane"
                            + (gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
                if (gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 6 has" + gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + " Outbound Lane"
                            + (gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }
        }

        JFileChooser fileChooser = new JFileChooser();

        try {
            File f = new File(new File(".").getCanonicalPath());
            fileChooser.setCurrentDirectory(f);
        }
        catch (IOException e) {}

        fileChooser.setDialogTitle("Save As GDV File (<projectname_>gdvdata)");

        int resultGDV;
        int resultSIM;
        File fileGDV;
        File fileSIM;

        while (true) {
            resultGDV = fileChooser.showSaveDialog(null);

            if (resultGDV == JFileChooser.CANCEL_OPTION) {
                flag_data = false;
                return;
            }

            fileGDV = fileChooser.getSelectedFile();

            if ((fileGDV == null) || fileGDV.getName().equals("")) {
                JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            if (fileGDV.exists()) {
                if (JOptionPane
                        .showConfirmDialog(null, "GDV File '" + fileGDV.getName() + "' exists.  Would you like to replace the existing file?", "Confirm File Replace",
                                JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                    break;
                }
                else {
                    continue;
                }
            }

            break;
        }

        if (!CheckGeometryData()) {
            flag_data = false;
            return;
        }

        String filenameGDV = fileGDV.getName();

        gclv_inter.filesWriteGDV("" + fileChooser.getCurrentDirectory() + File.separator + filenameGDV);

        if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
            gclv_inter.mstv_gdvdataFile = "";
            flag_data = false;
            return;
        }
        else {
            mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gdvsim.gclv_inter.IVERSN + " - GDV=" + gclv_inter.mstv_gdvdataFile);

            fileChooser.setDialogTitle("Save As SIM File (<projectname_>simdata)");

            while (true) {
                resultSIM = fileChooser.showSaveDialog(null);

                if (resultSIM == JFileChooser.CANCEL_OPTION) {
                    gclv_inter.mstv_simdataFile = "";
                    flag_data = false;
                    return;
                }

                fileSIM = fileChooser.getSelectedFile();

                if ((fileSIM == null) || fileSIM.getName().equals("")) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                if (fileSIM.exists()) {
                    if (JOptionPane.showConfirmDialog(null, "SIM File '" + fileSIM.getName() + "' exists.  Would you like to replace the existing file?", "Confirm File Replace",
                            JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                        break;
                    }
                    else {
                        continue;
                    }
                }

                break;
            }

            if (!CheckSimulationData()) {
                flag_data = false;
                return;
            }

            String filenameSIM = fileSIM.getName();

            gclv_inter.filesWriteSIM("" + fileChooser.getCurrentDirectory() + File.separator + filenameSIM);

            if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
                gclv_inter.mstv_simdataFile = "";
                flag_data = false;
                return;
            }
            mainFrame.setTitle("TEXAS Model for Intersection Traffic - gdvsim " + gdvsim.gclv_inter.IVERSN + " - GDV=" + gclv_inter.mstv_gdvdataFile + "  SIM=" + gclv_inter.mstv_simdataFile);
            flag_data = true;
        }
    } // end of method DataAction

    void PrintoutAction() {
        if (gclv_inter.mbov_is_diamond_interchange) {
            if ((gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gclv_inter.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0)
                    || (gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[gclv_inter.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)) {
                if (gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 1 has" + gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + " Inbound Lane"
                            + (gclv_inter.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
                if (gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 3 has " + gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + " Outbound Lane"
                            + (gclv_inter.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
                if (gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 4 has" + gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + " Inbound Lane"
                            + (gclv_inter.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
                if (gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                    JOptionPane.showMessageDialog(null, "Diamond Interchange Free U-Turns are defined and Leg 6 has" + gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + " Outbound Lane"
                            + (gclv_inter.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 1 ? "s" : "") + " defined.\nThe TEXAS Model does not allow both options.  Please correct.",
                            "Diamond Interchange Free U-Turn Warning", JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }
        }

        JFileChooser fileChooser = new JFileChooser();

        try {
            File f = new File(new File(".").getCanonicalPath());
            fileChooser.setCurrentDirectory(f);
        }
        catch (IOException e) {}

        fileChooser.setDialogTitle("Save As Printout GDV File (<projectname_>gdvdata.txt)");

        int resultGDV;
        int resultSIM;
        File fileGDV;
        File fileSIM;
        String filenameGDV;
        String filenameSIM;

        while (true) {
            resultGDV = fileChooser.showSaveDialog(null);

            if (resultGDV == JFileChooser.CANCEL_OPTION) {
                flag_printout = false;
                return;
            }

            fileGDV = fileChooser.getSelectedFile();

            if ((fileGDV == null) || fileGDV.getName().equals("")) {
                JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            filenameGDV = fileGDV.getName();

            if (!filenameGDV.endsWith(".txt")) {
                filenameGDV = filenameGDV.concat(".txt");
                fileGDV = new File("" + fileChooser.getCurrentDirectory() + File.separator + filenameGDV);
                filenameGDV = fileGDV.getName();
            }

            if (fileGDV.exists()) {
                if (JOptionPane.showConfirmDialog(null, "Printout GDV File '" + fileGDV.getName() + "' exists.  Would you like to replace the existing file?", "Confirm File Replace",
                        JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                    break;
                }
                else {
                    continue;
                }
            }

            break;
        }

        if (!CheckGeometryData()) {
            flag_data = false;
            return;
        }

        gclv_inter.filesPrintGDV("" + fileChooser.getCurrentDirectory() + File.separator + filenameGDV);

        if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
            flag_printout = false;
            return;
        }
        else {
            fileChooser.setDialogTitle("Save As Printout SIM File (<projectname_>simdata.txt)");

            while (true) {
                resultSIM = fileChooser.showSaveDialog(null);

                if (resultSIM == JFileChooser.CANCEL_OPTION) {
                    flag_printout = false;
                    return;
                }

                fileSIM = fileChooser.getSelectedFile();

                if ((fileSIM == null) || fileSIM.getName().equals("")) {
                    JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                    continue;
                }

                filenameSIM = fileSIM.getName();

                if (!filenameSIM.endsWith(".txt")) {
                    filenameSIM = filenameSIM.concat(".txt");
                    fileSIM = new File("" + fileChooser.getCurrentDirectory() + File.separator + filenameSIM);
                    filenameSIM = fileSIM.getName();
                }

                if (fileSIM.exists()) {
                    if (JOptionPane.showConfirmDialog(null, "Printout SIM File '" + fileSIM.getName() + "' exists.  Would you like to replace the existing file?", "Confirm File Replace",
                            JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                        break;
                    }
                    else {
                        continue;
                    }
                }

                break;
            }

            if (!CheckSimulationData()) {
                flag_data = false;
                return;
            }

            gclv_inter.filesPrintSIM("" + fileChooser.getCurrentDirectory() + File.separator + filenameSIM);

            if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
                flag_printout = false;
                return;
            }

            flag_printout = true;
            saveMenuItem.setEnabled(true);
        }
    } // end of method PrintoutAction

    void SaveAsDXFAction() {
        JFileChooser fileChooser = new JFileChooser();

        try {
            File f = new File(new File(".").getCanonicalPath());
            fileChooser.setCurrentDirectory(f);
        }
        catch (IOException e) {}

        fileChooser.setDialogTitle("Save As DXF File (<projectname_>drawing.dxf)");

        int resultDXF;
        File fileDXF;
        String filenameDXF;

        while (true) {
            resultDXF = fileChooser.showSaveDialog(null);

            if (resultDXF == JFileChooser.CANCEL_OPTION) {
                flag_DXF = false;
                return;
            }

            fileDXF = fileChooser.getSelectedFile();

            if ((fileDXF == null) || fileDXF.getName().equals("")) {
                JOptionPane.showMessageDialog(null, "Invalid File Name", "Invalid File Name", JOptionPane.ERROR_MESSAGE);
                continue;
            }

            filenameDXF = fileDXF.getName();

            if (!filenameDXF.endsWith(".dxf")) {
                filenameDXF = filenameDXF.concat(".dxf");
                fileDXF = new File("" + fileChooser.getCurrentDirectory() + File.separator + filenameDXF);
                filenameDXF = fileDXF.getName();
            }

            if (fileDXF.exists()) {
                if (JOptionPane
                        .showConfirmDialog(null, "DXF File '" + fileDXF.getName() + "' exists.  Would you like to replace the existing file?", "Confirm File Replace",
                                JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                    break;
                }
                else {
                    continue;
                }
            }

            break;
        }

        if (!CheckGeometryData()) {
            flag_data = false;
            return;
        }

        if (!CheckSimulationData()) {
            flag_data = false;
            return;
        }

        gclv_inter.filesWriteDXF("" + fileChooser.getCurrentDirectory() + File.separator + filenameDXF);

        if (gclv_inter.msiv_returnCode != gclv_inter.RETURN_SUCCESS) {
            flag_DXF = false;
            return;
        }

        flag_DXF = true;

    } // end of method SaveAsDXFAction

    boolean CheckGeometryData() {
        if (!gclv_inter.mbov_GDV_Data_OK) {
            JOptionPane.showMessageDialog(null, "GDV Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        if (gclv_inter.mbov_is_diamond_interchange) {
            if (!gclv_inter.mbov_Diamond_Lane_OK) {
                JOptionPane.showMessageDialog(null, "Diamond Lane Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (!gclv_inter.mbov_Diamond_Leg_OK) {
                JOptionPane.showMessageDialog(null, "Diamond Leg Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (!gclv_inter.mbov_Free_UTurns_OK) {
                JOptionPane.showMessageDialog(null, "Diamond Free UTurn Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }

        for (int lsiv_leg = 1; lsiv_leg <= gclv_inter.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs; lsiv_leg++) {
            if (!gclv_inter.mboa_Leg_Inbound_Data_OK[lsiv_leg]) {
                JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " Inbound Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (!gclv_inter.mboa_Leg_Lane_Data_OK[lsiv_leg]) {
                JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " Lane Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (!gclv_inter.mboa_Leg_Outbound_Data_OK[lsiv_leg]) {
                JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " Outbound Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (!gclv_inter.mboa_Leg_SDR_Data_OK[lsiv_leg]) {
                JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " SRD Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (!gclv_inter.mboa_Leg_Traffic_Mix_OK[lsiv_leg]) {
                JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " Traffic Mix Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }

            if (!gclv_inter.mboa_Leg_OK[lsiv_leg]) {
                JOptionPane.showMessageDialog(null, "Leg " + lsiv_leg + " Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }

        return true;
    } // end of method CheckGeometryData()

    boolean CheckSimulationData() {
        if (!flag_simulationData_ok) {
            JOptionPane.showMessageDialog(null, "Simulation Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        if (!flag_laneControl_ok) {
            JOptionPane.showMessageDialog(null, "Lane Control Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        if (gdvsim.gclv_inter.mbov_is_ic_PRETIMED) {
            if (!flag_pretimed_ok) {
                JOptionPane.showMessageDialog(null, "Pretimed Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_greenSequence_ok) {
                JOptionPane.showMessageDialog(null, "Green Interval Sequence Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_SEMI_ACT) {
            if (!flag_greenSequence_ok) {
                JOptionPane.showMessageDialog(null, "Green Interval Sequence Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_semiact_ok) {
                JOptionPane.showMessageDialog(null, "Semiact Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_detConnForNonNema_ok) {
                JOptionPane.showMessageDialog(null, "Detector Connection has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (gclv_inter.mbov_is_diamond_interchange) {
                if (!flag_detDataForDiamond_ok) {
                    JOptionPane.showMessageDialog(null, "Detector Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
            else {
                if (!flag_detDataForNonDiamond_ok) {
                    JOptionPane.showMessageDialog(null, "Detector Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_FULL_ACT) {
            if (!flag_greenSequence_ok) {
                JOptionPane.showMessageDialog(null, "Green Interval Sequence Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_fullact_ok) {
                JOptionPane.showMessageDialog(null, "Fullact Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_detConnForNonNema_ok) {
                JOptionPane.showMessageDialog(null, "Detector Connection has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (gclv_inter.mbov_is_diamond_interchange) {
                if (!flag_detDataForDiamond_ok) {
                    JOptionPane.showMessageDialog(null, "Detector Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
            else {
                if (!flag_detDataForNonDiamond_ok) {
                    JOptionPane.showMessageDialog(null, "Detector Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_TEX_DIA) {
            if (!flag_greenSequence_ok) {
                JOptionPane.showMessageDialog(null, "Green Interval Sequence Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_diamondTiming_ok) {
                JOptionPane.showMessageDialog(null, "Texas Diamond Timing Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_diamondInterval_ok) {
                JOptionPane.showMessageDialog(null, "Texas Diamond Special Interval Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_diamondOption_ok) {
                JOptionPane.showMessageDialog(null, "Green Interval Option Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_detDataForTexdia_ok) {
                JOptionPane.showMessageDialog(null, "Detector Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            if (!flag_greenSequence_ok) {
                JOptionPane.showMessageDialog(null, "Green Interval Sequence Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_nema_ok) {
                JOptionPane.showMessageDialog(null, "NEMA Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_nemaMovement_ok) {
                JOptionPane.showMessageDialog(null, "NEMA Movement Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_nemaRingGroup_ok) {
                JOptionPane.showMessageDialog(null, "NEMA RingGroup Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_nemaOverlap_ok) {
                JOptionPane.showMessageDialog(null, "NEMA Overlap Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_detConnForNema_ok) {
                JOptionPane.showMessageDialog(null, "Detector Connection has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (gclv_inter.mbov_is_diamond_interchange) {
                if (!flag_detDataForDiamond_ok) {
                    JOptionPane.showMessageDialog(null, "Detector Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
            else {
                if (!flag_detDataForNonDiamond_ok) {
                    JOptionPane.showMessageDialog(null, "Detector Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            if (!flag_greenSequence_ok) {
                JOptionPane.showMessageDialog(null, "Green Interval Sequence Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_nemaMovement_ok) {
                JOptionPane.showMessageDialog(null, "NEMA/HARDWARE Movement Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_nemaOverlap_ok) {
                JOptionPane.showMessageDialog(null, "NEMA/HARDWARE Overlap Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (!flag_detConnForNema_ok) {
                JOptionPane.showMessageDialog(null, "Detector Connection has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if (gclv_inter.mbov_is_diamond_interchange) {
                if (!flag_detDataForDiamond_ok) {
                    JOptionPane.showMessageDialog(null, "Detector Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
            else {
                if (!flag_detDataForNonDiamond_ok) {
                    JOptionPane.showMessageDialog(null, "Detector Data has not been accepted.", "Invalid Data", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
        }

        return true;

    } // end of CheckSimulationData

    class gdvZoomDialog extends JDialog implements ActionListener, KeyListener {

        public JLabel label;

        public JRadioButton rb1;

        public JRadioButton rb2;

        public JRadioButton rb3;

        public JSpinner jsp;

        public JButton buttonOK;

        public JButton buttonCancel;

        public Frame parent;

        public double zoomValue;

        public ChangeListener sl;

        public gdvZoomDialog(Frame p) {
            super(p, "Zoom ...", true);
            parent = p;
            label = new JLabel("Zoom to");

            rb1 = new JRadioButton();
            rb1.setText("200%");
            rb1.setActionCommand("2.0");
            rb1.addActionListener(this);
            rb1.addKeyListener(this);

            rb2 = new JRadioButton();
            rb2.setText("100%");
            rb2.setActionCommand("1.0");
            rb2.addActionListener(this);
            rb2.addKeyListener(this);

            rb3 = new JRadioButton();
            rb3.setText("75%");
            rb3.setActionCommand("0.75");
            rb3.addActionListener(this);
            rb3.addKeyListener(this);

            ButtonGroup bg = new ButtonGroup();
            bg.add(rb1);
            bg.add(rb2);
            bg.add(rb3);

            zoomValue = currZoom;
            jsp = new JSpinner(new SpinnerNumberModel(new Double(currZoom), null, null, new Double(0.01)));
            jsp.setValue(new Double(currZoom));
            zoomValue = currZoom;

            JComponent editor = jsp.getEditor();
            if (editor instanceof JSpinner.DefaultEditor) {
                DecimalFormat format2Dec = new DecimalFormat("0.00%");
                ((JSpinner.DefaultEditor)editor).getTextField().setFormatterFactory(new DefaultFormatterFactory(new NumberFormatter(format2Dec)));
                ((JSpinner.DefaultEditor)editor).getTextField().addKeyListener(this);
                ((JSpinner.DefaultEditor)editor).getTextField().setText(format2Dec.format((jsp.getValue())));
            }

            sl = new ChangeListener() {

                public synchronized void stateChanged(ChangeEvent e) {
                    JSpinner source = (JSpinner)e.getSource();
                    if (source.getValue() instanceof Long) {
                        zoomValue = (double)(((Long)source.getValue()).longValue());
                        source.setValue(new Double(zoomValue));
                    }
                    else {
                        zoomValue = ((Double)source.getValue()).doubleValue();
                    }
                }
            };

            jsp.addChangeListener(sl);
            ((JSpinner.DefaultEditor)jsp.getEditor()).getTextField().addKeyListener(this);

            Box box = Box.createVerticalBox();
            box.add(label);
            box.add(Box.createVerticalStrut(5));
            box.add(rb1);
            box.add(rb2);
            box.add(rb3);
            box.add(jsp);
            box.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

            buttonOK = new JButton("OK");
            buttonOK.addActionListener(this);
            buttonOK.addKeyListener(this);

            buttonCancel = new JButton("Cancel");
            buttonCancel.addActionListener(this);
            buttonCancel.addKeyListener(this);

            JPanel panel = new JPanel();
            panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
            panel.add(buttonOK);
            panel.add(buttonCancel);

            Container contentPane = getContentPane();
            contentPane.add(box, BorderLayout.CENTER);
            contentPane.add(panel, BorderLayout.SOUTH);

            pack();
            setResizable(false);
            setLocationRelativeTo(parent);
            setVisible(true);
        }

        public synchronized void actionPerformed(ActionEvent e) {
            Object source = e.getSource();

            if (source != buttonOK && source != buttonCancel) {
                zoomValue = Double.parseDouble(e.getActionCommand());
                JComponent editor = jsp.getEditor();
                DecimalFormat format2Dec = new DecimalFormat("0.00%");
                ((JSpinner.DefaultEditor)editor).getTextField().setText(format2Dec.format((new Double(zoomValue))));
            }

            if (source == buttonOK) {
                currZoom = zoomValue;
                setVisible(false);
                myIntersection.repaint();
                dispose();
            }
            else if (source == buttonCancel) {
                setVisible(false);
                dispose();
            }
        }

        public synchronized void keyReleased(KeyEvent k) {
            // Necessary to implement the KeyListener interface
        }

        public synchronized void keyPressed(KeyEvent k) {
            char t = ((char)(k.getKeyCode()));
            String st = String.valueOf(t);

            if (k.getSource() instanceof JFormattedTextField) {
                JFormattedTextField source = (JFormattedTextField)k.getSource();
                String sourceSt = source.getText();
                String s = sourceSt + st;
                try {
                    zoomValue = Double.valueOf(s.endsWith("%") ? s.substring(0, (s.indexOf("%"))) : s) / 100.0;
                }
                catch (Exception expt) {}
            }

            if (KeyEvent.VK_ENTER == k.getKeyCode() && k.getSource() instanceof JFormattedTextField) {
                JFormattedTextField source = (JFormattedTextField)k.getSource();
                String s = source.getText();
                try {
                    zoomValue = Double.valueOf(s.endsWith("%") ? s.substring(0, (s.indexOf("%"))) : s) / 100.0;
                }
                catch (Exception expt) {}
                currZoom = zoomValue;
                setVisible(false);
                myIntersection.repaint();
                dispose();
            }
            else if (KeyEvent.VK_ENTER == k.getKeyCode()) {
                Object source = k.getSource();

                if (source instanceof JButton && source == buttonCancel) {
                    setVisible(false);
                    dispose();
                }
                else {
                    currZoom = zoomValue;
                    setVisible(false);
                    myIntersection.repaint();
                    dispose();
                }
            }
            else if (KeyEvent.VK_ESCAPE == k.getKeyCode()) {
                setVisible(false);
                dispose();
            }
            else if (KeyEvent.VK_UP == k.getKeyCode()) {
                Object source = k.getSource();

                if (source == rb2)
                    rb1.requestFocusInWindow();
                if (source == rb3)
                    rb2.requestFocusInWindow();
            }
            else if (KeyEvent.VK_DOWN == k.getKeyCode()) {
                Object source = k.getSource();
                if (source == rb1)
                    rb2.requestFocusInWindow();
                if (source == rb2)
                    rb3.requestFocusInWindow();
            }
        }

        public synchronized void keyTyped(KeyEvent k) {
            // Necessary to implement the KeyListener interface
        }

    } // end of class gdvZoomDialog
} // end of class gdvsim

/******************************************************************************/
/* gdvsim.java */
/******************************************************************************/
