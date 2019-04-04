/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */

package org.etexascode.ntcip1202.parameter;

import java.io.Serializable;

/**
 * Data structure for Unit objects
 * 
 * @author dranker
 */
public class Unit implements Serializable {

    // NOT MAPPED
    /**
     * Unit Start up Flash time parameter in seconds (0 to 255 sec). The period/state (Start-Up
     * Flash occurs when power is restored following a device defined power interruption. During the
     * Start-Up Flash state, the Fault Monitor and Voltage Monitor outputs shall be inactive (if
     * present).
     */
    private int unitStartUpFlash;

    // NOT MAPPED
    /**
     * Unit Automatic Ped Clear parameter (1 = False/Disable 2=True/Enable). When enabled, the CU
     * shall time the Pedestrian Clearance interval when Manual Control Enable is active and prevent
     * the Pedestrian Clearance interval from being terminated by the Interval Advance input.
     */
    private int unitAutoPedestrianClear;

    // NOT MAPPED
    /**
     * The Backup Time in seconds (0-65535 sec). When any of the defined system control parameters
     * is SET, the backup timer is reset. After reset it times the unitBackupTime interval. If the
     * unitBackupTime interval expires without a SET operation to any of the system control
     * parameters, then the CU shall revert to Backup Mode. A value of zero (0) for this object
     * shall disable this feature. The system control parameters are: phaseControlGroupPhaseOmit,
     * phaseControlGroupPedOmit, phaseControlGroupHold, phaseControlGroupForceOff,
     * phaseControlGroupVehCall, phaseControlGroupPedCall, systemPatternControl, systemSyncControl,
     * preemptControlState, ringControlGroupStopTime, ringControlGroupForceOff,
     * ringControlGroupMax2, ringControlGroupMaxInhibit, ringControlGroupPedRecycle,
     * ringControlGroupRedRest, ringControlGroupOmitRedClear, unitControl,
     * specialFunctionOutputState (deprecated),
     */
    private int unitBackupTime;

    // NOT MAPPED
    /**
     * The red revert in tenth seconds ( 0.0 - 25.5 sec). This value shall provide the minimum red
     * revert time for all phases (i.e. if it is greater than a phaseRedRevert object value, then
     * this value shall be used as the red revert time for the affected phase). This object provides
     * a minimum Red indication following the Yellow Change interval and prior to the next display
     * of Green on the same signal output driver group.
     */
    private int unitRedRevert;

    // NOT MAPPED
    /**
     * The Control Mode for Pattern, Flash, or Free at the device: other: control by a source other
     * than those listed here. systemControl: control by master or central commands. systemStandby:
     * control by local based on master or central command to use local control. backupMode: Backup
     * Mode (see Terms). manual: control by entry other than zero in coordOperationalMode. timebase:
     * control by the local Time Base. interconnect: control by the local Interconnect inputs.
     * interconnectBackup: control by local TBC due to invalid Interconnect inputs or loss of sync.
     */
    private int unitControlStatus;

    // NOT MAPPED
    /**
     * The Flash modes: other: the CU is in flash for some other reason. notFlash: the CU is not in
     * Flash automatic: the CU is currently in an Automatic Flash state. localManual: the Controller
     * Unit Local Flash input is active, MMU Flash input is not active, and Flash is not commanded
     * by the Master. faultMonitor: the CU is currently in a Fault Monitor State. mmu: the
     * Controller Unit MMU Flash input is active and the CU is not in Start-Up Flash. startup: the
     * CU is currently timing the Start-Up Flash period. preempt: the CU is currently timing the
     * preempt Flash.
     */
    private int unitFlashStatus;

    // NOT MAPPED
    /**
     * Device Alarm Mask 2 ( 0 = False, 1 = True) as follows: Bit 7: Reserved. Bit 6: Reserved. Bit
     * 5: Offset Transitioning - Whenever the CU is performing an offset transition (correction in
     * process) Bit 4: Stop Time - When either CU Stop Time Input becomes active. Bit 3: External
     * Start - When the CU External Start becomes active. Bit 2: Response Fault - When any NEMA TS2
     * Port 1 response frame fault occurs. Bit 1: Low Battery - When any battery voltage falls below
     * the required level. Bit 0: Power Restart - When power returns after a power interruption.
     * Once set, a bit shall maintain it’s state as long as the condition exists. Bit 0 (Power
     * Restart) status shall be
     */
    private int unitAlarmStatus2;

    // NOT MAPPED
    /**
     * Device Alarm Mask 1 ( 0 = False, 1 = True) as follows: Bit 7: CoordActive - When coordination
     * is active and not preempted or overridden. Bit 6: Local Free - When any of the CU inputs
     * and/or programming cause it not to run coordination. Bit 5: Local Flash - When the Controller
     * Unit Local Flash input becomes active, MMU Flash input is not active, and Flash is not
     * commanded by the system. Bit 4: MMU Flash - When the Controller Unit MMU Flash input remains
     * active for a period of time exceeding the Start-Up Flash time. Bit 3: Cycle Fail - When a
     * local Controller Unit is operating in the non-coordinated mode, whether the result of a Cycle
     * Fault or Free being the current normal mode, and cycling diagnostics indicate that a
     * serviceable call exists that has not been serviced for two cycles. Bit 2: Coord Fail - When a
     * Coord Fault is in effect and a Cycle Fault occurs again within two cycles of the coordination
     * retry. Bit 1: Coord Fault - When a Cycle Fault is in effect and the serviceable call has been
     * serviced within two cycles after the Cycle Fault. Bit 0: Cycle Fault - When the Controller
     * Unit is operating in the coordinated mode and cycling diagnostics indicate that a serviceable
     * call exists that has not been serviced for two cycles. Once set, a bit shall maintain it’s
     * state as long as the condition exists.
     */
    private int unitAlarmStatus1;

    // NOT MAPPED
    /**
     * Short Alarm Mask ( 0 = False, 1 = True) as follows: Bit 7: Critical Alarm - When the Stop
     * Time input is active. Bit 6: Non-Critical Alarm - When an physical alarm input is active. Bit
     * 5: Detector Fault - When any detectorAlarm fault occurs. Bit 4: Coordination Alarm - When the
     * CU is not running the called pattern without offset correction within three cycles of the
     * command. An offset correction requiring less than three cycles due to cycle overrun caused by
     * servicing a pedestrian call shall not cause a Coordination Alarm. Bit 3: Local Override -
     * When any of the CU inputs and/or programming cause it not to run coordination. Bit 2: Local
     * Cycle Zero - When running coordinated and the Coord Cycle Status (coordCycleStatus) has
     * passed through zero. Bit 1: T&F Flash - When either the Local Flash or MMU Flash input
     * becomes active. Bit 0: Preempt - When any of the CU Preempt inputs become active. Once set, a
     * bit shall maintain it’s state as long as the condition exists. Bit 2 (Local Cycle Zero)
     * status shall be maintained until a READ of this object occurs.
     */
    private int shortAlarmStatus;

    // NOT MAPPED
    /**
     * This object is used to allow a remote entity to activate unit functions in the device ( 0 =
     * False / Disabled, 1 = True / Enabled) as follows: Bit 7: Dimming Enable - when set to 1,
     * causes channel dimming to operate as configured. For dimming to occur, (this control OR a
     * dimming input) AND a 'timebaseAscAuxillaryFunction' must be True. REFERENCE NEMA TS 2 Clause
     * 3.9.2 Bit 6: Interconnect - when set to 1, shall cause the interconnect inputs to operate at
     * a higher priority than the timebase control (TBC On Line). REFERENCE NEMA TS 2 Clause 3.6.2.3
     * and 3.8.3 Bit 5: Walk Rest Modifier - when set to 1, causes non-actuated phases to remain in
     * the timed-out Walk state (rest in Walk) in the absence of a serviceable conflicting call.
     * REFERENCE NEMA TS 2 Clause 3.5.5.5.13 Bit 4: Call to Non-Actuated 2 - when set to 1, causes
     * any phase(s) appropriately programmed in the phaseOptions object to operate in the
     * Non-Actuated Mode. REFERENCE NEMA TS 2 Clause 3.5.5.5.8 Bit 3: Call to Non-Actuated 1 - when
     * set to 1, causes any phase(s) appropriately programmed in the phaseOptions object to operate
     * in the Non-Actuated Mode. REFERENCE NEMA TS 2 Clause 3.5.5.5.8 Bit 2: External Minimum Recall
     * - when set to 1, causes a recurring demand on all vehicle phases for a minimum vehicle
     * service. REFERENCE NEMA TS 2 Clause 3.5.5.5.9 Bit 1: Reserved Bit 0: Reserved When a bit = 1,
     * the device shall activate the Unit control. When a bit = 0, the device shall not activate the
     * Unit control. A SET of a 'reserved' bit to a value other than zero (0) shall return a
     * badValue(3) error. The device shall reset this object to ZERO when in BACKUP Mode. A write to
     * this object shall reset the BACKUP timer (see unitBackupTime).
     */
    private int unitControl;

    // NOT MAPPED
    /**
     * This object contains the maximum number of alarm groups (8 alarm inputs per group) this
     * device supports. This object indicates the maximum rows which shall appear in the
     * alarmGroupTable object.
     */
    private int maxAlarmGroups;

    // NOT MAPPED
    /**
     * The Maximum Number of Special Functions this Actuated Controller Unit supports.
     */
    private int maxSpecialFunctionOutputs;

    private AlarmGroupTable[] alarmGroups;

    private SpecialFunctionOutputTable[] specialFunctions;

    public Unit() {
        alarmGroups = new AlarmGroupTable[maxAlarmGroups];
        specialFunctions = new SpecialFunctionOutputTable[maxSpecialFunctionOutputs];
    }

    public class AlarmGroupTable {

        /**
         * The alarm group number for objects in this row. This value shall not exceed the
         * maxAlarmGroups object value.
         */
        private int alarmGroupNumber;

        // NOT MAPPED
        /**
         * Alarm input state bit field. When a bit = 1, the associated physical alarm input is
         * active. When a bit = 0, the associated alarm input is NOT active. Bit 7: Alarm Input # =
         * ( alarmGroupNumber * 8) Bit 6: Alarm Input # = ( alarmGroupNumber * 8) -1 Bit 5: Alarm
         * Input # = ( alarmGroupNumber * 8) -2 Bit 4: Alarm Input # = ( alarmGroupNumber * 8) -3
         * Bit 3: Alarm Input # = ( alarmGroupNumber * 8) -4 Bit 2: Alarm Input # = (
         * alarmGroupNumber * 8) -5 Bit 1: Alarm Input # = ( alarmGroupNumber * 8) -6 Bit 0: Alarm
         * Input # = ( alarmGroupNumber * 8) -7
         */
        private int alarmGroupState;
    }

    public class SpecialFunctionOutputTable {

        // NOT MAPPED

        /**
         * The special function output number associated with object in this row. This value shall
         * not exceed the maxSpecialFunctionOutputs object value.
         */
        private int specialFunctionOutputNumber;

        // NOT MAPPED
        /**
         * The special function output (logical or physical) in the device may be controlled by this
         * object. 0 = OFF & 1 = ON The device shall reset this object to ZERO when in BACKUP Mode.
         * A write to this object shall reset the BACKUP timer (see unitBackupTime).
         */
        private int specialFunctionOutputControl;

        // NOT MAPPED
        /**
         * The current status (ON-OFF) of the special function output (logical or physical) in the
         * device. 0 = OFF & 1 = ON
         */
        private int speicalFunctionOutputStatus;
    }
}
