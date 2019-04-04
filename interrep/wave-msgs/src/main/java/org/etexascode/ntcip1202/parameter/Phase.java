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
 * This node shall contain objects that configure, monitor, or control phase functions for this
 * device.
 * 
 * @author dranker
 */
public class Phase implements Serializable {

    /**
     * The Maximum Number of Phases this Actuated Controller Unit supports. This object indicates
     * the maximum rows which shall appear in the phaseTable object.
     */
    private int maxPhases;

    /**
     * The Maximum Number of Phase Groups (8 Phases per group) this Actuated Controller Unit
     * supports. This value is equal to TRUNCATE [(maxPhases + 7) / 8]. This object indicates the
     * maximum rows which shall appear in the phaseStatusGroupTable and phaseControlGroupTable.
     */
    private int maxPhaseGroups;

    // Array of PhaseEntrys
    private PhaseEntry[] entries;

    /** Array of PhaseStatusGroupEntrys */
    private PhaseGroupTable[] groups;

    private PhaseControlTable[] control;

    public Phase() {
        entries = new PhaseEntry[maxPhases];

        groups = new PhaseGroupTable[maxPhaseGroups];

        control = new PhaseControlTable[maxPhaseGroups];
    }

    public class PhaseEntry {

        /**
         * The phase number for objects in this row. This value shall not exceed the maxPhases
         * object value.
         */
        private int phaseNumber;

        /**
         * Phase Walk Parameter in seconds. This shall control the amountof time the Walk indication
         * shall be displayed.
         */
        private int phaseWalk;

        /**
         * Phase Pedestrian Clear Parameter in seconds. This shall control the duration of the
         * Pedestrian Clearance output (if present) and the flashing period of the Don’t Walk
         * output.
         */
        private int phasePedestrianClear;

        /**
         * Phase Minimum Green Parameter in seconds (NEMA TS 2 range: 1-255 sec). The first timed
         * portion of the Green interval which may be set in consideration of the storage of
         * vehicles between the zone of detection for the approach vehicle detector(s) and the stop
         * line.
         */
        private int phaseMinimumGreen;

        /**
         * Phase Passage Parameter in tenth seconds (0-25.5 sec). Passage Time, Vehicle Interval,
         * Preset Gap, Vehicle Extension: the extensible portion of the Green shall be a function of
         * vehicle actuations that occur during the Green interval. The phase shall remain in the
         * extensible portion of the Green interval as long as the passage timer is not timed out.
         * The timing of this portion of the green interval shall be reset with each subsequent
         * vehicle actuation and shall not commence to time again until the vehicle actuation is
         * removed.
         */
        private int phasePassage;

        /**
         * Phase Maximum 1 Parameter in seconds (NEMA TS 2 range: 1-255 sec). This time setting
         * shall determine the maximum length of time this phase may be held Green in the presence
         * of a serviceable conflicting call. In the absence of a serviceable conflicting call the
         * Maximum Green timer shall be held reset unless Max Vehicle Recall is enabled for this
         * phase. This is the default maximum value to use. It may be overridden via an external
         * input, coordMaximumMode or other method.
         */
        private int phaseMaximum1;

        /**
         * Phase Maximum 2 Parameter in seconds (NEMA TS 2 range: 1-255 sec). This time setting
         * shall determine the maximum length of time this phase may be held Green in the presence
         * of a serviceable conflicting call. In the absence of a serviceable conflicting call the
         * Maximum Green timer shall be held reset unless Max Vehicle Recall is enabled for this
         * phase. This may be implemented as the max green timer via an external input,
         * coordMaximumMode or other method.
         */
        private int phaseMaximum2;

        /**
         * Phase Yellow Change Parameter in tenth seconds (NEMA TS 2 range: 3-25.5 sec). Following
         * the Green interval of each phase the CU shall provide a Yellow Change interval which is
         * timed according to the Yellow Change parameter for that phase.
         */
        private int phaseYellowChange;

        /**
         * Phase Red Clearance Parameter in tenth seconds (0-25.5 sec).Following the Yellow Change
         * interval for each phase, the CU shall provide a Red Clearance interval which is timed
         * according to the Red Clearance parameter for that phase.
         */
        private int phaseRedClear;

        /**
         * Red revert time parameter in tenth seconds . A minimum Red indication to be timed
         * following the Yellow Change interval and prior to the next display of Green on the same
         * signal output driver group. The unitRedRevert parameter shall act as a minimum red revert
         * time for all signal displays. The phaseRedRevert parameter may increase the red revert
         * time for a specific phase. If the phaseRedRevert parameter is less than the unitRedRevert
         * the unitRedRevert time shall be used.
         */
        private int phaseRedRevert;

        /**
         * Phase Added Initial Parameter in tenths of seconds (0-25.5 sec). Added Initial parameter
         * (Seconds / Actuation) shall determine the time by which the variable initial time period
         * will be increased from zero with each vehicle actuation received during the associated
         * phase Yellow and Red intervals.
         */
        private int phaseAddedInitial;

        /**
         * Phase Maximum Initial Parameter in seconds (0-255 sec). The maximum value of the variable
         * initial timing period. Variable Initial timing shall equal the lesser of [added initial
         * (seconds / actuation) * number of actuations] or [ Max Initial ]. The variable initial
         * time shall not be less than Minimum Green.
         */
        private int phaseMaximumInitial;

        /**
         * Phase Time Before Reduction (TBR) Parameter in seconds (0-255 sec). The Time Before NTCIP
         * 1202:2005 v02.19 Page 15 © 2005 AASHTO / ITE / NEMA Copy Per MIB Distribution Notice
         * Reduction period shall begin when the phase is Green and there is a serviceable
         * conflicting call. If the serviceable conflicting call is removed before completion of
         * this time (or time to reduce), the timer shall reset. Upon completion of the TBR period
         * or the CarsBeforeReduction (CBR) parameter is satisfied, whichever occurs first, the
         * linear reduction of the allowable gap from the Passage Time shall begin.
         */
        private int phaseTimeBeforeReduction;

        /**
         * Phase Cars Before Reduction (CBR) Parameter (0-255 vehicles). When the phase is Green and
         * the sum of the cars waiting (vehicle actuations during Yellow & Red intervals) on
         * serviceable conflicting phases equals or exceeds the CBR parameter or the Time Before
         * Reduction (TBR) parameter is satisfied, whichever occurs first, the linear reduction of
         * the allowable gap from the Passage Time shall begin.
         */
        private int phaseCarsBeforeReduction;

        /**
         * Phase Time To Reduce Parameter in seconds (0-255 sec). This parameter shall control the
         * rate of reduction of the allowable gap between the Passage Time and Minimum Gap setting.
         */
        private int phaseTimeToReduce;

        /**
         * This object may be used for volume density gap reduction as an alternate to the linear
         * reduction defined by NEMA TS 1 and TS 2. It contains the tenths of seconds to reduce the
         * gap by (0.0 - 25.5 seconds). The frequency of reduction shall produce the Minimum Gap
         * after a time equal to the ‘phaseTimeToReduce’ object.
         */
        private int phaseReduceBy;

        /**
         * Phase Minimum Gap Parameter in tenth seconds (0-25.5 sec). The reduction of the allowable
         * gap shall continue until the gap reaches a value equal to or less than the minimum gap as
         * set on the Minimum Gap control after which the allowable gap shall remain fixed at the
         * values set on the Minimum Gap control.
         */
        private int phaseMinimumGap;

        /**
         * This object shall determine either the upper or lower limit of the running max in seconds
         * (0-255 sec) during dynamic max operation. The normal maximum (i.e. Max1, Max2, etc.)
         * shall determine the other limit as follows: When dynamicMaxLimit is larger than the
         * normal maximum, it shall become the upper limit. When dynamicMaxLimit is smaller than the
         * normal maximum, it shall become the lower limit. Setting dynamicMaxLimit greater than
         * zero enables dynamic max operation with the normal maximum used as the initial maximum
         * setting. See dynamicMaxStep for details on dynamic max operation. Maximum recall or a
         * failed detector that is assigned to the associated phase shall disable dynamic max
         * operation for the phase.
         */
        private int phaseDynamicMaxLimit;

        /**
         * This object shall determine the automatic adjustment to the running max in tenth seconds
         * (0-25.5) When a phase maxes out twice in a row, and on each successive max out
         * thereafter, one dynamic max step value shall be added to the running max until such
         * addition would mean the running max was greater than the larger of normal max or dynamic
         * max limit. When a phase gaps out twice in a row, and on each successive gap out
         * thereafter, one dynamic max step value shall be subtracted from the running max until
         * such subtraction would mean the running max was less than the smaller of the normal max
         * or the dynamic max limit. If a phase gaps out in one cycle and maxes out in the next
         * cycle, or vice versa, the running max will not change.
         */
        private int phaseDynamicMaxStep;

        /**
         * The Phase Startup parameter is an enumerated integer which selects the startup state for
         * each phase after restoration of a defined power interruption or activation of the
         * external start input. The following entries are defined: other: this phase is not enabled
         * (phaseOptions bit 0=0 or phaseRing=0) or initializes in a state not defined by this
         * standard. phaseNotOn: this phase initializes in a Red state (the phase is not active and
         * no intervals are timing). greenWalk: this phase initializes at the beginning of the
         * minimum green and walk timing intervals. greenNoWalk: this phase initializes at the
         * beginning of the minimum green timing interval. yellowChange: this phase initializes at
         * the beginning of the Yellow Change interval. redClear: this phase initializes at the
         * beginning of the Red Clearance interval.
         */
        private int phaseStartup;

        /**
         * Optional phase functions ( 0 = False/ Disabled, 1 = True/Enabled) Bit 15:
         * AddedInitialCalculation - If set (1) the CU shall compare counts from all associated
         * AddedInitial detectors and use the largest count value for the calculations. If clear (0)
         * the CU shall sum all associated AddedInitial detector counts and use this sum for the
         * calculations. The ability to modify the setting of this bit is optional. Bit 14:
         * Conditional Service Enable - in multi-ring configurations when set to 1 causes a
         * gapped/maxed phase to conditionally service a preceding actuated vehicle phase when
         * sufficient time remains before max time out of the phase(s) not prepared to terminate.
         * Support is optional. REFERENCE NEMA TS 2 Clause 3.5.3.9 Bit 13: Actuated Rest In Walk -
         * when set to 1 causes an actuated phase to rest in Walk when there is no serviceable
         * conflicting call at the end of Walk Timing. Bit 12: Guaranteed Passage - when set to 1
         * enables an actuated phase operating in volume density mode (using gap reduction) to
         * retain the right of way for the unexpired portion of the Passage time following the
         * decision to terminate the green due to a reduced gap. Support is optional Bit 11:
         * Simultaneous Gap Disable - in multi-ring configurations when set to 1 disables a gapped
         * out phase from reverting to the extensible portion. Support is optional REFERENCE NEMA TS
         * 2 Clause 3.5.5.3 Bit 10: Dual Entry Phase - in multi-ring configurations when set to 1
         * causes the phase to become active upon entry into a concurrency group (crossing a
         * barrier) when no calls exist in its ring within its concurrency group. REFERENCE NEMA TS
         * 2 Clause 3.5.5.3 Bit 9: Soft Vehicle Recall - when set to 1 causes a call on a phase when
         * all conflicting phases are in green dwell or red dwell and there are no serviceable
         * conflicting calls. Support is optional. Bit 8: Ped. Recall - when set to 1 causes a
         * recurring pedestrian demand which shall function in the same manner as an external
         * pedestrian call except that it shall not recycle the pedestrian service until a
         * conflicting phase is serviced REFERENCE NEMA TS 2 Clause 3.5.3.7 Bit 7: Max Vehicle
         * Recall - when set to 1 causes a call on a phase such that the timing of the Green
         * interval for that phase shall be extended to Maximum Green time. REFERENCE NEMA TS 2
         * Clause 3.5.3.5 Bit 6: Min. Vehicle Recall - when set to 1 causes recurring demand for
         * vehicle service on the phase when that phase is not in its Green interval. REFERENCE NEMA
         * TS 2 Clause 3.5.3.6 Bit 5: Non Lock Detector Memory - when set to 0 will cause the call
         * to be locked at the beginning of the yellow interval. When set to 1 call locking will
         * depend on the detectorOptions object. REFERENCE NEMA TS 2 Clause 3.5.3.4 Bit 4:
         * Non-Actuated 2 - when set to 1 causes a phase to respond to the Call To Non-Actuated 2
         * input (if present) or other method. Support is optional REFERENCE NEMA TS 2 Clause
         * 3.5.5.5.8 Bit 3: Non-Actuated 1 - when set to 1 causes a phase to respond to the Call To
         * Non-Actuated 1 input (if present) or other method. Support is optional REFERENCE NEMA TS
         * 2 Clause 3.5.5.5.8 Bit 2: Automatic Flash Exit Phase - The CU shall move immediately to
         * the beginning of the phase(s) programmed as Exit Phase(s) when Automatic Flash
         * terminates. Support is optional REFERENCE NEMA TS 2 Clause 3.9.1.2.1 Bit 1: Automatic
         * Flash Entry Phase - When Automatic Flash is called, the CU shall service the Entry
         * Phase(s), clear to an All Red, then initiate flashing operation. Support is optional.
         * REFERENCE NEMA TS 2 Clause 3.9.1.2.1 Bit 0: Enabled Phase - provide a means to define
         * whether this phase is used in the current configuration. A disabled phase shall not
         * provide any outputs nor respond to any phase inputs. The object phaseRing = 0 has the
         * same effect.
         */
        private int phaseOptions;

        /**
         * Phase ring number (1..maxRings) that identified the ring which contains the associated
         * phase. This value must not exceed the maxRings object value. If the ring number is zero,
         * the phase is disabled (phaseOptions Bit 0 = 0 has the same effect).
         */
        private int phaseRing;

        /**
         * Each octet contains a phase number (binary value) that may run concurrently with the
         * associated phase. Phases that are contained in the same ring may NOT run concurrently.
         */
        private String phaseConcurrency;

        public int getPhaseAddedInitial() {
            return phaseAddedInitial;
        }

        public void setPhaseAddedInitial(int phaseAddedInitial) {
            this.phaseAddedInitial = phaseAddedInitial;
        }

        public int getPhaseCarsBeforeReduction() {
            return phaseCarsBeforeReduction;
        }

        public void setPhaseCarsBeforeReduction(int phaseCarsBeforeReduction) {
            this.phaseCarsBeforeReduction = phaseCarsBeforeReduction;
        }

        public String getPhaseConcurrency() {
            return phaseConcurrency;
        }

        public void setPhaseConcurrency(String phaseConcurrency) {
            this.phaseConcurrency = phaseConcurrency;
        }

        public int getPhaseDynamicMaxLimit() {
            return phaseDynamicMaxLimit;
        }

        public void setPhaseDynamicMaxLimit(int phaseDynamicMaxLimit) {
            this.phaseDynamicMaxLimit = phaseDynamicMaxLimit;
        }

        public int getPhaseDynamicMaxStep() {
            return phaseDynamicMaxStep;
        }

        public void setPhaseDynamicMaxStep(int phaseDynamicMaxStep) {
            this.phaseDynamicMaxStep = phaseDynamicMaxStep;
        }

        public int getPhaseMaximum1() {
            return phaseMaximum1;
        }

        public void setPhaseMaximum1(int phaseMaximum1) {
            this.phaseMaximum1 = phaseMaximum1;
        }

        public int getPhaseMaximum2() {
            return phaseMaximum2;
        }

        public void setPhaseMaximum2(int phaseMaximum2) {
            this.phaseMaximum2 = phaseMaximum2;
        }

        public int getPhaseMaximumInitial() {
            return phaseMaximumInitial;
        }

        public void setPhaseMaximumInitial(int phaseMaximumInitial) {
            this.phaseMaximumInitial = phaseMaximumInitial;
        }

        public int getPhaseMinimumGap() {
            return phaseMinimumGap;
        }

        public void setPhaseMinimumGap(int phaseMinimumGap) {
            this.phaseMinimumGap = phaseMinimumGap;
        }

        public int getPhaseMinimumGreen() {
            return phaseMinimumGreen;
        }

        public void setPhaseMinimumGreen(int phaseMinimumGreen) {
            this.phaseMinimumGreen = phaseMinimumGreen;
        }

        public int getPhaseNumber() {
            return phaseNumber;
        }

        public void setPhaseNumber(int phaseNumber) {
            this.phaseNumber = phaseNumber;
        }

        public int getPhaseOptions() {
            return phaseOptions;
        }

        public void setPhaseOptions(int phaseOptions) {
            this.phaseOptions = phaseOptions;
        }

        public int getPhasePassage() {
            return phasePassage;
        }

        public void setPhasePassage(int phasePassage) {
            this.phasePassage = phasePassage;
        }

        public int getPhasePedestrianClear() {
            return phasePedestrianClear;
        }

        public void setPhasePedestrianClear(int phasePedestrianClear) {
            this.phasePedestrianClear = phasePedestrianClear;
        }

        public int getPhaseRedClear() {
            return phaseRedClear;
        }

        public void setPhaseRedClear(int phaseRedClear) {
            this.phaseRedClear = phaseRedClear;
        }

        public int getPhaseRedRevert() {
            return phaseRedRevert;
        }

        public void setPhaseRedRevert(int phaseRedRevert) {
            this.phaseRedRevert = phaseRedRevert;
        }

        public int getPhaseReduceBy() {
            return phaseReduceBy;
        }

        public void setPhaseReduceBy(int phaseReduceBy) {
            this.phaseReduceBy = phaseReduceBy;
        }

        public int getPhaseRing() {
            return phaseRing;
        }

        public void setPhaseRing(int phaseRing) {
            this.phaseRing = phaseRing;
        }

        public int getPhaseStartup() {
            return phaseStartup;
        }

        public void setPhaseStartup(int phaseStartup) {
            this.phaseStartup = phaseStartup;
        }

        public int getPhaseTimeBeforeReduction() {
            return phaseTimeBeforeReduction;
        }

        public void setPhaseTimeBeforeReduction(int phaseTimeBeforeReduction) {
            this.phaseTimeBeforeReduction = phaseTimeBeforeReduction;
        }

        public int getPhaseTimeToReduce() {
            return phaseTimeToReduce;
        }

        public void setPhaseTimeToReduce(int phaseTimeToReduce) {
            this.phaseTimeToReduce = phaseTimeToReduce;
        }

        public int getPhaseWalk() {
            return phaseWalk;
        }

        public void setPhaseWalk(int phaseWalk) {
            this.phaseWalk = phaseWalk;
        }

        public int getPhaseYellowChange() {
            return phaseYellowChange;
        }

        public void setPhaseYellowChange(int phaseYellowChange) {
            this.phaseYellowChange = phaseYellowChange;
        }
    }

    /**
     * A table containing Actuated Controller Unit phase parameters. The number of rows in this
     * table is equal to the maxPhases object.
     */
    public class PhaseGroupTable {

        /**
         * The Phase StatusGroup number for objects in this row. This value shall not exceed the
         * maxPhaseGroups object value.
         */
        private int phaseStatusGroupNumber;

        /**
         * Phase Red Output Status Mask, when a bit = 1, the Phase Red is currently active. When a
         * bit = 0, the Phase Red is NOT currently active. Bit 7: Phase # = (phaseStatusGroupNumber
         * * 8) Bit 6: Phase # = (phaseStatusGroupNumber * 8) - 1 Bit 5: Phase # =
         * (phaseStatusGroupNumber * 8) - 2 Bit 4: Phase # = (phaseStatusGroupNumber * 8) - 3 Bit 3:
         * Phase # = (phaseStatusGroupNumber * 8) - 4 Bit 2: Phase # = (phaseStatusGroupNumber * 8)
         * - 5 Bit 1: Phase # = (phaseStatusGroupNumber * 8) - 6 Bit 0: Phase # =
         * (phaseStatusGroupNumber * 8) - 7
         */
        private int phaseStatusGroupReds;

        /**
         * Phase Yellow Output Status Mask, when a bit = 1, the Phase Yellow is currently active.
         * When a bit = 0, the Phase Yellow is NOT currently active. Bit 7: Phase # =
         * (phaseStatusGroupNumber * 8) Bit 6: Phase # = (phaseStatusGroupNumber * 8) - 1 Bit 5:
         * Phase # = (phaseStatusGroupNumber * 8) - 2 Bit 4: Phase # = (phaseStatusGroupNumber * 8)
         * - 3 Bit 3: Phase # = (phaseStatusGroupNumber * 8) - 4 Bit 2: Phase # =
         * (phaseStatusGroupNumber * 8) - 5 Bit 1: Phase # = (phaseStatusGroupNumber * 8) - 6 Bit 0:
         * Phase # = (phaseStatusGroupNumber * 8) - 7
         */
        private int phaseStatusGroupYellows;

        /**
         * Phase Green Output Status Mask, when a bit = 1, the Phase Green is currently active. When
         * a bit = 0, the Phase Green is NOT currently active. Bit 7: Phase # =
         * (phaseStatusGroupNumber * 8) Bit 6: Phase # = (phaseStatusGroupNumber * 8) - 1 Bit 5:
         * Phase # = (phaseStatusGroupNumber * 8) - 2 Bit 4: Phase # = (phaseStatusGroupNumber * 8)
         * - 3 Bit 3: Phase # = (phaseStatusGroupNumber * 8) - 4 Bit 2: Phase # =
         * (phaseStatusGroupNumber * 8) - 5 Bit 1: Phase # = (phaseStatusGroupNumber * 8) - 6 Bit 0:
         * Phase # = (phaseStatusGroupNumber * 8) - 7
         */
        private int phaseStatusGroupGreens;

        /**
         * Phase Dont Walk Output Status Mask, when a bit = 1, the Phase Dont Walk is currently
         * active. When a bit = 0, the Phase Dont Walk is NOT currently active. Bit 7: Phase # =
         * (phaseStatusGroupNumber * 8) Bit 6: Phase # = (phaseStatusGroupNumber * 8) - 1 Bit 5:
         * Phase # = (phaseStatusGroupNumber * 8) - 2 Bit 4: Phase # = (phaseStatusGroupNumber * 8)
         * - 3 Bit 3: Phase # = (phaseStatusGroupNumber * 8) - 4 Bit 2: Phase # =
         * (phaseStatusGroupNumber * 8) - 5 Bit 1: Phase # = (phaseStatusGroupNumber * 8) - 6 Bit 0:
         * Phase # = (phaseStatusGroupNumber * 8) - 7
         */
        private int phaseStatusGroupDontWalks;

        /**
         * Phase Ped Clear Output Status Mask, when a bit = 1, the Phase Ped Clear is currently
         * active. When a bit = 0, the Phase Ped Clear is NOT currently active. Bit 7: Phase # =
         * (phaseStatusGroupNumber * 8) Bit 6: Phase # = (phaseStatusGroupNumber * 8) - 1 Bit 5:
         * Phase # = (phaseStatusGroupNumber * 8) - 2 Bit 4: Phase # = (phaseStatusGroupNumber * 8)
         * - 3 Bit 3: Phase # = (phaseStatusGroupNumber * 8) - 4 Bit 2: Phase # =
         * (phaseStatusGroupNumber * 8) - 5 Bit 1: Phase # = (phaseStatusGroupNumber * 8) - 6 Bit 0:
         * Phase # = (phaseStatusGroupNumber * 8) - 7
         */
        private int phaseStatusGroupPedClears;

        /**
         * Phase Walk Output Status Mask, when a bit = 1, the Phase Walk is currently active. When a
         * bit = 0, the Phase Walk is NOT currently active. Bit 7: Phase # = (phaseStatusGroupNumber
         * * 8) Bit 6: Phase # = (phaseStatusGroupNumber * 8) - 1 Bit 5: Phase # =
         * (phaseStatusGroupNumber * 8) - 2 Bit 4: Phase # = (phaseStatusGroupNumber * 8) - 3 Bit 3:
         * Phase # = (phaseStatusGroupNumber * 8) - 4 Bit 2: Phase # = (phaseStatusGroupNumber * 8)
         * - 5 Bit 1: Phase # = (phaseStatusGroupNumber * 8) - 6 Bit 0: Phase # =
         * (phaseStatusGroupNumber * 8) - 7
         */
        private int phaseStatusGroupWalks;

        /**
         * Phase Vehicle Call Status Mask, when a bit = 1, the Phase vehicle currently has a call
         * for service. When a bit = 0, the Phase vehicle currently does NOT have a call for
         * service. Bit 7: Phase # = (phaseStatusGroupNumber * 8) Bit 6: Phase # =
         * (phaseStatusGroupNumber * 8) - 1 Bit 5: Phase # = (phaseStatusGroupNumber * 8) - 2 Bit 4:
         * Phase # = (phaseStatusGroupNumber * 8) - 3 Bit 3: Phase # = (phaseStatusGroupNumber * 8)
         * - 4 Bit 2: Phase # = (phaseStatusGroupNumber * 8) - 5 Bit 1: Phase # =
         * (phaseStatusGroupNumber * 8) - 6 Bit 0: Phase # = (phaseStatusGroupNumber * 8) - 7
         */
        private int phaseStatusGroupVehCalls;

        /**
         * Phase Pedestrian Call Status Mask, when a bit = 1, the Phase pedestrian currently has a
         * call for service. When a bit = 0, the Phase pedestrian currently does NOT have a call for
         * service. Bit 7: Phase # = (phaseStatusGroupNumber * 8) Bit 6: Phase # =
         * (phaseStatusGroupNumber * 8) - 1 Bit 5: Phase # = (phaseStatusGroupNumber * 8) - 2 Bit 4:
         * Phase # = (phaseStatusGroupNumber * 8) - 3 Bit 3: Phase # = (phaseStatusGroupNumber * 8)
         * - 4 Bit 2: Phase # = (phaseStatusGroupNumber * 8) - 5 Bit 1: Phase # =
         * (phaseStatusGroupNumber * 8) - 6 Bit 0: Phase # = (phaseStatusGroupNumber * 8) - 7
         */
        private int phaseStatusGroupPedCalls;

        /**
         * Phase On Status Mask, when a bit = 1, the Phase is currently active. When a bit = 0, the
         * Phase currently is NOT active. The phase is ON during the Green, Yellow, & Red Clearance
         * intervals of that phase. It shall be permissible for this status to be True (bit=1)
         * during the Red Dwell state. Bit 7: Phase # = (phaseStatusGroupNumber * 8) Bit 6: Phase #
         * = (phaseStatusGroupNumber * 8) - 1 Bit 5: Phase # = (phaseStatusGroupNumber * 8) - 2 Bit
         * 4: Phase # = (phaseStatusGroupNumber * 8) - 3 Bit 3: Phase # = (phaseStatusGroupNumber *
         * 8) - 4 Bit 2: Phase # = (phaseStatusGroupNumber * 8) - 5 Bit 1: Phase # =
         * (phaseStatusGroupNumber * 8) - 6 Bit 0: Phase # = (phaseStatusGroupNumber * 8) - 7
         */
        private int phaseStatusGroupPhaseOns;

        /**
         * Phase Next Status Mask, when a bit = 1, the Phase currently is committed to be NEXT in
         * sequence & remains present until the phase becomes active (On/Timing). When a bit = 0,
         * the Phase currently is NOT committed to be NEXT in sequence. The phase next to be
         * serviced shall be determined at the end of the green interval of the terminating phase;
         * except that if the decision cannot be made at the end of the Green interval, it shall not
         * be made until after the end of all Vehicle Change & Clearance intervals. Bit 7: Phase # =
         * (phaseStatusGroupNumber * 8) Bit 6: Phase # = (phaseStatusGroupNumber * 8) - 1 Bit 5:
         * Phase # = (phaseStatusGroupNumber * 8) - 2 Bit 4: Phase # = (phaseStatusGroupNumber * 8)
         * - 3 Bit 3: Phase # = (phaseStatusGroupNumber * 8) - 4 Bit 2: Phase # =
         * (phaseStatusGroupNumber * 8) - 5 Bit 1: Phase # = (phaseStatusGroupNumber * 8) - 6 Bit 0:
         * Phase # = (phaseStatusGroupNumber * 8) - 7
         */
        private int phaseStatusGroupPhaseNexts;

        public int getPhaseStatusGroupDontWalks() {
            return phaseStatusGroupDontWalks;
        }

        public void setPhaseStatusGroupDontWalks(int phaseStatusGroupDontWalks) {
            this.phaseStatusGroupDontWalks = phaseStatusGroupDontWalks;
        }

        public int getPhaseStatusGroupGreens() {
            return phaseStatusGroupGreens;
        }

        public void setPhaseStatusGroupGreens(int phaseStatusGroupGreens) {
            this.phaseStatusGroupGreens = phaseStatusGroupGreens;
        }

        public int getPhaseStatusGroupNumber() {
            return phaseStatusGroupNumber;
        }

        public void setPhaseStatusGroupNumber(int phaseStatusGroupNumber) {
            this.phaseStatusGroupNumber = phaseStatusGroupNumber;
        }

        public int getPhaseStatusGroupPedCalls() {
            return phaseStatusGroupPedCalls;
        }

        public void setPhaseStatusGroupPedCalls(int phaseStatusGroupPedCalls) {
            this.phaseStatusGroupPedCalls = phaseStatusGroupPedCalls;
        }

        public int getPhaseStatusGroupPedClears() {
            return phaseStatusGroupPedClears;
        }

        public void setPhaseStatusGroupPedClears(int phaseStatusGroupPedClears) {
            this.phaseStatusGroupPedClears = phaseStatusGroupPedClears;
        }

        public int getPhaseStatusGroupPhaseNexts() {
            return phaseStatusGroupPhaseNexts;
        }

        public void setPhaseStatusGroupPhaseNexts(int phaseStatusGroupPhaseNexts) {
            this.phaseStatusGroupPhaseNexts = phaseStatusGroupPhaseNexts;
        }

        public int getPhaseStatusGroupPhaseOns() {
            return phaseStatusGroupPhaseOns;
        }

        public void setPhaseStatusGroupPhaseOns(int phaseStatusGroupPhaseOns) {
            this.phaseStatusGroupPhaseOns = phaseStatusGroupPhaseOns;
        }

        public int getPhaseStatusGroupReds() {
            return phaseStatusGroupReds;
        }

        public void setPhaseStatusGroupReds(int phaseStatusGroupReds) {
            this.phaseStatusGroupReds = phaseStatusGroupReds;
        }

        public int getPhaseStatusGroupVehCalls() {
            return phaseStatusGroupVehCalls;
        }

        public void setPhaseStatusGroupVehCalls(int phaseStatusGroupVehCalls) {
            this.phaseStatusGroupVehCalls = phaseStatusGroupVehCalls;
        }

        public int getPhaseStatusGroupWalks() {
            return phaseStatusGroupWalks;
        }

        public void setPhaseStatusGroupWalks(int phaseStatusGroupWalks) {
            this.phaseStatusGroupWalks = phaseStatusGroupWalks;
        }

        public int getPhaseStatusGroupYellows() {
            return phaseStatusGroupYellows;
        }

        public void setPhaseStatusGroupYellows(int phaseStatusGroupYellows) {
            this.phaseStatusGroupYellows = phaseStatusGroupYellows;
        }
    }

    public class PhaseControlTable {

        /**
         * The Phase Control Group number for objects in this row. This value shall not exceed the
         * maxPhaseGroups object value.
         */
        private int phaseControlGroupNumber;

        /**
         * This object is used to allow a remote entity to omit phases from being serviced in the
         * device. When a bit = 1, the device shall activate the System Phase Omit control for that
         * phase. When a bit = 0, the device shall not activate the System Phase Omit control for
         * that phase. Bit 7: Phase # = (phaseControlGroupNumber * 8) Bit 6: Phase # =
         * (phaseControlGroupNumber * 8) - 1 Bit 5: Phase # = (phaseControlGroupNumber * 8) - 2 Bit
         * 4: Phase # = (phaseControlGroupNumber * 8) - 3 Bit 3: Phase # = (phaseControlGroupNumber
         * * 8) - 4 Bit 2: Phase # = (phaseControlGroupNumber * 8) - 5 Bit 1: Phase # =
         * (phaseControlGroupNumber * 8) - 6 Bit 0: Phase # = (phaseControlGroupNumber * 8) - 7 The
         * device shall reset this object to ZERO when in BACKUP Mode. A write to this object shall
         * reset the Backup timer to ZERO (see unitBackupTime).
         */
        private int phaseControlGroupPhaseOmit;

        /**
         * This object is used to allow a remote entity to omit peds from being serviced in the
         * device. When a bit = 1, the device shall activate the System Ped Omit control for that
         * phase. When a bit = 0, the device shall not activate the System Ped Omit control for that
         * phase. Bit 7: Phase # = (phaseControlGroupNumber * 8) Bit 6: Phase # =
         * (phaseControlGroupNumber * 8) - 1 Bit 5: Phase # = (phaseControlGroupNumber * 8) - 2 Bit
         * 4: Phase # = (phaseControlGroupNumber * 8) - 3 Bit 3: Phase # = (phaseControlGroupNumber
         * * 8) - 4 Bit 2: Phase # = (phaseControlGroupNumber * 8) - 5 Bit 1: Phase # =
         * (phaseControlGroupNumber * 8) - 6 Bit 0: Phase # = (phaseControlGroupNumber * 8) - 7 The
         * device shall reset this object to ZERO when in BACKUP Mode. A write to this object shall
         * reset the Backup timer to ZERO (see unitBackupTime).
         */
        private int phaseControlGroupPedOmit;

        /**
         * This object is used to allow a remote entity to hold phases in the device. When a bit =
         * 1, the device shall activate the System Phase Hold control for that phase. When a bit =
         * 0, the device shall not activate the System Phase Hold control for that phase. Bit 7:
         * Phase # = (phaseControlGroupNumber * 8) Bit 6: Phase # = (phaseControlGroupNumber * 8) -
         * 1 Bit 5: Phase # = (phaseControlGroupNumber * 8) - 2 Bit 4: Phase # =
         * (phaseControlGroupNumber * 8) - 3 Bit 3: Phase # = (phaseControlGroupNumber * 8) - 4 Bit
         * 2: Phase # = (phaseControlGroupNumber * 8) - 5 Bit 1: Phase # = (phaseControlGroupNumber
         * * 8) - 6 Bit 0: Phase # = (phaseControlGroupNumber * 8) - 7 The device shall reset this
         * object to ZERO when in BACKUP Mode. A write to this object shall reset the Backup timer
         * to ZERO (see unitBackupTime).
         */
        private int phaseControlGroupHold;

        /**
         * This object is used to apply force offs on a per phase basis. When a bit = 1, the device
         * shall activate the System Phase Force Off control for that phase. When a bit = 0, the
         * device shall not activate the System Phase Force Off control for that phase. When the
         * phase green terminates, the associated bit shall be reset to 0. Bit 7: Phase # =
         * (phaseControlGroupNumber * 8) Bit 6: Phase # = (phaseControlGroupNumber * 8) - 1 Bit 5:
         * Phase # = (phaseControlGroupNumber * 8) - 2 Bit 4: Phase # = (phaseControlGroupNumber *
         * 8) - 3 Bit 3: Phase # = (phaseControlGroupNumber * 8) - 4 Bit 2: Phase # =
         * (phaseControlGroupNumber * 8) - 5 Bit 1: Phase # = (phaseControlGroupNumber * 8) - 6 Bit
         * 0: Phase # = (phaseControlGroupNumber * 8) - 7 The device shall reset this object to ZERO
         * when in BACKUP Mode. A write to this object shall reset the Backup timer to ZERO (see
         * unitBackupTime).
         */
        private int phaseControlGroupForceOff;

        /**
         * This object is used to allow a remote entity to place calls for vehicle service in the
         * device. When a bit = 1, the device shall place a call for vehicle service on that phase.
         * When a bit = 0, the device shall not place a call for vehicle service on that phase. Bit
         * 7: Phase # = (phaseControlGroupNumber * 8) Bit 6: Phase # = (phaseControlGroupNumber * 8)
         * - 1 Bit 5: Phase # = (phaseControlGroupNumber * 8) - 2 Bit 4: Phase # =
         * (phaseControlGroupNumber * 8) - 3 Bit 3: Phase # = (phaseControlGroupNumber * 8) - 4 Bit
         * 2: Phase # = (phaseControlGroupNumber * 8) - 5 Bit 1: Phase # = (phaseControlGroupNumber
         * * 8) - 6 Bit 0: Phase # = (phaseControlGroupNumber * 8) - 7 The device shall reset this
         * object to ZERO when in BACKUP Mode. A write to this object shall reset the Backup timer
         * to ZERO (see unitBackupTime).
         */
        private int phaseControlGroupVehCall;

        /**
         * This object is used to allow a remote entity to place calls for ped service in the
         * device. When a bit = 1, the device shall place a call for ped service on that phase. When
         * a bit = 0, the device shall not place a call for ped service on that phase. Bit 7: Phase
         * # = (phaseControlGroupNumber * 8) Bit 6: Phase # = (phaseControlGroupNumber * 8) - 1 Bit
         * 5: Phase # = (phaseControlGroupNumber * 8) - 2 Bit 4: Phase # = (phaseControlGroupNumber
         * * 8) - 3 Bit 3: Phase # = (phaseControlGroupNumber * 8) - 4 Bit 2: Phase # =
         * (phaseControlGroupNumber * 8) - 5 Bit 1: Phase # = (phaseControlGroupNumber * 8) - 6 Bit
         * 0: Phase # = (phaseControlGroupNumber * 8) - 7 The device shall reset this object to ZERO
         * when in BACKUP Mode. A write to this object shall reset the Backup timer to ZERO (see
         * unitBackupTime).
         */
        private int phaseControlGroupPedCall;

        public int getPhaseControlGroupForceOff() {
            return phaseControlGroupForceOff;
        }

        public void setPhaseControlGroupForceOff(int phaseControlGroupForceOff) {
            this.phaseControlGroupForceOff = phaseControlGroupForceOff;
        }

        public int getPhaseControlGroupHold() {
            return phaseControlGroupHold;
        }

        public void setPhaseControlGroupHold(int phaseControlGroupHold) {
            this.phaseControlGroupHold = phaseControlGroupHold;
        }

        public int getPhaseControlGroupNumber() {
            return phaseControlGroupNumber;
        }

        public void setPhaseControlGroupNumber(int phaseControlGroupNumber) {
            this.phaseControlGroupNumber = phaseControlGroupNumber;
        }

        public int getPhaseControlGroupPedCall() {
            return phaseControlGroupPedCall;
        }

        public void setPhaseControlGroupPedCall(int phaseControlGroupPedCall) {
            this.phaseControlGroupPedCall = phaseControlGroupPedCall;
        }

        public int getPhaseControlGroupPedOmit() {
            return phaseControlGroupPedOmit;
        }

        public void setPhaseControlGroupPedOmit(int phaseControlGroupPedOmit) {
            this.phaseControlGroupPedOmit = phaseControlGroupPedOmit;
        }

        public int getPhaseControlGroupPhaseOmit() {
            return phaseControlGroupPhaseOmit;
        }

        public void setPhaseControlGroupPhaseOmit(int phaseControlGroupPhaseOmit) {
            this.phaseControlGroupPhaseOmit = phaseControlGroupPhaseOmit;
        }

        public int getPhaseControlGroupVehCall() {
            return phaseControlGroupVehCall;
        }

        public void setPhaseControlGroupVehCall(int phaseControlGroupVehCall) {
            this.phaseControlGroupVehCall = phaseControlGroupVehCall;
        }
    }

    public PhaseControlTable[] getControl() {
        return control.clone();
    }

    public void setControl(PhaseControlTable[] control) {
        this.control = control.clone();
    }

    public PhaseEntry[] getEntries() {
        return entries.clone();
    }

    public void setEntries(PhaseEntry[] entries) {
        this.entries = entries.clone();
    }

    public PhaseGroupTable[] getGroups() {
        return groups.clone();
    }

    public void setGroups(PhaseGroupTable[] groups) {
        this.groups = groups.clone();
    }

    public int getMaxPhaseGroups() {
        return maxPhaseGroups;
    }

    public void setMaxPhaseGroups(int maxPhaseGroups) {
        this.maxPhaseGroups = maxPhaseGroups;
    }

    public int getMaxPhases() {
        return maxPhases;
    }

    public void setMaxPhases(int maxPhases) {
        this.maxPhases = maxPhases;
    }
}
