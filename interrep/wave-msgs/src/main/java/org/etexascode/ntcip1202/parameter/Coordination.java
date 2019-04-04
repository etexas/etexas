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
 * The coord node contains objects that support coordination configuration, status, and control
 * functions for this device.
 * 
 * @author dranker
 */
public class Coordination implements Serializable {

    // NOT MAPPED
    /**
     * This object defines the operational mode for coordination. The possible modes are: Value
     * Description 0 Automatic - this mode provides for coord operation, free, and flash to be
     * determined automatically by the possible sources (i.e. Interconnect, Time Base, or System
     * Commands). 1-253 Manual Pattern - these modes provides for Coord operation running this
     * pattern. This selection of pattern overrides all other pattern commands. 254 Manual Free -
     * this mode provides for Free operation without coordination or Automatic Flash from any
     * source. 255 Manual Flash - this mode provides for Automatic Flash without coordination or
     * Free from any source.
     */
    private int coordOperationMode;

    // NOT MAPPED
    /**
     * This object defines the Coord Correction Mode. The possible modes are: other: the coordinator
     * establishes a new offset by a mechanism not defined in this standard. dwell: when changing
     * offset, the coordinator shall establish a new offset by dwelling in the coord phase(s) until
     * the desired offset is reached. shortway (Smooth): when changing offset, the coordinator shall
     * establish a new offset by adding or subtracting to/from the timings in a manner that limits
     * the cycle change. This operation is performed in a device specific manner. addOnly: when
     * changing offset, the coordinator shall establish a new offset by adding to the timings in a
     * manner that limits the cycle change. This operation is performed in a device specific manner.
     */
    private int coordCorrectionMode;

    // NOT MAPPED
    /**
     * "This object defines the Coord Maximum Mode. The possible modes are: other: the maximum mode
     * is determined by some other mechanism not defined in this standard. maximum1: the internal
     * Maximum 1 Timing shall be effective while coordination is running a pattern. maximum2: the
     * internal Maximum 2 Timing shall be effective while coordination is running a pattern.
     * maxInhibit: the internal Maximum Timing shall be inhibited while coordination is running a
     * pattern.
     */
    private int coordMaximumMode;

    // NOT MAPPED
    /**
     * This object defines the Pattern Force Mode. The possible modes are: other: the CU implements
     * a mechanism not defined in this standard. floating: each non-coord phase will be forced to
     * limit its time to the split time value. This allows unused split time to revert to the coord
     * phase. fixed: each non-coord phase will be forced at a fixed position in the cycle. This
     * allows unused split time to revert to the following phase.
     */
    private int coordForceMode;

    // NOT MAPPED
    /**
     * The maximum number of Patterns this Actuated Controller Unit supports. This object indicates
     * how many rows are in the patternTable object (254 and 255 are defined as non-pattern status
     * for Free and Flash).
     */
    private int maxPatterns;

    // NOT MAPPED
    /**
     * This object provides information about any special organizational structure required for the
     * pattern table. The defined structures are as follows: other: The pattern table setup is not
     * described in this standard, refer to device manual. patterns: Each row of the pattern table
     * represents a unique pattern and has no dependencies on other rows. offset3: The pattern table
     * is organized into plans which have three offsets. Each plan uses three consecutive rows. Only
     * patternOffsetTime and patternSequenceNumber values may vary between each of the three rows.
     * Plan 1 is contained in rows 1, 2 and 3, Plan 2 is contained in rows 4, 5 and 6, Plan 3 is in
     * rows 7, 8 and 9, etc. offset5: The pattern table is organized into plans which have five
     * offsets. Each plan occupies five consecutive rows. Only patternOffsetTime and
     * patternSequenceNumber values may vary between each of the rows. Plan 1 is contained in rows
     * 1, 2, 3, 4 and 5, Plan 2 is contained in rows 6, 7, 8, 9 and 10, Plan 3 is contained in rows
     * 11, 12, 13, 14 and 15, etc.
     */
    private int patternTableType;

    // NOT MAPPED
    /**
     * The maximum number of Split Plans this Actuated Controller Unit supports. This object
     * indicates how many Split plans are in the splitTable object.
     */
    private int maxSplits;

    // NOT MAPPED
    /**
     * This object defines the running coordination pattern/mode in the device. The possible values
     * are: Value Description 0 Not used 1-253 Pattern - indicates the currently running pattern 254
     * Free - indicates Free operation without coordination. 255 Flash - indicates Automatic Flash
     * without coordination.
     */
    private int coordPatternStatus;

    // NOT MAPPED
    /**
     * The Free modes: other: Some other condition has caused the device to run in free mode.
     * notFree: The unit is not running in free mode. commandFree: the current pattern command is
     * the Free mode pattern. transitionFree: the CU has a pattern command but is cycling to a point
     * to begin coordination. inputFree: one of the CU inputs cause it to not respond to
     * coordination. coordFree: the CU programming for the called pattern is to run Free. badPlan:
     * Free - the called pattern is invalid. badCycleTime: the pattern cycle time is less than
     * adequate to service the minimum requirements of all phases. splitOverrun: Free - the sum of
     * the critical path splitTime’s exceed the programmed patternCycleTime value. invalidOffset:
     * Free - reserved / not used failed: cycling diagnostics have called for Free. An ASC may
     * provide diagnostics beyond those stated herein. Therefore, for a set of given bad data, the
     * free status between devices may be inconsistent.
     */
    private int localFreeStatus;

    // NOT MAPPED
    /**
     * The Coord Cycle Status represents the current position in the local coord cycle of the
     * running pattern (0 to 510 sec). This value normally counts down from patternCycleTime to
     * Zero. This value may exceed the patternCycleTime during a coord cycle with offset correction
     * (patternCycleTime + correction).
     */
    private int coordCycleStatus;

    // NOT MAPPED
    /**
     * The Coord Sync Status represents the time since the system reference point for the running
     * pattern (0 to 510 sec). This value normally counts up from Zero to the next system reference
     * point (patternCycleTime). This value may exceed the patternCycleTime during a coord cycle in
     * which the system reference point has changed.
     */
    private int coordSyncStatus;

    // NOT MAPPED
    /**
     * This object is used to establish the Called System Pattern/Mode for the device. The possible
     * values are: Value Description 0 Standby - the system relinquishes control of the device.
     * 1-253 Pattern - these values indicate the system commanded pattern 254 Free - this value
     * indicates a call for Free 255 Flash - this value indicates a call for Automatic Flash If an
     * unsupported / invalid pattern is called, Free shall be the operational mode. The device shall
     * reset this object to ZERO when in BACKUP Mode. A write to this object shall reset the Backup
     * timer to ZERO (see unitBackupTime).
     */
    private int systemPatternControl;

    // NOT MAPPED
    /**
     * This object is used to establish the system reference point for the Called System Pattern by
     * providing the current position in the system pattern cycle (0-254 sec). The device shall
     * recognize a write to this object as a command to establish the time until the next system
     * reference point. Thereafter, the system reference point shall be assumed to occur at a
     * frequency equal to the patternCycleTime. When the value in the object is 255, the system
     * reference point shall be referenced to the local Time Base in accordance with its
     * programming. This CU must maintain an accuracy of 0.1 seconds based on the receipt of the SET
     * packet. The device shall reset this object to ZERO when in BACKUP Mode. A write to this
     * object shall reset the Backup timer to ZERO (see unitBackupTime).
     */
    private int systemSyncControl;

    private PatternTable[] patterns;

    private SplitTable[] splits;

    public Coordination() {
        patterns = new PatternTable[maxPatterns];
        splits = new SplitTable[maxSplits];
    }

    public class PatternTable {

        public int getPatternCycleTime() {
            return patternCycleTime;
        }

        public void setPatternCycleTime(int patternCycleTime) {
            this.patternCycleTime = patternCycleTime;
        }

        public int getPatternNumber() {
            return patternNumber;
        }

        public void setPatternNumber(int patternNumber) {
            this.patternNumber = patternNumber;
        }

        public int getPatternOffsetTime() {
            return patternOffsetTime;
        }

        public void setPatternOffsetTime(int patternOffsetTime) {
            this.patternOffsetTime = patternOffsetTime;
        }

        public int getPatternSequenceNumber() {
            return patternSequenceNumber;
        }

        public void setPatternSequenceNumber(int patternSequenceNumber) {
            this.patternSequenceNumber = patternSequenceNumber;
        }

        public int getPatternSplitNumber() {
            return patternSplitNumber;
        }

        public void setPatternSplitNumber(int patternSplitNumber) {
            this.patternSplitNumber = patternSplitNumber;
        }

        // NOT MAPPED
        /**
         * The pattern number for objects in this row. This value shall not exceed the maxPatterns
         * object value.
         */
        private int patternNumber;

        // NOT MAPPED
        /**
         * The patternCycleTime object specifies the length of the pattern cycle in seconds (NEMA TS
         * 2 range: 30-255). A pattern cycle time less than adequate to service the minimum
         * requirements of all phases shall result in Free mode. While this condition exists, the
         * Local Free bit of unitAlarmStatus1 and the Local Override bit of shortAlarmStatus shall
         * be set to one (1). The minimum requirements of a phase with a not-actuated ped include
         * Minimum Green, Walk, Pedestrian Clear, Yellow Clearance, and Red Clearance; the minimum
         * requirements of a phase with an actuated pedestrian include Minimum Green, Yellow
         * Clearance, and Red Clearance. If the pattern cycle time is zero and the associated split
         * table (if any) contains values greater than zero, then the CU shall utilize the split
         * time values as maximum values for each phase.
         */
        private int patternCycleTime;

        // NOT MAPPED
        /**
         * The patternOffsetTime defines by how many seconds (NEMA TS 2 range: 0-254) the local time
         * zero shall lag the system time zero (synchronization pulse) for this pattern. An offset
         * value equal to or greater than the patternCycleTime shall result in Free being the
         * operational mode. While this condition exists, the Local Free bit of unitAlarmStatus1 and
         * the LocalOverride bit of shortAlarmStatus shall be set to one (1).
         */
        private int patternOffsetTime;

        // NOT MAPPED
        /**
         * This object is used to locate information in the splitTable to use for this pattern. This
         * value shall not exceed the maxSplits object value.
         */
        private int patternSplitNumber;

        // NOT MAPPED
        /**
         * This object is used to locate information in the sequenceTable to use with this pattern.
         * This value shall not exceed the maxSequences object value.
         */
        private int patternSequenceNumber;
    }

    public class SplitTable {

        // NOT MAPPED

        /**
         * The object defines which rows of the split table comprise a split group. All rows that
         * have the same splitNumber are in the same split group. The value of this object shall not
         * exceed the maxSplits object value.
         */
        private int splitNumber;

        // NOT MAPPED
        /**
         * The phase number for objects in this row. The value of this object shall not exceed the
         * maxPhases object value
         */
        private int splitPhase;

        // NOT MAPPED
        /**
         * The time in seconds the splitPhase is allowed to receive (i.e. before a Force Off is
         * applied) when constant demands exist on all phases. In floating coordForceMode, this is
         * always the maximum time a non-coordinated phase is allowed to receive. In fixed
         * coordForceMode, the actual allowed time may be longer if a previous phase gapped out. The
         * splitTime includes all phase clearance times for the associated phase. The split time
         * shall be longer than the sum of the phase minimum service requirements for the phase.
         * When the time is NOT adequate to service the minimum service requirements of the phase,
         * Free Mode shall be the result. The minimum requirements of a phase with a not-actuated
         * ped include Minimum Green, Walk, Pedestrian Clear, Yellow Clearance, and Red Clearance;
         * the minimum requirements of a phase with an actuated pedestrian include Minimum Green,
         * Yellow Clearance, and Red Clearance. If the cycleTime entry of the associated
         * patternTable entry is zero (i.e. the device is in Free Mode), then the value of this
         * object shall be applied, if non-zero, as a maximum time for the associated phase. If the
         * critical path through the phase diagram is less than the cycleTime entry of the
         * associated patternTable entry, all extra time is alloted to the coordination phase in
         * each ring. If the critical path through the phase diagram is greater than the cycleTime
         * entry of the associated patternTable entry (and the cycleTime is not zero) the device
         * shall operate in the Free Mode. While the Free Mode condition exists, the Local Override
         * bit of shortAlarm shall be set to one (1).
         */
        private int splitTime;

        // NOT MAPPED
        /**
         * This object defines operational characteristics of the phase. The following options are
         * available: other: the operation is not specified in this standard none: no split mode
         * control. minimumVehicleRecall: this phase operates with a minimum vehicle recall.
         * maximumVehicleRecall: this phase operates with a maximum vehicle recall.
         * pedestrianRecall: this phase operates with a pedestrian recall.
         * maximumVehicleAndPedestrianRecall: this phase operates with a maximum vehicle &
         * pedestrian recall. phaseOmitted: this phase is omitted.
         */
        private int splitMode;

        // NOT MAPPED
        /**
         * To select the associated phase as a coordinated phase this object shall be set to TRUE
         * (non zero).
         */
        private int splitCoordPhase;

        public int getSplitCoordPhase() {
            return splitCoordPhase;
        }

        public void setSplitCoordPhase(int splitCoordPhase) {
            this.splitCoordPhase = splitCoordPhase;
        }

        public int getSplitMode() {
            return splitMode;
        }

        public void setSplitMode(int splitMode) {
            this.splitMode = splitMode;
        }

        public int getSplitNumber() {
            return splitNumber;
        }

        public void setSplitNumber(int splitNumber) {
            this.splitNumber = splitNumber;
        }

        public int getSplitPhase() {
            return splitPhase;
        }

        public void setSplitPhase(int splitPhase) {
            this.splitPhase = splitPhase;
        }

        public int getSplitTime() {
            return splitTime;
        }

        public void setSplitTime(int splitTime) {
            this.splitTime = splitTime;
        }

    }

    public int getCoordCorrectionMode() {
        return coordCorrectionMode;
    }

    public void setCoordCorrectionMode(int coordCorrectionMode) {
        this.coordCorrectionMode = coordCorrectionMode;
    }

    public int getCoordCycleStatus() {
        return coordCycleStatus;
    }

    public void setCoordCycleStatus(int coordCycleStatus) {
        this.coordCycleStatus = coordCycleStatus;
    }

    public int getCoordForceMode() {
        return coordForceMode;
    }

    public void setCoordForceMode(int coordForceMode) {
        this.coordForceMode = coordForceMode;
    }

    public int getCoordMaximumMode() {
        return coordMaximumMode;
    }

    public void setCoordMaximumMode(int coordMaximumMode) {
        this.coordMaximumMode = coordMaximumMode;
    }

    public int getCoordOperationMode() {
        return coordOperationMode;
    }

    public void setCoordOperationMode(int coordOperationMode) {
        this.coordOperationMode = coordOperationMode;
    }

    public int getCoordPatternStatus() {
        return coordPatternStatus;
    }

    public void setCoordPatternStatus(int coordPatternStatus) {
        this.coordPatternStatus = coordPatternStatus;
    }

    public int getCoordSyncStatus() {
        return coordSyncStatus;
    }

    public void setCoordSyncStatus(int coordSyncStatus) {
        this.coordSyncStatus = coordSyncStatus;
    }

    public int getLocalFreeStatus() {
        return localFreeStatus;
    }

    public void setLocalFreeStatus(int localFreeStatus) {
        this.localFreeStatus = localFreeStatus;
    }

    public int getMaxPatterns() {
        return maxPatterns;
    }

    public void setMaxPatterns(int maxPatterns) {
        this.maxPatterns = maxPatterns;
    }

    public int getMaxSplits() {
        return maxSplits;
    }

    public void setMaxSplits(int maxSplits) {
        this.maxSplits = maxSplits;
    }

    public int getPatternTableType() {
        return patternTableType;
    }

    public void setPatternTableType(int patternTableType) {
        this.patternTableType = patternTableType;
    }

    public PatternTable[] getPatterns() {
        return patterns.clone();
    }

    public void setPatterns(PatternTable[] patterns) {
        this.patterns = patterns.clone();
    }

    public SplitTable[] getSplits() {
        return splits.clone();
    }

    public void setSplits(SplitTable[] splits) {
        this.splits = splits.clone();
    }

    public int getSystemPatternControl() {
        return systemPatternControl;
    }

    public void setSystemPatternControl(int systemPatternControl) {
        this.systemPatternControl = systemPatternControl;
    }

    public int getSystemSyncControl() {
        return systemSyncControl;
    }

    public void setSystemSyncControl(int systemSyncControl) {
        this.systemSyncControl = systemSyncControl;
    }
}