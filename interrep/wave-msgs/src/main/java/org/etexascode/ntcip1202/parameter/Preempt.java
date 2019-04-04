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
 * The preempt node contains objects that support preempt input functions for the device.
 * 
 * @author dranker
 */
public class Preempt implements Serializable {

    /**
     * The Maximum Number of Preempts this Actuated Controller Unit supports. This object indicates
     * the maximum rows which shall appear in the preemptTable object.
     */
    private int maxPreempts;

    private PreemptTable[] preemptTableEntries;

    private PreemptControlTable[] controlTableEntries;

    public Preempt() {
        preemptTableEntries = new PreemptTable[maxPreempts];
        controlTableEntries = new PreemptControlTable[maxPreempts];
    }

    /**
     * A table containing Actuated Controller Unit preemption parameters. The number of rows in this
     * table is equal to the maxPreempts object.
     */
    public class PreemptTable {

        // NOT MAPPED

        /**
         * The preempt number for objects in this row. The value shall not exceed the maxPreempts
         * object value. When all preemptControl objects have a value where bit 2 = 0, each
         * preemptNumber routine shall be a higher priority and override all preemptNumber routines
         * that have a larger preemptNumber. When a preemptControl object has a value where bit 2 =
         * 1, the next higher preemptNumber becomes of equal priority with the preemptNumber but may
         * still be a higher priority than larger preemptNumbers depending on bit 2 of the relavent
         * preemptControl objects.
         */
        private int preemptNumber;

        // NOT MAPPED
        /**
         * Preempt Miscellaneous Control Parameter Mask ( Bit=0: False/Disabled, Bit=1:
         * True/Enabled) as follows: Bit 7: Reserved Bit 6: Reserved Bit 5: Reserved Bit 4: Reserved
         * Bit 3: Flash Dwell - the CU shall cause the phases listed in the preemptDwellPhase object
         * to flash Yellow during the Dwell interval. All active phases not listed in
         * preemptDwellPhase shall flash Red. The CU shall cause the overlaps listed in the
         * preemptDwellOverlap object to flash Yellow during the Dwell state. All active overlaps
         * not listed in preemptDwellOverlap shall flash Red. Preempt cycling phase programming is
         * ignored if this bit is set. This control is optional. Bit 2: Preempt Override
         * preemptNumber + 1 - provide a means to define whether this preempt shall NOT override the
         * next higher numbered Preempt. When set (1) this preempt shall not override the next
         * higher numbered preempt. Lowered numbered preempts override higher numbered preempts. For
         * example, 1 overrides 3, and the only way to get 3 equal to 1, is to set both 1 and 2 to
         * NOT override the next higher numbered preempt. This parameter shall be ignored when
         * preemptNumber equals maxPreempts. Bit 1: Preempt Override Flash - provide a means to
         * define whether this preempt shall NOT override Automatic Flash. When set (1) this preempt
         * shall not override Automatic Flash. Bit 0: Non-Locking Memory - provide a means to enable
         * an operation which does not require detector memory. When set (1) a preempt sequence
         * shall not occur if the preempt input terminates prior to expiration of the preemptDelay
         * time. A SET of a 'reserved' bit to a value other than zero (0) shall return a badValue(3)
         * error.
         */
        private int preemptControl;

        // NOT MAPPED
        /**
         * This object provides a means to define a higher priority preempt to be combined (linked)
         * with this preempt. At the end of preemptDwellGreen, the linked preempt shall receive an
         * automatic call that shall be maintained as long as the demand for this preempt is active.
         * Any value that is not a higher priority preempt or a valid preempt shall be ignored. The
         * value shall not exceed the maxPreempts object value.
         */
        private int preemptLink;

        // NOT MAPPED
        /**
         * Preempt Delay Time in seconds (0-600 sec). This value determines the time the preempt
         * input shall be active prior to initiating any preempt sequence. A non-locking preempt
         * input which is removed prior to the completion of this time shall not cause a preempt
         * sequence to occur.
         */
        private int preemptDelay;

        // NOT MAPPED
        /**
         * Preempt Minimum Duration Time in seconds (0..65535 sec). This value determines the
         * minimum time during which the preempt is active. Duration begins timing at the end of
         * Preempt Delay (if non zero) and will prevent an exit from the Dwell interval until this
         * time has elapsed.
         */
        private int preemptMinimumDuration;

        // NOT MAPPED
        /**
         * Preempt Minimum Green Time in seconds (0-255 sec). A preempt initiated transition shall
         * not cause the termination of an existing Green prior to its display for lesser of the
         * phase’s Minimum Green time or this period. CAUTION - if this value is zero, phase Green
         * is terminated immediately.
         */
        private int preemptMinimumGreen;

        // NOT MAPPED
        /**
         * Preempt Minimum Walk Time in seconds (0-255 sec). A preempt initiated transition shall
         * not cause the termination of an existing Walk prior to its display for the lesser of the
         * phase’s Walk time or this period. CAUTION - if this value is zero, phase Walk is
         * terminated immediately.
         */
        private int preemptMinimumWalk;

        // NOT MAPPED
        /**
         * Enter Ped ClearTime in seconds (0-255 sec). This parameter controls the ped clear timing
         * for a normal Walk signal terminated by a preempt initiated transition. A preempt
         * initiated transition shall not cause the termination of a Pedestrian Clearance prior to
         * its display for the lesser of the phase’s Pedestrian Clearance time or this period.
         * CAUTION - if this value is zero, phase Ped Clear is terminated immediately.
         */
        private int preemptEnterPedClear;

        // NOT MAPPED
        /**
         * Track Clear Green Time in seconds (0-255 sec). This parameter controls the green timing
         * for the track clearance movement. Track Clear phase(s) are enabled in the
         * preemptTrackPhase object. If this value is zero, the track clearance movement is omitted,
         * regardless of preemptTrackPhase programming.
         */
        private int preemptTrackGreen;

        // NOT MAPPED
        /**
         * Minimum Dwell interval in seconds (1-255 sec). This parameter controls the minimum timing
         * for the dwell interval. Phase(s) active during the Dwell interval are enabled in
         * preemptDwellPhase and preemptCyclingPhase objects. The Dwell interval shall not terminate
         * prior to the completion of preemptMinimumDuration, preemptDwellGreen (this object), and
         * the call is no longer present.
         */
        private int preemptDwellGreen;

        // NOT MAPPED
        /**
         * Preempt Maximum Presence time in seconds (0-65535 sec). This value determines the maximum
         * time which a preempt call may remain active and be considered valid. When the preempt
         * call has been active for this time period, the CU shall return to normal operation. This
         * preempt call shall be considered invalid until such time as a change in state occurs (no
         * longer active). When set to zero the preempt maximum presence time is disabled.
         */
        private int preemptMaximumPresence;

        // NOT MAPPED
        /**
         * Each octet within the octet string contains a phaseNumber(binary value) that shall be
         * active during the Preempt Track Clear intervals. The values of phaseNumber used here
         * shall not exceed maxPhases or violate the Consistency Checks defined in Annex B.
         */
        private String preemptTrackPhase;

        // NOT MAPPED
        /**
         * Each octet within the octet string contains a phaseNumber (binary value) that specifies
         * the phase(s) to be served in the Preempt Dwell interval. The phase(s) defined in
         * preemptCyclingPhase shall occur after those defined herein. The values of phaseNumber
         * used here shall not exceed maxPhases or violate the Consistency Checks defined in Annex
         * B.
         */
        private String preemptDwellPhase;

        // NOT MAPPED
        /**
         * Each octet within the octet string contains a phaseNumber (binary value) that specifies
         * the pedestrian movement(s) to be served in the Preempt Dwell interval. The peds defined
         * in premptCyclingPed shall occur after those defined herein. The values of phaseNumber
         * used here shall not exceed maxPhases or violate the Consistency Checks defined in Annex
         * B.
         */
        private String preemptDwellPed;

        // NOT MAPPED
        /**
         * Each octet within the octet string contains a phaseNumber (binary value) that shall be
         * active following Preempt. The values of phaseNumber used here shall not exceed maxPhases
         * or violate the Consistency Checks defined in Annex B.
         */
        private String preemptExitPhase;

        // NOT MAPPED
        /**
         * Preempt State provides status on which state the associated preempt is in. The states are
         * as follows: other: preempt service is not specified in this standard. notActive: preempt
         * input is not active, this preempt is not active. notActiveWithCall: preempt input is
         * active, preempt service has not started. entryStarted: preempt service is timing the
         * entry intervals. trackService: preempt service is timing the track intervals. dwell:
         * preempt service is timing the dwell intervals. linkActive: preempt service is performing
         * linked operation. exitStarted: preempt service is timing the exit intervals. maxPresence:
         * preempt input has exceeded maxPresence time <DescriptiveName> NTCIP-
         */
        private int preemptState;

        // NOT MAPPED
        /**
         * Each octet within the octet string contains a overlapNumber (binary value) that shall be
         * active during the Preempt Track Clear intervals. The values of overlapNumber used here
         * shall not exceed maxOverlaps or violate the consistency checks defined in Annex B.
         */
        private String preemptTrackOverlap;

        // NOT MAPPED
        /**
         * Each octet within the octet string contains a overlapNumber (binary value) that is
         * allowed during the Preempt Dwell interval. The values of overlapNumber used here shall
         * not exceed maxOverlaps or violate the consistency checks defined in Annex B.
         */
        private String preemptDwellOverlap;

        // NOT MAPPED
        /**
         * Each octet within the octet string contains a phaseNumber (binary value) that is allowed
         * to cycle during the Preempt Dwell interval. The values of phaseNumber used here shall not
         * exceed maxPhases or violate the Consistency Checks defined in Annex B.
         */
        private String preemptCyclingPhase;

        // NOT MAPPED
        /**
         * Each octet within the octet string contains a phaseNumber (binary value) indicating a
         * pedestrian movement that is allowed to cycle during the Preempt Dwell interval. The
         * values of phaseNumber used here shall not exceed maxPhases or violate the consistency
         * checks defined in Annex B.
         */
        private String preemptCyclingPed;

        // NOT MAPPED
        /**
         * Each octet within the octet string contains a overlapNumber (binary value) that is
         * allowed to cycle during the Preempt Dwell interval. The values of overlapNumber used here
         * shall not exceed maxOverlaps or violate the consistency checks defined in Annex B.
         */
        private String preemptCyclingOverlap;

        // NOT MAPPED
        /**
         * Enter Yellow Change in tenth seconds (0-25.5 sec). This parameter controls the yellow
         * change timing for a normal Yellow Change signal terminated by a preempt initiated
         * transition. A preempt initiated transition shall not cause the termination of a Yellow
         * Change prior to its display for the lesser of the phase’s Yellow Change time or this
         * period. CAUTION - if this value is zero, phase Yellow Change is terminated immediately.
         */
        private int preemptEnterYellowChange;

        // NOT MAPPED
        /**
         * Enter Red Clear in tenth seconds (0-25.5 sec). This parameter controls the red clearance
         * timing for a normal Red Clear signal terminated by a preempt initiated transition. A
         * preempt initiated transition shall not cause the termination of a Red Clear prior to its
         * display for the lesser of the phase’s Red Clear time or this period. CAUTION - if this
         * value is zero, phase Red Clear is terminated immediately.
         */
        private int preemptEnterRedClear;

        // NOT MAPPED
        /**
         * Track Clear Yellow Change time in tenth seconds (0-25.5 sec). The lesser of the phase’s
         * Yellow Change time or this parameter controls the yellow timing for the track clearance
         * movement. Track clear phase(s) are enabled in the preemptTrackPhase object.
         */
        private int preemptTrackYellowChange;

        // NOT MAPPED
        /**
         * Track Clear Red Clear time in tenth seconds (0-25.5 sec). The lesser of the phase’s Red
         * Clear time or this parameter controls the Red Clear timing for the track clearance
         * movement. Track clear phase(s) are enabled in the preemptTrackPhase object.
         */
        private int preemptTrackRedClear;

        public int getPreemptControl() {
            return preemptControl;
        }

        public void setPreemptControl(int preemptControl) {
            this.preemptControl = preemptControl;
        }

        public String getPreemptCyclingOverlap() {
            return preemptCyclingOverlap;
        }

        public void setPreemptCyclingOverlap(String preemptCyclingOverlap) {
            this.preemptCyclingOverlap = preemptCyclingOverlap;
        }

        public String getPreemptCyclingPed() {
            return preemptCyclingPed;
        }

        public void setPreemptCyclingPed(String preemptCyclingPed) {
            this.preemptCyclingPed = preemptCyclingPed;
        }

        public String getPreemptCyclingPhase() {
            return preemptCyclingPhase;
        }

        public void setPreemptCyclingPhase(String preemptCyclingPhase) {
            this.preemptCyclingPhase = preemptCyclingPhase;
        }

        public int getPreemptDelay() {
            return preemptDelay;
        }

        public void setPreemptDelay(int preemptDelay) {
            this.preemptDelay = preemptDelay;
        }

        public int getPreemptDwellGreen() {
            return preemptDwellGreen;
        }

        public void setPreemptDwellGreen(int preemptDwellGreen) {
            this.preemptDwellGreen = preemptDwellGreen;
        }

        public String getPreemptDwellOverlap() {
            return preemptDwellOverlap;
        }

        public void setPreemptDwellOverlap(String preemptDwellOverlap) {
            this.preemptDwellOverlap = preemptDwellOverlap;
        }

        public String getPreemptDwellPed() {
            return preemptDwellPed;
        }

        public void setPreemptDwellPed(String preemptDwellPed) {
            this.preemptDwellPed = preemptDwellPed;
        }

        public String getPreemptDwellPhase() {
            return preemptDwellPhase;
        }

        public void setPreemptDwellPhase(String preemptDwellPhase) {
            this.preemptDwellPhase = preemptDwellPhase;
        }

        public int getPreemptEnterPedClear() {
            return preemptEnterPedClear;
        }

        public void setPreemptEnterPedClear(int preemptEnterPedClear) {
            this.preemptEnterPedClear = preemptEnterPedClear;
        }

        public int getPreemptEnterRedClear() {
            return preemptEnterRedClear;
        }

        public void setPreemptEnterRedClear(int preemptEnterRedClear) {
            this.preemptEnterRedClear = preemptEnterRedClear;
        }

        public int getPreemptEnterYellowChange() {
            return preemptEnterYellowChange;
        }

        public void setPreemptEnterYellowChange(int preemptEnterYellowChange) {
            this.preemptEnterYellowChange = preemptEnterYellowChange;
        }

        public String getPreemptExitPhase() {
            return preemptExitPhase;
        }

        public void setPreemptExitPhase(String preemptExitPhase) {
            this.preemptExitPhase = preemptExitPhase;
        }

        public int getPreemptLink() {
            return preemptLink;
        }

        public void setPreemptLink(int preemptLink) {
            this.preemptLink = preemptLink;
        }

        public int getPreemptMaximumPresence() {
            return preemptMaximumPresence;
        }

        public void setPreemptMaximumPresence(int preemptMaximumPresence) {
            this.preemptMaximumPresence = preemptMaximumPresence;
        }

        public int getPreemptMinimumDuration() {
            return preemptMinimumDuration;
        }

        public void setPreemptMinimumDuration(int preemptMinimumDuration) {
            this.preemptMinimumDuration = preemptMinimumDuration;
        }

        public int getPreemptMinimumGreen() {
            return preemptMinimumGreen;
        }

        public void setPreemptMinimumGreen(int preemptMinimumGreen) {
            this.preemptMinimumGreen = preemptMinimumGreen;
        }

        public int getPreemptMinimumWalk() {
            return preemptMinimumWalk;
        }

        public void setPreemptMinimumWalk(int preemptMinimumWalk) {
            this.preemptMinimumWalk = preemptMinimumWalk;
        }

        public int getPreemptNumber() {
            return preemptNumber;
        }

        public void setPreemptNumber(int preemptNumber) {
            this.preemptNumber = preemptNumber;
        }

        public int getPreemptState() {
            return preemptState;
        }

        public void setPreemptState(int preemptState) {
            this.preemptState = preemptState;
        }

        public int getPreemptTrackGreen() {
            return preemptTrackGreen;
        }

        public void setPreemptTrackGreen(int preemptTrackGreen) {
            this.preemptTrackGreen = preemptTrackGreen;
        }

        public String getPreemptTrackOverlap() {
            return preemptTrackOverlap;
        }

        public void setPreemptTrackOverlap(String preemptTrackOverlap) {
            this.preemptTrackOverlap = preemptTrackOverlap;
        }

        public String getPreemptTrackPhase() {
            return preemptTrackPhase;
        }

        public void setPreemptTrackPhase(String preemptTrackPhase) {
            this.preemptTrackPhase = preemptTrackPhase;
        }

        public int getPreemptTrackRedClear() {
            return preemptTrackRedClear;
        }

        public void setPreemptTrackRedClear(int preemptTrackRedClear) {
            this.preemptTrackRedClear = preemptTrackRedClear;
        }

        public int getPreemptTrackYellowChange() {
            return preemptTrackYellowChange;
        }

        public void setPreemptTrackYellowChange(int preemptTrackYellowChange) {
            this.preemptTrackYellowChange = preemptTrackYellowChange;
        }
    }

    /**
     * This table contains the control objects that allow the preempts to be activated remotely.
     * There shall be one control object for each preempt input supported by the device. The number
     * of rows in this table shall be equal to maxPreempts.
     */
    public class PreemptControlTable {

        // NOT MAPPED

        /**
         * This object shall indicate the preempt input number controlled by the associated
         * preemptControlState object in this row.
         */
        private int preemptControlNumber;

        // NOT MAPPED
        /**
         * This object when set to ON (one) shall cause the associated preempt actions to occur
         * unless the actions have already been started by the physical preempt input. The preempt
         * shall remain active as long as this object is ON or the physical preempt input is ON.
         * This object when set to OFF (zero) shall cause the physical preempt input to control the
         * associated preempt actions. The device shall reset this object to ZERO when in BACKUP
         * Mode. A write to this object shall reset the Backup timer to ZERO (see unitBackupTime).
         */
        private int preemptControlState;

        public int getPreemptControlNumber() {
            return preemptControlNumber;
        }

        public void setPreemptControlNumber(int preemptControlNumber) {
            this.preemptControlNumber = preemptControlNumber;
        }

        public int getPreemptControlState() {
            return preemptControlState;
        }

        public void setPreemptControlState(int preemptControlState) {
            this.preemptControlState = preemptControlState;
        }
    }
}