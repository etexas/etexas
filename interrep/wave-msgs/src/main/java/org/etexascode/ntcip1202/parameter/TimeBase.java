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
 * This object is an identifier used to group all objects for support of timebase functions. If a
 * device implements timebase functions then these objects shall be supported
 * 
 * @author dranker
 */
public class TimeBase implements Serializable {

    // NOT MAPPED

    /**
     * Pattern Sync Reference in minutes past midnight. When the value is 65535, the controller unit
     * shall use the Action time as the Sync Reference for that pattern. Action time is the hour and
     * minute associated with the active dayPlanEventNumber (as defined in NTCIP 1201).
     */
    private int timebaseAscPatternSync;

    // NOT MAPPED
    /**
     * The Maximum Number of Actions this device supports. This object indicates the maximum rows
     * which shall appear in the timebaseAscActionTable object.
     */
    private int maxTimebaseAscActions;

    // NOT MAPPED
    /**
     * This object indicates the current time base Action Table row that will be used when the CU is
     * in Time Base operation. A value of zero indicates that no time base Action is selected.
     */
    private int timebaseAscActionStatus;

    private TimeBaseAscActionTable[] timeBaseAscActions;

    public TimeBase() {
        timeBaseAscActions = new TimeBaseAscActionTable[maxTimebaseAscActions];
    }

    public class TimeBaseAscActionTable {

        // NOT MAPPED

        /**
         * The time base Action number for objects in this row. This value shall not exceed the
         * maxTimebaseAscActions object value. This object may be defined as a dayPlanActionOID (as
         * defined in NTCIP 1201).
         */
        private int timebaseAscActionNumber;

        // NOT MAPPED
        /**
         * The Pattern that shall be active when this Action is active. The value shall not exceed
         * the value of maxPatterns, except for flash or free. A pattern of zero indicates that no
         * pattern is being selected. A pattern = 0 relinquishes control to entity of a lower
         * priority than timebase and allows that entity to control (i.e., interconnect if
         * available).
         */
        private int timebaseAscPattern;

        // NOT MAPPED
        /**
         * The Auxiliary functions that shall be active when this Action is active. Bit 7: Reserved
         * Bit 6: Reserved Bit 5: Reserved Bit 4: Reserved Bit 3: Dimming enabled if set (non-zero),
         * disabled if clear (zero). For dimming to occur, this control AND ('unitControl' OR a
         * dimming input) must be True. Bit 2: Auxiliary Function 3 enabled if set (non-zero),
         * disabled if clear (zero). Bit 1: Auxiliary Function 2 enabled if set (non-zero), disabled
         * if clear (zero). Bit 0: Auxiliary Function 1 enabled if set (non-zero), disabled if clear
         * (zero). A SET of a 'reserved' bit to a value other than zero (0) shall return a
         * badValue(3) error.
         */
        private int timebaseAscAuxillaryFunction;

        // NOT MAPPED
        /**
         * The Special Functions that shall be active when this Action is active. Bit 7: Special
         * Function 8 Bit 6: Special Function 7 Bit 5: Special Function 6 Bit 4: Special Function 5
         * Bit 3: Special Function 4 Bit 2: Special Function 3 Bit 1: Special Function 2 Bit 0:
         * Special Function 1 Bit = 0 - False/Disabled, Bit = 1 - True/Enabled
         */
        private int timebaseAscSpecialFunction;

        public int getTimebaseAscActionNumber() {
            return timebaseAscActionNumber;
        }

        public void setTimebaseAscActionNumber(int timebaseAscActionNumber) {
            this.timebaseAscActionNumber = timebaseAscActionNumber;
        }

        public int getTimebaseAscAuxillaryFunction() {
            return timebaseAscAuxillaryFunction;
        }

        public void setTimebaseAscAuxillaryFunction(int timebaseAscAuxillaryFunction) {
            this.timebaseAscAuxillaryFunction = timebaseAscAuxillaryFunction;
        }

        public int getTimebaseAscPattern() {
            return timebaseAscPattern;
        }

        public void setTimebaseAscPattern(int timebaseAscPattern) {
            this.timebaseAscPattern = timebaseAscPattern;
        }

        public int getTimebaseAscSpecialFunction() {
            return timebaseAscSpecialFunction;
        }

        public void setTimebaseAscSpecialFunction(int timebaseAscSpecialFunction) {
            this.timebaseAscSpecialFunction = timebaseAscSpecialFunction;
        }

    }

    public int getMaxTimebaseAscActions() {
        return maxTimebaseAscActions;
    }

    public void setMaxTimebaseAscActions(int maxTimebaseAscActions) {
        this.maxTimebaseAscActions = maxTimebaseAscActions;
    }

    public TimeBaseAscActionTable[] getTimeBaseAscActions() {
        return timeBaseAscActions.clone();
    }

    public void setTimeBaseAscActions(TimeBaseAscActionTable[] timeBaseAscActions) {
        this.timeBaseAscActions = timeBaseAscActions.clone();
    }

    public int getTimebaseAscActionStatus() {
        return timebaseAscActionStatus;
    }

    public void setTimebaseAscActionStatus(int timebaseAscActionStatus) {
        this.timebaseAscActionStatus = timebaseAscActionStatus;
    }

    public int getTimebaseAscPatternSync() {
        return timebaseAscPatternSync;
    }

    public void setTimebaseAscPatternSync(int timebaseAscPatternSync) {
        this.timebaseAscPatternSync = timebaseAscPatternSync;
    }

}
