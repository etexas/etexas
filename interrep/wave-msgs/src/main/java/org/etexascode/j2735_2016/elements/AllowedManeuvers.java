/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2018 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.j2735_2016.elements;

import java.util.Objects;

import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The allowed maneuvers element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class AllowedManeuvers implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover an allowed maneuvers.
     */
    public static final int NUM_BITS = 12;

    /**
     * The maneuver straight allowed status.
     */
    private boolean maneuverStraightAllowed = false;

    /**
     * The maneuver left allowed status.
     */
    private boolean maneuverLeftAllowed = false;

    /**
     * The maneuver right allowed status.
     */
    private boolean maneuverRightAllowed = false;

    /**
     * The maneuver u-turn allowed status.
     */
    private boolean maneuverUTurnAllowed = false;

    /**
     * The maneuver left turn on red allowed status.
     */
    private boolean maneuverLeftTurnOnRedAllowed = false;

    /**
     * The maneuver right turn on red allowed status.
     */
    private boolean maneuverRightTurnOnRedAllowed = false;

    /**
     * The maneuver lane change allowed status.
     */
    private boolean maneuverLaneChangeAllowed = false;

    /**
     * The maneuver no stopping allowed status.
     */
    private boolean maneuverNoStoppingAllowed = false;

    /**
     * The yield allways required status.
     */
    private boolean yieldAllwaysRequired = false;

    /**
     * The go with halt status.
     */
    private boolean goWithHalt = false;

    /**
     * The caution status.
     */
    private boolean caution = false;

    /**
     * The reserved 1 status.
     */
    private boolean reserved1 = false;

    /**
     * A getter for the maneuver straight allowed status.
     * 
     * @return The maneuver straight allowed status.
     */
    public boolean isManeuverStraightAllowed() {

        return maneuverStraightAllowed;
    }

    /**
     * A setter for the maneuver straight allowed status.
     * 
     * @param maneuverStraightAllowed The maneuver straight allowed status to set.
     */
    public void setManeuverStraightAllowed(boolean maneuverStraightAllowed) {

        this.maneuverStraightAllowed = maneuverStraightAllowed;
    }

    /**
     * A getter for the maneuver left allowed status.
     * 
     * @return The maneuver left allowed status.
     */
    public boolean isManeuverLeftAllowed() {

        return maneuverLeftAllowed;
    }

    /**
     * A setter for the maneuver left allowed status.
     * 
     * @param maneuverLeftAllowed The maneuver left allowed status to set.
     */
    public void setManeuverLeftAllowed(boolean maneuverLeftAllowed) {

        this.maneuverLeftAllowed = maneuverLeftAllowed;
    }

    /**
     * A getter for the maneuver right allowed status.
     * 
     * @return The maneuver right allowed status.
     */
    public boolean isManeuverRightAllowed() {

        return maneuverRightAllowed;
    }

    /**
     * A setter for the maneuver right allowed status.
     * 
     * @param maneuverRightAllowed The maneuver right allowed status to set.
     */
    public void setManeuverRightAllowed(boolean maneuverRightAllowed) {

        this.maneuverRightAllowed = maneuverRightAllowed;
    }

    /**
     * A getter for the maneuver u-turn allowed status.
     * 
     * @return The maneuver u-turn allowed status.
     */
    public boolean isManeuverUTurnAllowed() {

        return maneuverUTurnAllowed;
    }

    /**
     * A setter for the maneuver u-turn allowed status.
     * 
     * @param maneuverUTurnAllowed The maneuver u-turn allowed status to set.
     */
    public void setManeuverUTurnAllowed(boolean maneuverUTurnAllowed) {

        this.maneuverUTurnAllowed = maneuverUTurnAllowed;
    }

    /**
     * A getter for the maneuver left turn on red allowed status.
     * 
     * @return The maneuver left turn on red allowed status.
     */
    public boolean isManeuverLeftTurnOnRedAllowed() {

        return maneuverLeftTurnOnRedAllowed;
    }

    /**
     * A setter for the maneuver left turn on red allowed status.
     * 
     * @param maneuverLeftTurnOnRedAllowed The maneuver left turn on red allowed status to set.
     */
    public void setManeuverLeftTurnOnRedAllowed(boolean maneuverLeftTurnOnRedAllowed) {

        this.maneuverLeftTurnOnRedAllowed = maneuverLeftTurnOnRedAllowed;
    }

    /**
     * A getter for the maneuver right turn on red allowed status.
     * 
     * @return The maneuver right turn on red allowed status.
     */
    public boolean isManeuverRightTurnOnRedAllowed() {

        return maneuverRightTurnOnRedAllowed;
    }

    /**
     * A setter for the maneuver right turn on red allowed status.
     * 
     * @param maneuverRightTurnOnRedAllowed The maneuver right turn on red allowed status to set.
     */
    public void setManeuverRightTurnOnRedAllowed(boolean maneuverRightTurnOnRedAllowed) {

        this.maneuverRightTurnOnRedAllowed = maneuverRightTurnOnRedAllowed;
    }

    /**
     * A getter for the maneuver lane change allowed status.
     * 
     * @return The maneuver lane change allowed status.
     */
    public boolean isManeuverLaneChangeAllowed() {

        return maneuverLaneChangeAllowed;
    }

    /**
     * A setter for the maneuver lane change allowed status.
     * 
     * @param maneuverLaneChangeAllowed The maneuver lane change allowed status to set.
     */
    public void setManeuverLaneChangeAllowed(boolean maneuverLaneChangeAllowed) {

        this.maneuverLaneChangeAllowed = maneuverLaneChangeAllowed;
    }

    /**
     * A getter for the maneuver no stopping allowed status.
     * 
     * @return The maneuver no stopping allowed status.
     */
    public boolean isManeuverNoStoppingAllowed() {

        return maneuverNoStoppingAllowed;
    }

    /**
     * A setter for the maneuver no stopping allowed status.
     * 
     * @param maneuverNoStoppingAllowed The maneuver no stopping allowed status to set.
     */
    public void setManeuverNoStoppingAllowed(boolean maneuverNoStoppingAllowed) {

        this.maneuverNoStoppingAllowed = maneuverNoStoppingAllowed;
    }

    /**
     * A getter for the yield allways required status.
     * 
     * @return The yield allways required status.
     */
    public boolean isYieldAllwaysRequired() {

        return yieldAllwaysRequired;
    }

    /**
     * A setter for the yield allways required status.
     * 
     * @param yieldAllwaysRequired The yield allways required status to set.
     */
    public void setYieldAllwaysRequired(boolean yieldAllwaysRequired) {

        this.yieldAllwaysRequired = yieldAllwaysRequired;
    }

    /**
     * A getter for the go with halt status.
     * 
     * @return The go with halt status.
     */
    public boolean isGoWithHalt() {

        return goWithHalt;
    }

    /**
     * A setter for the go with halt status.
     * 
     * @param goWithHalt The go with halt status to set.
     */
    public void setGoWithHalt(boolean goWithHalt) {

        this.goWithHalt = goWithHalt;
    }

    /**
     * A getter for the caution status.
     * 
     * @return The caution status.
     */
    public boolean isCaution() {

        return caution;
    }

    /**
     * A setter for the caution status.
     * 
     * @param caution The caution status to set.
     */
    public void setCaution(boolean caution) {

        this.caution = caution;
    }

    /**
     * A getter for the reserved 1 status.
     * 
     * @return The reserved 1 status.
     */
    public boolean isReserved1() {

        return reserved1;
    }

    /**
     * A setter for the reserved 1 status.
     * 
     * @param reserved1 The reserved 1 status to set.
     */
    public void setReserved1(boolean reserved1) {

        this.reserved1 = reserved1;
    }

    @Override
    public String encodeUPER() {

        StringBuilder allowedManeuvers = new StringBuilder(12);
        allowedManeuvers.append(maneuverStraightAllowed ? '1' : '0');
        allowedManeuvers.append(maneuverLeftAllowed ? '1' : '0');
        allowedManeuvers.append(maneuverRightAllowed ? '1' : '0');
        allowedManeuvers.append(maneuverUTurnAllowed ? '1' : '0');
        allowedManeuvers.append(maneuverLeftTurnOnRedAllowed ? '1' : '0');
        allowedManeuvers.append(maneuverRightTurnOnRedAllowed ? '1' : '0');
        allowedManeuvers.append(maneuverLaneChangeAllowed ? '1' : '0');
        allowedManeuvers.append(maneuverNoStoppingAllowed ? '1' : '0');
        allowedManeuvers.append(yieldAllwaysRequired ? '1' : '0');
        allowedManeuvers.append(goWithHalt ? '1' : '0');
        allowedManeuvers.append(caution ? '1' : '0');
        allowedManeuvers.append(reserved1 ? '1' : '0');

        return allowedManeuvers.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (AllowedManeuvers.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an AllowedManeuvers element (%d)", AllowedManeuvers.NUM_BITS));
        }

        String allowedManeuversBits = bits.substring(0, AllowedManeuvers.NUM_BITS);

        maneuverStraightAllowed = allowedManeuversBits.charAt(0) == '1';
        maneuverLeftAllowed = allowedManeuversBits.charAt(1) == '1';
        maneuverRightAllowed = allowedManeuversBits.charAt(2) == '1';
        maneuverUTurnAllowed = allowedManeuversBits.charAt(3) == '1';
        maneuverLeftTurnOnRedAllowed = allowedManeuversBits.charAt(4) == '1';
        maneuverRightTurnOnRedAllowed = allowedManeuversBits.charAt(5) == '1';
        maneuverLaneChangeAllowed = allowedManeuversBits.charAt(6) == '1';
        maneuverNoStoppingAllowed = allowedManeuversBits.charAt(7) == '1';
        yieldAllwaysRequired = allowedManeuversBits.charAt(8) == '1';
        goWithHalt = allowedManeuversBits.charAt(9) == '1';
        caution = allowedManeuversBits.charAt(10) == '1';
        reserved1 = allowedManeuversBits.charAt(11) == '1';

        return bits.substring(AllowedManeuvers.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hash(maneuverStraightAllowed, maneuverLeftAllowed, maneuverRightAllowed, maneuverUTurnAllowed, maneuverLeftTurnOnRedAllowed, maneuverRightTurnOnRedAllowed,
                maneuverLaneChangeAllowed, maneuverNoStoppingAllowed, yieldAllwaysRequired, goWithHalt, caution, reserved1);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof AllowedManeuvers)) {

            return false;
        }
        AllowedManeuvers element = (AllowedManeuvers)object;
        return this.maneuverStraightAllowed == element.maneuverStraightAllowed
                && this.maneuverLeftAllowed == element.maneuverLeftAllowed
                && this.maneuverRightAllowed == element.maneuverRightAllowed
                && this.maneuverUTurnAllowed == element.maneuverUTurnAllowed
                && this.maneuverLeftTurnOnRedAllowed == element.maneuverLeftTurnOnRedAllowed
                && this.maneuverRightTurnOnRedAllowed == element.maneuverRightTurnOnRedAllowed
                && this.maneuverLaneChangeAllowed == element.maneuverLaneChangeAllowed
                && this.maneuverNoStoppingAllowed == element.maneuverNoStoppingAllowed
                && this.yieldAllwaysRequired == element.yieldAllwaysRequired
                && this.goWithHalt == element.goWithHalt
                && this.caution == element.caution
                && this.reserved1 == element.reserved1;
    }
}
