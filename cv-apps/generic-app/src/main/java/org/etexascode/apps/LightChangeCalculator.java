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
package org.etexascode.apps;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A calculator for determining when lights are changing.
 * 
 * @author ablatt
 * @author janway
 */
public class LightChangeCalculator {

    /**
     * Serial ID
     */
    private static final long serialVersionUID = 1390798418547493027L;

    /**
     * enum for keeping track of light colors.
     * 
     * @author ablatt
     */
    public enum Color {
        GREEN,
        RED,
        YELLOW
    };

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(LightChangeCalculator.class);

    /**
     * Key: Lane Id Value: Current Light state for that lane
     */
    Map<Integer, LightState> lightsByLane = new HashMap<Integer, LightState>();

    /**
     * Key: Lane Id Value: Light state on the previous frame for that lane
     */
    Map<Integer, LightState> lightOnPrev = null;

    /**
     * The sim time of the last step.
     */
    double prevSimTime;

    /**
     * Constructor.
     * 
     * @param laneIds Set of lane ids to watch.
     */
    public LightChangeCalculator(Set<Integer> laneIds) {
        for (Integer i : laneIds) {
            lightsByLane.put(i, null);
        }
    }

    /**
     * Updates the current signal state for each lane. Returns a map of all the "changes" which
     * occurred since the last time update was called. Assuming that the light folding algorithm
     * hasn't been changed from its default, the following is a list of the valid keys: GREEN-GREEN
     * GREEN-YELLOW GREEN-RED YELLOW-GREEN YELLOW-YELLOW YELLOW-RED RED-GREEN RED-YELLOW RED-RED You
     * can ensure you get the proper valid key using ...
     * 
     * @param newSigMan The new signal manager.
     * @param simTime The simulation time.
     * @return The map.
     */
    public Map<String, Set<Integer>> update(ISignalManager newSigMan, Double simTime) {
        Map<String, Set<Integer>> ret = genUpdateMap();

        double timeElapsed = simTime - prevSimTime;

        lightOnPrev = cloneLightMap(lightsByLane);

        for (Entry<Integer, LightState> lightEntry : lightsByLane.entrySet()) {

            Integer i = lightEntry.getKey();
            LightState light = lightEntry.getValue();

            // calculate the new light state
            LightState lightFold = foldLightState(newSigMan.getSignalsByLaneId(i));

            if (lightFold == null) {

                if (light == null) {
                    continue;
                }

                if (light.timeToChange != null) {
                    light.timeToChange -= timeElapsed;

                    if (light.timeToChange <= 0.0) {
                        light.advance();
                    }
                } // else {} // do nothing

                lightFold = light;

            }
            else {
                lightsByLane.put(i, lightFold);
            }

            LightState prevLight = lightOnPrev.get(i);

            if (prevLight != null) {
                ret.get(getLightChangeString(prevLight.color, lightFold.color)).add(i);
            } // else {} // do nothing
        }

        return ret;
    }

    /**
     * Returns a map of times to change keyed by lane ID.
     * 
     * @return The map.
     */
    public Map<Integer, Double> getTimeToChangeByLane() {
        Map<Integer, Double> ret = new HashMap<Integer, Double>();
        for (Entry<Integer, LightState> entry : lightsByLane.entrySet()) {
            if (entry.getValue() == null) {
                continue;
            }
            Double ttc = entry.getValue().timeToChange;
            if (ttc == null) {
                ttc = -1.0;
            }
            else {
                ttc = new Double(ttc.doubleValue());
            }
            ret.put(entry.getKey(), ttc);
        }

        return ret;
    }

    /**
     * Returns a set of lane IDs for signals of the given color.
     * 
     * @return The set of lane IDs.
     */
    public Map<LightChangeCalculator.Color, Set<Integer>> getLanesBySignalColor() {
        Map<LightChangeCalculator.Color, Set<Integer>> ret = new HashMap<LightChangeCalculator.Color, Set<Integer>>();

        for (Entry<Integer, LightState> entry : lightsByLane.entrySet()) {
            if (entry.getValue() == null) {
                continue;
            }
            if (!ret.containsKey(entry.getValue().color)) {
                ret.put(entry.getValue().color, new HashSet<Integer>());
            }
            ret.get(entry.getValue().color).add(entry.getKey());
        }

        return ret;
    }

    /**
     * Creates a SignalManagerInfo that represents the current state of the lights.
     * 
     * @return The SignalManagerInfo.
     */
    public ISignalManager getSignalManager() {
        SignalManager sm = new SignalManager();
        for (Entry<Integer, LightState> entry : lightsByLane.entrySet()) {
            if (entry.getValue() == null) {
                continue;
            }

            SignalIndication si = new SignalIndication();
            SignalIndication.Color siColor = SignalIndication.Color.GREEN;
            LightChangeCalculator.Color entryColor = entry.getValue().color;

            if (entryColor == Color.GREEN) {
                siColor = SignalIndication.Color.GREEN;
            }
            else if (entryColor == Color.YELLOW) {
                siColor = SignalIndication.Color.YELLOW;
            }
            else {
                siColor = SignalIndication.Color.RED;
            }

            si.setColorIndication(siColor);
            si.setLaneId(entry.getKey());
            si.setStateIndication(SignalIndication.State.STEADY);
            si.setTypeIndication(SignalIndication.Type.BALL);

            if (entry.getValue().timeToChange == null) {
                si.setTimeToChange(-1.0);
            }
            else {
                si.setTimeToChange(entry.getValue().timeToChange);
            }
            sm.addSignal(si);
        }

        return sm;
    }

    /**
     * Uniform means of generating a map to return to the developer who called update.
     * 
     * @return The map.
     */
    private Map<String, Set<Integer>> genUpdateMap() {
        Map<String, Set<Integer>> ret = new HashMap<String, Set<Integer>>();

        ret.put("GREEN-GREEN", new HashSet<Integer>());
        ret.put("GREEN-YELLOW", new HashSet<Integer>());
        ret.put("GREEN-RED", new HashSet<Integer>());
        ret.put("YELLOW-GREEN", new HashSet<Integer>());
        ret.put("YELLOW-YELLOW", new HashSet<Integer>());
        ret.put("YELLOW-RED", new HashSet<Integer>());
        ret.put("RED-GREEN", new HashSet<Integer>());
        ret.put("RED-YELLOW", new HashSet<Integer>());
        ret.put("RED-RED", new HashSet<Integer>());

        return ret;
    }

    /**
     * A function for converting a pair of light colors to a key in the map returned by update.
     * 
     * @param before Color the light is changing from.
     * @param after Color the light is changing to.
     * @return The string result.
     */
    public static String getLightChangeString(Color before, Color after) {
        StringBuilder sb = new StringBuilder(before.name());
        sb.append("-");
        sb.append(after.name());
        return sb.toString();
    }

    /**
     * Clones a light state map
     * 
     * @param states
     * @return The map.
     */
    private Map<Integer, LightState> cloneLightMap(Map<Integer, LightState> states) {
        Map<Integer, LightState> ret = new HashMap<Integer, LightState>();

        for (Entry<Integer, LightState> entry : states.entrySet()) {
            Integer i = entry.getKey();
            LightState ls = entry.getValue();
            if (ls == null) {
                ret.put(i, null);
            }
            else {
                ret.put(i, ls.clone());
            }
        }

        return ret;
    }

    /**
     * Perform a fold left on signal indications to convert them into light states. Fold goal is the
     * signal state closest to green with the time to change furthest in the future. GREEN less than
     * YELLOW less than RED
     * 
     * @param laneSignals The signal indications to fold.
     * @return The light state generated from folding signals indications together.
     */
    public LightState foldLightState(List<? extends ISignalIndication> laneSignals) {
        if (laneSignals == null || laneSignals.isEmpty()) {
            return null;
        }

        LightState ret = new LightState();

        for (ISignalIndication si : laneSignals) {
            ret = foldLightState(si, ret);
        }

        /*
         * if(ret.timeToChange != null && ret.timeToChange == 0.0) { ret.timeToChange = null; }
         */

        return ret;
    }

    /**
     * Perform a fold left on signal indications to convert them into light states. Fold goal is the
     * signal state closest to green with the time to change furthest in the future. GREEN < YELLOW
     * < RED
     * 
     * @param si Signal Indication to fold into the light state.
     * @param curr Current Light state to fold.
     * @return The new light state generated from
     */
    private LightState foldLightState(ISignalIndication si, LightState curr) {
        SignalIndication.Color c = si.getColorIndication();

        if (c == SignalIndication.Color.GREEN) {
            curr = foldLightState(Color.GREEN, si.getTimeToChange(), curr);
        }
        else if (c == SignalIndication.Color.YELLOW) {
            curr = foldLightState(Color.YELLOW, si.getTimeToChange(), curr);
        }
        else if (c == SignalIndication.Color.RED) {
            curr = foldLightState(Color.RED, si.getTimeToChange(), curr);
        }
        else {} // c == SignalIndication.Color.NONE

        return curr;
    }

    /**
     * Perform a fold left on signal indications to convert them into light states. Fold goal is the
     * signal state closest to green with the time to change furthest in the future. GREEN < YELLOW
     * < RED
     * 
     * @param c The color of the light state generated from the signal indication.
     * @param timeToChange The time to change of the light state generated from the signal
     *        indication.
     * @param curr The current light state to fold into.
     * @return The new light state generated from the fold.
     */
    private LightState foldLightState(Color c, double timeToChange, LightState curr) {
        int colorCmp = curr.compareColor(c);

        if (colorCmp == 1) {
            return curr;
        }
        else if (colorCmp == -1) {
            curr = new LightState();

            curr.color = c;
            curr.timeToChange = timeToChange;

            return curr;
        }
        else { // colorCmp == 0
            if ((curr.timeToChange == null) || (curr.timeToChange < timeToChange)) {
                curr = new LightState();

                curr.color = c;
                curr.timeToChange = timeToChange;

                return curr;
            }
            else {
                return curr;
            }
        }
    }

    /**
     * A class designed to hold the current light state of a single lane.
     * 
     * @author ablatt
     */
    static public class LightState implements Cloneable {

        public Color color = Color.RED;

        public Double timeToChange = null;

        /**
         * Getter {@link #color}
         * 
         * @return The color.
         */
        public Color getColor() {
            return color;
        }

        /**
         * Getter{@link #timeToChange}
         * 
         * @return The time to change.
         */
        public Double getTimeToChange() {
            return timeToChange;
        }

        /**
         * Move this light state to its next logical state. GREEN to YELLOW to RED to GREEN Time to
         * change is set to null.
         */
        public void advance() {
            timeToChange = null;

            if (color == Color.GREEN) {
                color = Color.YELLOW;
            }
            else if (color == Color.RED) {
                color = Color.GREEN;
            }
            else { // color == Color.YELLOW
                color = Color.RED;
            }
        }

        /**
         * Clone this LightState
         */
        @CoberturaIgnore
        @Override
        public LightState clone() {
            try {
                return (LightState)super.clone();
            }
            catch (CloneNotSupportedException ex) {
                throw new AssertionError();
            }
        }

        /**
         * Comparator for the Color enum. NOTE: Assumes that RED, YELLOW, and GREEN are the only
         * Color enums.
         * 
         * @param c The color.
         * @return The result of the color comparison.
         */
        public int compareColor(Color c) {
            if (c == color) {
                return 0;
            }
            else if (c == Color.RED) {
                return 1;
            }
            else if (c == Color.GREEN) {
                return -1;
            }
            else if (color == Color.RED) {
                return -1;
            }
            else {
                return 1;
            }
        }

        @Override
        public boolean equals(Object o) {
            if (o instanceof LightState) {
                LightState ls = (LightState)o;

                if (!(color == ls.color)) {
                    return false;
                }

                if ((timeToChange != null) && (ls.timeToChange != null)) {
                    return closeEnough(timeToChange, ls.timeToChange);
                }
                else if ((timeToChange == null) && (ls.timeToChange == null)) {
                    return true;
                }
                else {
                    return false;
                }

            }
            else {
                return false;
            }
        }

        /**
         * HashCode operation.
         * 
         * @return A hashcode of the data.
         */
        @Override
        @CoberturaIgnore
        public int hashCode() {
            return new HashCodeBuilder(41, 29).append(color).toHashCode();
        }

        /**
         * Checks if d1 is close enough to d2 for our purposes.
         * 
         * @param d1
         * @param d2
         * @return Are the doubles close.
         */
        @CoberturaIgnore
        private boolean closeEnough(double d1, double d2) {
            return ((d1 - d2) <= 0.05) && ((d2 - d1) <= 0.05);
        }
    }
}