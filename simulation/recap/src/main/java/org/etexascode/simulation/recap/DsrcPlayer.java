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
package org.etexascode.simulation.recap;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Defines a player to scan through DSRC messages from one or more DSRC units.
 * 
 * @author emyers
 */
public class DsrcPlayer {

    /**
     * The list of DSRC units.
     */
    private List<DsrcUnit> units;

    /**
     * The map of current DSRC messages.
     */
    private Map<String, DsrcMessage> messageMap;

    /**
     * The DSRC message listeners.
     */
    private List<DsrcListener> listeners;

    /**
     * Creates a new <code>DsrcPlayer</code> for the list of DSRC units.
     * 
     * @param units the DSRC units to play
     * @throws IllegalArgumentException if the list of DSRC units is empty
     * @throws NullPointerException if a <code>null</code> value is given for the list of DSRC units
     */
    public DsrcPlayer(List<DsrcUnit> units) {

        // if no DSRC units are given
        if (units == null) {

            // throw a NullPointerException to indicate the error
            throw new NullPointerException("no DSRC units exist");
        }
        // if an empty list of DSRC units is given
        else if (units.isEmpty()) {

            // throw an IllegaArgumentException to indicate the error
            throw new IllegalArgumentException(
                    "at least one DSRC unit must be provided");
        }

        // initialize the DSRC units
        this.units = units;
        DsrcListener listener = new UnitListener();
        for (DsrcUnit unit : units) {
            unit.addDsrcListener(listener);
        }

        // create the DSRC listeners and the map of current messages
        listeners = new ArrayList<DsrcListener>();
        messageMap = new HashMap<String, DsrcMessage>();
    }

    /**
     * Adds the DSRC listener to those registered for DSRC message notifications. DSRC listeners
     * cannot be added more than once.
     * 
     * @param listener the DSRC listener to register
     * @throws NullPointerException if a <code>null</code> value is given for the DSRC listener
     */
    public void addDsrcListener(DsrcListener listener) {

        // if no listener is given
        if (listener == null) {

            // throw a NullPointerException to indicate the error
            throw new NullPointerException("a valid listener must be provided");
        }

        // if the listener is not already registered
        if (!listeners.contains(listener)) {

            // add the listener for DSRc message notifications
            listeners.add(listener);
        }
    }

    /**
     * Removes the DSRC listener from those registered for DSRC message notifications.
     * 
     * @param listener the DSRC listener to remove
     */
    public void removeDsrcListener(DsrcListener listener) {

        listeners.remove(listener);
    }

    /**
     * Forwards the given DSRC message to all registered listeners.
     * 
     * @param message the DSRC message to forward
     */
    private void fireMessageEvent(DsrcMessage message) {

        // forward the message to all registered listeners
        for (DsrcListener listener : listeners) {
            listener.messageReceived(message);
        }
    }

    /**
     * Fires the next DSRC message in the source PDML file. If the end of the file has been reached,
     * messages will resume from the beginning of the file on the next invocation. To prevent
     * repetition, check the playing status with <code>isPlaying()</code> prior to invoking this
     * method.
     */
    public void play() {

        // if playing has stopped
        if (!isPlaying()) {

            // resume playing for each unit
            for (DsrcUnit unit : units) {
                unit.play();
            }
        }
        // if playing is in progress
        else {

            // play the next DSRC message
            playMessage();
        }
    }

    /**
     * Fires the next DSRC message. The next message is the earliest among the DSRC units with
     * messages remaining.
     */
    private void playMessage() {

        DsrcMessage message;
        DsrcMessage selectedMessage = null;

        // for each DSRC unit
        for (DsrcUnit unit : units) {

            // get the loaded message
            message = messageMap.get(unit.getName());

            // if play is in progress but no message is loaded
            if (message == null && unit.isPlaying()) {

                // load the next message for the unit
                unit.play();
                message = messageMap.get(unit.getName());
            }

            // if the message is the earliest message
            if (message != null) {
                if (selectedMessage == null || message.getTime() < selectedMessage.getTime()) {

                    // set the selected message
                    selectedMessage = message;
                }
            }
        }

        // if no message was selected
        if (selectedMessage != null) {

            // forward the selected message
            messageMap.put(selectedMessage.getUnitName(), null);
            fireMessageEvent(selectedMessage);
        }
    }

    /**
     * Returns whether the player is still playing DSRC messages.
     * 
     * @return <code>true</code> if any of the DSRC units are still playing, otherwise a value of
     *         <code>false</code> is returned
     */
    public boolean isPlaying() {

        // return true if any unit is playing
        for (DsrcUnit unit : units) {
            if (unit.isPlaying()) {
                return true;
            }
        }

        return false; // return that no unit is playing
    }

    /**
     * Defines the method to receive DSRC messages from each unit.
     */
    private class UnitListener implements DsrcListener {

        @Override
        public void messageReceived(DsrcMessage message) {

            // if a message still exists for the source unit
            if (messageMap.get(message.getUnitName()) != null) {

                // throw a new IllegalStateException to indicate the error
                throw new IllegalStateException("unit should not have been played");
            }

            // update the current message for the source unit
            messageMap.put(message.getUnitName(), message);
        }
    }
}